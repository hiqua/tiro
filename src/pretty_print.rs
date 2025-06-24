use std::fs::{create_dir_all, File};
use std::io;
use std::io::Result;
use std::io::Write;
use std::iter::{Extend, Iterator};
use std::path::{Path, PathBuf};

use chrono::Datelike;
use colored::control::{set_override, unset_override};
use colored::Color;
use colored::*;
use chrono::Duration as ChronoDuration; // Added for gap calculation

use crate::config::Quadrant;
use crate::config::Quadrant::*;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use crate::summary::Timestamp;

pub fn get_output_writer(
    path: Option<&str>,
    prefix: &str,
    filetime: &str,
    start_time: Timestamp,
) -> (Box<dyn Write>, bool) {
    let (out_writer, color) = match path {
        Some("-") => (Box::new(io::stdout()) as Box<dyn Write>, true),
        Some(x) => {
            let path = Path::new(x);
            if path.is_dir() {
                let file_path: PathBuf = [
                    x,
                    &format!(
                        "{:4}-w{:02}",
                        start_time.year(),
                        start_time.iso_week().week()
                    ),
                    &format!("{}", start_time.date().naive_local()),
                    &format!("{}_{}.txt", prefix, filetime),
                ]
                .iter()
                .collect();
                if let Some(parent) = file_path.parent() {
                    create_dir_all(parent)
                        .expect("Could not create some parents of the output files.");
                }
                (
                    Box::new(File::create(&file_path).unwrap()) as Box<dyn Write>,
                    false,
                )
            } else {
                (
                    Box::new(File::create(path).unwrap()) as Box<dyn Write>,
                    false,
                )
            }
        }
        None => (Box::new(io::stdout()) as Box<dyn Write>, true),
    };

    (out_writer, color)
}

pub fn format_lifelapses(lifelapses: &[LifeLapse]) -> Vec<String> {
    let mut lines = vec![];
    let mut previous_end_time: Option<Timestamp> = None;

    for ll in lifelapses {
        if let Some(prev_end) = previous_end_time {
            let gap = ll.start().signed_duration_since(prev_end);
            if gap > ChronoDuration::zero() {
                lines.push(format_gap_duration(gap));
            }
        }
        lines.extend(format_list_of_chunks(ll.start(), ll.tokens_as_ref()));
        previous_end_time = Some(ll.end());
    }
    lines
}

fn format_gap_duration(duration: ChronoDuration) -> String {
    let hours = duration.num_hours();
    let minutes = duration.num_minutes() % 60;

    let mut parts = Vec::new();
    if hours > 0 {
        parts.push(format!("{} hour{}", hours, if hours == 1 { "" } else { "s" }));
    }
    if minutes > 0 {
        parts.push(format!("{} minute{}", minutes, if minutes == 1 { "" } else { "s" }));
    }

    if parts.is_empty() {
        // This case should ideally not be reached if we only call for gaps > 0
        return String::from("Minimal gap");
    }

    format!("--- Gap of {} ---", parts.join(" ")).italic().to_string()
}

/// Need this producer because the coloring won't be flexible otherwise
pub fn write_to(
    output_producer: impl Fn() -> Vec<String>,
    writer: &mut Box<dyn Write>,
    color: bool,
) -> Result<()> {
    if !color {
        set_override(false);
    }
    let output = output_producer();
    writeln!(writer, "{}", output.join("\n"))?;
    unset_override();
    Ok(())
}

fn format_list_of_chunks(start_time: Timestamp, list_of_lc: &[TimedLifeChunk]) -> Vec<String> {
    let mut desc = vec![];
    // XXX: show date as well
    let format_date = |d: Timestamp| {
        let format = "%H:%M %Y-%m-%d";
        let s = format!("-- {}", d.format(format));
        s.bold().to_string()
    };
    desc.push(format_date(start_time));
    let mut curr_time = start_time;
    for c in list_of_lc {
        if c.start < curr_time {
            desc.push(get_warning_overlapping());
        }
        curr_time = c.start;
        desc.push(format_life_chunk(&c.life_chunk, c.start));
    }
    desc.push("".to_string());
    desc
}

/// Split string into strings of the given size.
fn format_line_in_lines(s: &str, size: usize) -> Vec<String> {
    let mut lines: Vec<Vec<&str>> = vec![vec![]];
    let mut cur_len = 0;
    for w in s.split_whitespace() {
        if cur_len == 0 || cur_len + w.chars().count() < size {
            lines.last_mut().unwrap().push(w);
            cur_len += w.chars().count();
        } else {
            lines.push(vec![w]);
            cur_len = 0;
        }
    }
    lines.iter().map(|lv| lv.join(" ")).collect()
}

fn format_life_chunk(chunk: &LifeChunk, start_time: Timestamp) -> String {
    let screen_size = 70;
    let end_time = start_time + chunk.duration;

    let mut lines_to_print = vec![];
    {
        let string_lines: Vec<String> = format_line_in_lines(chunk.get_input(), screen_size);

        let mut it = string_lines.iter();
        if let Some(first_line) = it.next() {
            lines_to_print.push(color_line(
                format!("-> {} {}", end_time.format("%Hh%M"), first_line),
                chunk.quadrant,
            ));
        }
        it.for_each(|s| lines_to_print.push(color_line(format!("         {}", s), chunk.quadrant)));
    }

    lines_to_print.join("\n")
}

/// XXX: should be configurable
fn color_line(s: String, q: Quadrant) -> String {
    let c = match q {
        Q1 => Color::Red,
        Q2 => Color::Yellow,
        Q3 => Color::Cyan,
        Q4 => Color::Blue,
        Q5 => Color::Green,
        Q6 => Color::White,
    };
    format!("{}", s.color(c))
}

fn get_warning_overlapping() -> String {
    "\n/!\\ Overlapping activities /!\\\n".to_string()
}
