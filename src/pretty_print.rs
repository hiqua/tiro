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
use time; // Added for tests
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{LifeLapse, LifeChunk, TimedLifeChunk, get_life_chunk}; // Added LifeChunk, TimedLifeChunk, get_life_chunk
    use crate::config::Quadrant; // Added for minimal_life_chunk
    use chrono::{Local, TimeZone, Duration as ChronoDuration};
    use time; // Already added, but ensure it's here for LifeChunk's duration

    #[test]
    fn test_format_gap_duration_hours_and_minutes() {
        let duration = ChronoDuration::hours(2) + ChronoDuration::minutes(30);
        // The .italic().to_string() adds ANSI codes if not overridden
        set_override(false); // Ensure no ANSI codes for string comparison
        assert_eq!(format_gap_duration(duration), "--- Gap of 2 hours 30 minutes ---");
        unset_override();
    }

    #[test]
    fn test_format_gap_duration_only_hours() {
        let duration = ChronoDuration::hours(5);
        set_override(false);
        assert_eq!(format_gap_duration(duration), "--- Gap of 5 hours ---");
        unset_override();
    }

    #[test]
    fn test_format_gap_duration_only_minutes() {
        let duration = ChronoDuration::minutes(45);
        set_override(false);
        assert_eq!(format_gap_duration(duration), "--- Gap of 45 minutes ---");
        unset_override();
    }

    #[test]
    fn test_format_gap_duration_one_hour_one_minute() {
        let duration = ChronoDuration::hours(1) + ChronoDuration::minutes(1);
        set_override(false);
        assert_eq!(format_gap_duration(duration), "--- Gap of 1 hour 1 minute ---");
        unset_override();
    }

    // Tests for format_lifelapses
    // For these tests, we need to construct LifeLapse instances.
    // LifeLapse::new(start) initializes `end` to `start`.
    // LifeLapse::push(TimedLifeChunk) updates `end`.
    // LifeLapse::start() and LifeLapse::end() are public.

    // Minimal LifeChunk for testing purposes.
    fn minimal_life_chunk(duration_hours: i64, duration_minutes: i64) -> LifeChunk {
        // Construct a line that get_life_chunk can parse to create a LifeChunk
        // with the desired duration.
        // Format: "H M Description"
        // We only care about duration for these tests, description/categories can be minimal.
        let line = format!("{} {} test_activity", duration_hours, duration_minutes);
        get_life_chunk(&line)
    }

    // Helper to create a LifeLapse with a specific start and duration
    fn create_lapse(start_datetime: Timestamp, duration_seconds: i64) -> LifeLapse {
        let mut lapse = LifeLapse::new(start_datetime);
        if duration_seconds > 0 {
            let hours = duration_seconds / 3600;
            let minutes = (duration_seconds % 3600) / 60;
            let chunk = minimal_life_chunk(hours, minutes);
            let timed_chunk = TimedLifeChunk { start: start_datetime, life_chunk: chunk };
            // This is not how LifeLapse::push works, it doesn't take TimedLifeChunk directly in some versions.
            // It takes LifeChunk and internally creates TimedLifeChunk.
            // Let's assume LifeLapse has a method to add a duration or a simple chunk.
            // The actual `push` method in `parse.rs` for `LifeLapse` is private.
            // We'll use the public `extend` method instead.
            lapse.extend(std::iter::once(timed_chunk));
        }
        lapse
    }

    #[test]
    fn test_format_lifelapses_no_lapses() {
        let lapses = vec![];
        assert_eq!(format_lifelapses(&lapses).len(), 0);
    }

    #[test]
    fn test_format_lifelapses_single_lapse() {
        // Colored output makes direct string comparison tricky.
        // We are interested in whether gap lines are added or not.
        // format_list_of_chunks adds lines for the lapse itself.
        set_override(false); // Disable color for consistent output
        let start_time = Local.ymd(2024, 1, 1).and_hms_opt(10, 0, 0).unwrap();
        let lapses = vec![create_lapse(start_time, 3600)]; // 1 hour duration
        let formatted_lines = format_lifelapses(&lapses);
        // Expect no gap lines, only lines from format_list_of_chunks
        // format_list_of_chunks adds: header, one line per chunk (here 1), and a blank line.
        // So, 3 lines for a single chunk lapse.
        assert!(formatted_lines.len() > 0);
        assert!(!formatted_lines.iter().any(|line| line.contains("--- Gap of")));
        unset_override();
    }

    #[test]
    fn test_format_lifelapses_with_gap() {
        set_override(false);
        let t1_start = Local.ymd(2024, 1, 1).and_hms_opt(10, 0, 0).unwrap();
        let lapse1 = create_lapse(t1_start, 3600); // Ends at 11:00:00

        let t2_start = Local.ymd(2024, 1, 1).and_hms_opt(12, 0, 0).unwrap(); // Starts 1 hour after lapse1 ends
        let lapse2 = create_lapse(t2_start, 1800); // 30 mins duration

        let lapses = vec![lapse1, lapse2];
        let formatted_lines = format_lifelapses(&lapses);

        assert!(formatted_lines.iter().any(|line| line == "--- Gap of 1 hour ---"));
        unset_override();
    }

    #[test]
    fn test_format_lifelapses_no_gap() {
        set_override(false);
        let t1_start = Local.ymd(2024, 1, 1).and_hms_opt(10, 0, 0).unwrap();
        let lapse1 = create_lapse(t1_start, 3600); // Ends at 11:00:00

        let t2_start = Local.ymd(2024, 1, 1).and_hms_opt(11, 0, 0).unwrap(); // Starts immediately after lapse1 ends
        let lapse2 = create_lapse(t2_start, 1800);

        let lapses = vec![lapse1, lapse2];
        let formatted_lines = format_lifelapses(&lapses);

        assert!(!formatted_lines.iter().any(|line| line.contains("--- Gap of")));
        unset_override();
    }

    #[test]
    fn test_format_lifelapses_overlapping_lapses_behavior() {
        // Current implementation of gap calculation `ll.start().signed_duration_since(prev_end)`
        // If `ll.start()` is before `prev_end`, the duration will be negative.
        // The condition `if gap > ChronoDuration::zero()` means negative gaps are not printed.
        set_override(false);
        let t1_start = Local.ymd(2024, 1, 1).and_hms_opt(10, 0, 0).unwrap();
        let lapse1 = create_lapse(t1_start, 7200); // Ends at 12:00:00

        let t2_start = Local.ymd(2024, 1, 1).and_hms_opt(11, 0, 0).unwrap(); // Starts *before* lapse1 ends
        let lapse2 = create_lapse(t2_start, 1800);

        let lapses = vec![lapse1, lapse2];
        let formatted_lines = format_lifelapses(&lapses);

        // No "Gap of" line should be printed for negative/zero gaps.
        // An "Overlapping activities" warning is printed by format_list_of_chunks if tokens overlap,
        // but that's different from the gap between lapses.
        assert!(!formatted_lines.iter().any(|line| line.contains("--- Gap of")));
        unset_override();
    }
}
