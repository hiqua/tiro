use crate::config::Config;
use crate::parse::{read_lines_from_file, read_stdin_lines, LifeLapse};
use crate::pretty_print::{format_lifelapses, get_output_writer, write_to};
use crate::summary::{compute_context_summary, format_category_summary, Summary, Timestamp};
use crate::{TiroResult, Writer};
use chrono::{Local, SecondsFormat};

use std::borrow::BorrowMut;

use std::io::Write;
use std::path::PathBuf;

use std::thread::sleep;
use std::time::Duration as StdDuration;

pub fn write_plan(
    all_life_lapses: &[LifeLapse],
    mut plan_writers: Vec<(Box<dyn Write>, bool)>,
) -> TiroResult<()> {
    for (ref mut plan_writer, plan_color) in &mut plan_writers {
        write_to(
            || format_lifelapses(all_life_lapses),
            plan_writer.borrow_mut(),
            *plan_color,
        )?;
    }
    Ok(())
}

pub fn write_summary(
    all_summaries: &[(Timestamp, Summary)],
    mut summary_writers: Vec<(Box<dyn Write>, bool)>,
) -> TiroResult<()> {
    for (ts, summary) in all_summaries {
        for (ref mut summary_writer, summary_color) in &mut summary_writers {
            write_to(
                || format_category_summary(compute_context_summary(summary), ts.date()),
                summary_writer.borrow_mut(),
                *summary_color,
            )?;
        }
    }
    Ok(())
}

pub fn get_writers(start_time: Timestamp, config: &Config) -> (Vec<Writer>, Vec<Writer>) {
    let summary_out = config.summary_out.as_ref();
    let plan_out = config.plan_out.as_ref();

    // ":" does not seem to work well with Android
    let filetime = Local::now()
        .to_rfc3339_opts(SecondsFormat::Nanos, true)
        .replace(":", "_");
    let mut plan_writers = vec![];
    let mut summary_writers = vec![];

    let quiet = config.quiet;
    if !quiet {
        plan_writers.push(get_output_writer(Some("-"), "plan", &filetime, start_time));
        summary_writers.push(get_output_writer(
            Some("-"),
            "summary",
            &filetime,
            start_time,
        ));
    }

    if let Some(p) = plan_out {
        plan_writers.push(get_output_writer(
            Some(p.as_str()),
            "plan",
            &filetime,
            start_time,
        ));
    }

    if let Some(s) = summary_out {
        summary_writers.push(get_output_writer(
            Some(s.as_str()),
            "summary",
            &filetime,
            start_time,
        ));
    }

    (plan_writers, summary_writers)
}

pub fn get_all_lines(
    file_paths: Box<dyn Iterator<Item = PathBuf>>,
) -> TiroResult<Vec<Vec<String>>> {
    let mut all_activities_line = vec![];

    {
        for f in file_paths {
            all_activities_line.push(read_lines_from_file(f)?);
        }

        if all_activities_line.is_empty() {
            all_activities_line.push(read_stdin_lines()?);
        }
    }

    Ok(all_activities_line)
}
pub fn delay() {
    let delay = 500;
    sleep(StdDuration::from_millis(delay));
}
