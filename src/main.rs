#![allow(unused_imports)]

#[macro_use]
extern crate clap;

use std::collections::VecDeque;
use std::fmt;
use std::io::Write;
use std::sync::mpsc;
use std::sync::mpsc::{channel, Sender};
use std::thread::JoinHandle;
use std::time::Duration as StdDuration;

use clap::App;
use notify::{watcher, RecursiveMode, Watcher};

use crate::config::{load_config_from_matches, Config};
use crate::domain::TimedLifeChunk;
use crate::notification::spawn_notification_thread;
use crate::output::{
    delay, get_all_lines, get_writers, write_global_summary, write_plan, write_summary, Writers,
};
use crate::parse::get_all_life_lapses;
use crate::summary::{compute_all_summaries, merge_summaries_on_same_date};

mod config;
mod domain;
mod merge;
mod notification;
mod output;
mod parse;
mod parse_state;
mod pretty_print;
mod summary;

type Writer = (Box<dyn Write>, bool);

fn main_loop(config: &Config) -> anyhow::Result<(Sender<()>, Option<JoinHandle<()>>)> {
    // START PARSE
    let file_paths = config.get_file_paths();
    let all_activities_line = get_all_lines(Box::new(file_paths.into_iter()))?;

    let (start_time, all_life_lapses) = get_all_life_lapses(all_activities_line, config);
    // END PARSE

    // COMPUTE SUMMARIES
    let all_summaries = compute_all_summaries(&all_life_lapses);
    let all_summaries = merge_summaries_on_same_date(all_summaries);

    // WRITE
    let Writers {
        plan_writers,
        summary_writers,
        global_summary_writers,
    } = get_writers(start_time, config);

    write_plan(&all_life_lapses, plan_writers)?;
    write_summary(&all_summaries, summary_writers)?;
    write_global_summary(&all_summaries, global_summary_writers)?;
    // END WRITE

    // WATCHING
    let (tx, rx) = channel();
    let handle = if config.notify {
        // XXX: should exit early if lifelapses are incompatible, otherwise this will be buggy
        let mut q: Vec<TimedLifeChunk> = vec![];
        for ll in all_life_lapses {
            q.extend(ll.tokens());
        }
        q.sort_by_key(|tlc| tlc.start);
        Some(spawn_notification_thread(VecDeque::from(q), rx))
    } else {
        None
    };
    Ok((tx, handle))
}

/// Should be allowed to fail, after some timeouts or number of attempts.
fn watch_main_loop(config: &Config) -> anyhow::Result<()> {
    let (tx, rx) = channel();

    // XXX: infinite loop, causes problems, how to make it reasonable?

    let mut watcher_var = loop {
        // clone also use in watcher method anyway
        if let Ok(r) = watcher(tx.clone(), StdDuration::from_secs(0)) {
            break r;
        }
        delay();
    };

    let mut notification_tx;
    let mut handle;
    let paths = config.get_file_paths();
    'big_loop: loop {
        loop {
            if let Ok((n, h)) = main_loop(config) {
                notification_tx = n;
                handle = h;
                break;
            }
            delay();
        }

        for filepath in &paths {
            let w = watcher_var.watch(filepath, RecursiveMode::Recursive);
            if w.is_err() {
                delay();
                continue 'big_loop;
            }
        }
        match rx.recv() {
            Ok(_event) => {
                // XXX: ugly, matches all around the place
                if !config.quiet {
                    print!("{}[H", 27 as char);
                    print!("{}[J", 27 as char);
                }
                // kill notification thread
                if let Some(h) = handle {
                    // should we care and do all this cleaning?
                    if notification_tx.send(()).is_err() || h.join().is_err() {
                        // XXX: ignoring errors, especially in case where notifications are disabled. Better way?
                    }
                }
            }
            Err(e) => println!("watch error: {:?}", e),
        }
    }
}

fn main() -> anyhow::Result<()> {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();

    let config = load_config_from_matches(&matches);
    if config.watch {
        watch_main_loop(&config)?;
    } else {
        main_loop(&config)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use tempfile::NamedTempFile;
    use std::collections::BTreeSet;
    use std::path::PathBuf;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_main_loop_basic() {
        let mut activity_file = NamedTempFile::new().unwrap();
        writeln!(activity_file, "2024-01-01 10:00\n1 0 Task 1 @work").unwrap();

        let plan_file = NamedTempFile::new().unwrap();
        let summary_file = NamedTempFile::new().unwrap();

        let mut config = Config::new(vec![activity_file.path().to_path_buf()]);
        config.plan_out = Some(plan_file.path().to_str().unwrap().to_string());
        config.summary_out = Some(summary_file.path().to_str().unwrap().to_string());
        config.quiet = true;

        let result = main_loop(&config);
        assert!(result.is_ok());

        let plan_output = std::fs::read_to_string(plan_file.path()).unwrap();
        assert!(plan_output.contains("-- 10:00 2024-01-01"));
        assert!(plan_output.contains("-> 11h00 Task 1 @work"));

        let summary_output = std::fs::read_to_string(summary_file.path()).unwrap();
        assert!(!summary_output.contains("2024-01-01+00:00 (summary)"));
        assert!(summary_output.contains("(all past summaries)"));
        assert!(summary_output.contains("@work: 01h00"));
    }
}
