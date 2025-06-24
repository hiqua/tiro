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
use crate::input::{
    delay, get_all_lines, get_writers, write_global_summary, write_plan, write_summary, Writers,
};
use crate::notification::spawn_notification_thread;
use crate::parse::{get_all_life_lapses, TimedLifeChunk};
use crate::summary::{compute_all_summaries, merge_summaries_on_same_date};

mod config;
mod input;
mod merge;
mod notification;
mod parse;
mod parse_state;
mod pretty_print;
mod summary;

type Writer = (Box<dyn Write>, bool);
type TiroResult<T> = anyhow::Result<T>;

fn main_loop(config: &Config) -> TiroResult<(Sender<()>, Option<JoinHandle<()>>)> {
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
