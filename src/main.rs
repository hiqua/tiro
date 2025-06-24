// #![allow(unused_imports)] // Removed

use std::collections::VecDeque;
use std::io::Write;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Sender};
use std::thread::JoinHandle;

use clap::Parser;
use notify::{
    Config as NotifyConfig, Error as NotifyError, Event, RecommendedWatcher, RecursiveMode, Watcher,
}; // Updated notify imports

use crate::config::{load_config_from_cli_args, Config};

/// Planning tool
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CliArgs {
    /// Sets the activity file to use, - is stdin
    #[arg(short, long, value_name = "FILE")] // Removed multiple = true
    activities: Vec<PathBuf>,

    /// Sets the plan file to export to, defaults to stdout
    #[arg(short, long, value_name = "FILE")]
    plan: Option<PathBuf>,

    /// Sets the summary file to export to, defaults to stdout
    #[arg(short, long, value_name = "FILE")]
    summary: Option<PathBuf>,

    /// Watch the input file
    #[arg(short, long)]
    watch: bool,

    /// Notify when the next activity is close
    #[arg(short, long)]
    notify: bool,

    /// Quiet output
    #[arg(short, long)]
    quiet: bool,

    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    config: PathBuf, // Required, so not Option
}
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

fn main_loop(config: &Config) -> anyhow::Result<(Sender<()>, Option<JoinHandle<()>>)> {
    // START PARSE
    let file_paths = config.get_file_paths();
    let all_activities_line = get_all_lines(Box::new(file_paths.into_iter()))?;

    let (start_time, all_life_lapses) = get_all_life_lapses(all_activities_line, config)?;
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
    let (tx, rx) = channel(); // This channel will receive () from the event handler

    // Create a new sender for the notify event handler to use,
    // as the handler for RecommendedWatcher takes the sender by value.
    let event_handler_tx = tx.clone();
    let mut watcher_var: RecommendedWatcher = RecommendedWatcher::new(
        move |res: Result<Event, NotifyError>| {
            match res {
                Ok(_event) => {
                    // We don't need event details, just signal that something happened
                    event_handler_tx.send(()).unwrap_or_else(|e| {
                        eprintln!("[ERROR] Failed to send filesystem event signal: {:?}", e);
                    });
                }
                Err(e) => {
                    eprintln!("[ERROR] Watch error: {:?}", e);
                }
            }
        },
        NotifyConfig::default(),
    )?; // RecommendedWatcher::new returns a Result

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
                    if let Err(e) = notification_tx.send(()) {
                        eprintln!(
                            "[ERROR] Failed to send stop signal to notification thread: {:?}",
                            e
                        );
                    }
                    if let Err(e) = h.join() {
                        eprintln!("[ERROR] Notification thread panicked: {:?}", e);
                    }
                }
            }
            Err(e) => eprintln!("[ERROR] Filesystem watch error: {:?}", e), // Changed to eprintln
        }
    }
}

fn main() -> anyhow::Result<()> {
    let cli_args = CliArgs::parse();

    // Pass cli_args to a renamed/refactored version of load_config_from_matches
    let config = load_config_from_cli_args(&cli_args);
    if config.watch {
        watch_main_loop(&config)?;
    } else {
        main_loop(&config)?;
    }

    Ok(())
}
