#![allow(unused_imports)]

#[macro_use]
extern crate clap;

use clap::App;

use crate::config::load_config_from_matches;

mod app;
mod config;
mod domain;
mod merge;
mod notification;
mod output;
mod parse;
mod parse_state;
mod pretty_print;
mod summary;

fn main() -> anyhow::Result<()> {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();

    let config = load_config_from_matches(&matches);
    if config.watch {
        app::watch_main_loop(&config)?;
    } else {
        app::main_loop(&config)?;
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
