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
