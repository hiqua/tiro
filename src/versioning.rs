use std::ops::Add;
use std::process::Command;

use git2::{Repository, RepositoryState};

pub fn full_version() -> String {
    yaml_version()
        + "-"
        + env!("VERGEN_SHA_SHORT")
        + "-"
        + env!("VERSION_SUFFIX")
        + " "
        + env!("VERGEN_BUILD_TIMESTAMP")
}

fn yaml_version() -> String {
    let yaml = load_yaml!("cli.yml");
    let version = &yaml["version"].as_str().unwrap();

    (*version).to_string()
}
