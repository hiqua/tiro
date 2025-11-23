extern crate vergen;

use std::process::Command;
use vergen::Emitter;

fn is_current_repo_clean() -> bool {
    if let Ok(out) = Command::new("git")
        .arg("status")
        .arg("--porcelain")
        .output()
    {
        return out.stdout.is_empty();
    }

    false
}

fn main() {
    Emitter::new()
        .emit()
        .expect("Unable to generate the cargo keys!");

    let version_suffix = if is_current_repo_clean() {
        "clean"
    } else {
        "dirty"
    };

    println!("cargo:rustc-env=VERSION_SUFFIX={}", version_suffix);
}
