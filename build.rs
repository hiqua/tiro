use std::process::Command;
use vergen::EmitBuilder; // Updated use statement

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
    EmitBuilder::builder()
        // .all_cargo() // Removed, feature in Cargo.toml should handle this
        .emit()
        .expect("Unable to generate vergen keys!");

    let version_suffix = if is_current_repo_clean() {
        "clean"
    } else {
        "dirty"
    };

    println!("cargo:rustc-env=VERSION_SUFFIX={}", version_suffix);
}
