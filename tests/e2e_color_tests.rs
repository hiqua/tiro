use std::fs;
use std::process::Command;
use tempfile::tempdir;
use textwrap::dedent;
use walkdir::WalkDir;

#[test]
fn cli_file_output_has_no_color() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_path = dir.path().join("output.txt");

    fs::write(&activities_path, "2020-12-01 10:00\n1 0 Task 1 @work\n").unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--plan")
        .arg(output_path.to_str().unwrap())
        // We do NOT use --quiet so we can potentially check stdout too, 
        // but mainly we want to check the file.
        ;

    // Force color in stdout if possible?
    // The binary uses `colored` which checks TTY.
    // We can try setting CLICOLOR_FORCE=1 env var which `colored` might respect.
    cmd.env("CLICOLOR_FORCE", "1");

    let output = cmd.assert().success();

    // Check file content
    assert!(output_path.exists(), "Output file should be created");
    let content = fs::read_to_string(&output_path).unwrap();

    // ANSI escape code for Red is \x1b[31m (or similar).
    // We just check for the escape character \x1b.
    assert!(
        !content.contains("\x1b"),
        "File should not contain ANSI escape codes"
    );
    assert!(
        content.contains("Task 1"),
        "File should contain the task text"
    );

    // Check stdout content
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();
    // If CLICOLOR_FORCE worked, stdout should have colors.
    // Note: `colored` crate respects `CLICOLOR_FORCE` or `FORCE_COLOR`.
    // Let's check if we see escape codes in stdout.
    // If this assertion fails in some envs we might need to relax it, but let's try.
    assert!(
        stdout.contains("\x1b"),
        "Stdout SHOULD contain ANSI escape codes when forced"
    );
}

#[test]
fn cli_summary_output_has_no_color() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_path = dir.path().join("summary.txt");

    fs::write(&activities_path, "2020-12-01 10:00\n1 0 Task 1 @work\n").unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--summary")
        .arg(output_path.to_str().unwrap());

    cmd.env("CLICOLOR_FORCE", "1");

    cmd.assert().success();

    assert!(
        output_path.exists(),
        "Summary output file should be created"
    );
    let content = fs::read_to_string(&output_path).unwrap();

    assert!(
        !content.contains("\x1b"),
        "Summary file should not contain ANSI escape codes"
    );
    assert!(
        content.contains("@work"),
        "Summary file should contain the category"
    );
}

#[test]
fn cli_directory_output_has_no_color() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_dir = dir.path().join("output_dir");
    fs::create_dir(&output_dir).unwrap();

    fs::write(&activities_path, "2020-12-01 10:00\n1 0 Task 1 @work\n").unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--plan")
        .arg(output_dir.to_str().unwrap())
        .arg("--summary")
        .arg(output_dir.to_str().unwrap());

    cmd.env("CLICOLOR_FORCE", "1");

    cmd.assert().success();

    // Tiro creates a subdirectory structure like YYYY-wWW/YYYY-MM-DD/
    // We need to find the files recursively or just look for any .txt file in the tree.
    let mut found_files = false;
    for entry in WalkDir::new(&output_dir) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            let file_name = entry.file_name().to_str().unwrap();
            if file_name.ends_with(".txt") {
                found_files = true;
                let content = fs::read_to_string(entry.path()).unwrap();
                assert!(
                    !content.contains("\x1b"),
                    "File {} should not contain ANSI escape codes",
                    file_name
                );

                // Verify it's one of the expected files
                if file_name.starts_with("plan_") {
                    assert!(content.contains("Task 1"), "Plan file should contain task");
                } else if file_name.starts_with("summary_")
                    || file_name.starts_with("global_summary_")
                {
                    assert!(
                        content.contains("@work"),
                        "Summary file should contain category"
                    );
                }
            }
        }
    }
    assert!(
        found_files,
        "Should have found generated output files in the directory"
    );
}
