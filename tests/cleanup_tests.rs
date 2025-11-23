use std::fs;
use std::process::Command;
use tempfile::tempdir;
use textwrap::dedent;

#[test]
fn cleanup_removes_redundant_files() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_dir = dir.path().join("history");

    fs::create_dir(&output_dir).unwrap();

    // Initial activities
    fs::write(&activities_path, "2025-01-01 10:00\n1 0 Task 1 @work\n").unwrap();

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
        .arg("--quiet");

    cmd.assert().success();

    // Find generated file recursively
    // Note: The file structure is history/YYYY-wWW/YYYY-MM-DD/filename.txt
    let find_files = || {
        let mut files = vec![];
        for entry in walkdir::WalkDir::new(&output_dir) {
            let entry = entry.unwrap();
            if entry.file_type().is_file() && entry.path().extension().is_some_and(|e| e == "txt") {
                files.push(entry.path().to_owned());
            }
        }
        files
    };

    let files_1 = find_files();
    assert_eq!(files_1.len(), 1, "Should have created one plan file");
    let file_1 = files_1[0].clone();

    // Run again with superset
    fs::write(
        &activities_path,
        "2025-01-01 10:00\n1 0 Task 1 @work\n1 0 Task 2 @work\n",
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--plan")
        .arg(output_dir.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    let files_2 = find_files();
    assert_eq!(
        files_2.len(),
        1,
        "Should still have only one plan file (redundant one deleted)"
    );
    let file_2 = files_2[0].clone();

    assert_ne!(
        file_1, file_2,
        "Filename should have changed (new timestamp)"
    );
    assert!(!file_1.exists(), "Old file should be deleted");
    assert!(file_2.exists(), "New file should exist");
}

#[test]
fn cleanup_preserves_non_redundant_files() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_dir = dir.path().join("history_preserve");

    fs::create_dir(&output_dir).unwrap();

    // Initial activities
    fs::write(&activities_path, "2025-01-01 10:00\n1 0 Task 1 @work\n").unwrap();

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
        .arg("--quiet");

    cmd.assert().success();

    let find_files = || {
        let mut files = vec![];
        for entry in walkdir::WalkDir::new(&output_dir) {
            let entry = entry.unwrap();
            if entry.file_type().is_file() && entry.path().extension().is_some_and(|e| e == "txt") {
                files.push(entry.path().to_owned());
            }
        }
        files
    };

    let files_1 = find_files();
    assert_eq!(files_1.len(), 1);

    // Run again with different content (not superset)
    fs::write(
        &activities_path,
        "2025-01-01 10:00\n1 0 Task 1 Modified @work\n",
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--plan")
        .arg(output_dir.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    let files_2 = find_files();
    assert_eq!(
        files_2.len(),
        2,
        "Should have two plan files (history preserved)"
    );
}

#[test]
fn cleanup_removes_redundant_summary_files() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let output_dir = dir.path().join("history");

    fs::create_dir(&output_dir).unwrap();

    // Initial activities
    fs::write(&activities_path, "2025-01-01 10:00\n1 0 Task 1 @work\n").unwrap();

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
        .arg(output_dir.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    // Find generated file recursively
    // Note: The file structure is history/YYYY-wWW/YYYY-MM-DD/filename.txt
    let find_files = || {
        let mut files = vec![];
        for entry in walkdir::WalkDir::new(&output_dir) {
            let entry = entry.unwrap();
            if entry.file_type().is_file() && entry.path().extension().is_some_and(|e| e == "txt") {
                files.push(entry.path().to_owned());
            }
        }
        files
    };

    let files_1 = find_files();
    assert_eq!(
        files_1.len(),
        2,
        "Should have created one summary file and one global summary file"
    );
    let summary_file_1 = files_1
        .iter()
        .find(|f| f.file_name().unwrap().to_str().unwrap().starts_with("summary"))
        .unwrap()
        .clone();
    let global_summary_file_1 = files_1
        .iter()
        .find(|f| {
            f.file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .starts_with("global_summary")
        })
        .unwrap()
        .clone();

    // Run again with an additional activity with a new category.
    // This should result in a summary that is a superset of the previous one.
    fs::write(
        &activities_path,
        "2025-01-01 10:00\n1 0 Task 1 @work\n1 0 Task 2 @zen\n",
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--summary")
        .arg(output_dir.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    let files_2 = find_files();
    assert_eq!(
        files_2.len(),
        2,
        "Should still have two summary files (redundant one deleted)"
    );

    let summary_file_2 = files_2
        .iter()
        .find(|f| f.file_name().unwrap().to_str().unwrap().starts_with("summary"))
        .unwrap()
        .clone();
    let global_summary_file_2 = files_2
        .iter()
        .find(|f| {
            f.file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .starts_with("global_summary")
        })
        .unwrap()
        .clone();

    assert!(!summary_file_1.exists(), "Old summary file should be deleted");
    assert!(
        !global_summary_file_1.exists(),
        "Old global summary file should be deleted"
    );
    assert!(summary_file_2.exists(), "New summary file should exist");
    assert!(
        global_summary_file_2.exists(),
        "New global summary file should exist"
    );
}
