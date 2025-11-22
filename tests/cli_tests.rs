use std::process::Command;

use predicates::prelude::*;
use regex::Regex;
use std::fs;
use tempfile::tempdir;
use textwrap::dedent;

#[test]
fn cli_basic_run_with_activities_file_outputs_to_stdout() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");

    fs::write(
        &activities_path,
        "2020-12-01 10:00\n1 0 Task 1 @work\n2 0 Task 2 @home\n",
    )
    .unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            Q2 = ["@home"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config").arg(config_path.to_str().unwrap());

    let output = cmd.assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // Replace dynamic date in global summary with placeholder
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}\+\d{2}:\d{2} \(all past summaries\)").unwrap();
    let normalized = re.replace(&stdout, "<DATE> (all past summaries)");

    let expected = dedent(
        r#"
        -- 10:00 2020-12-01
        -> 11h00 Task 1 @work
        -> 13h00 Task 2 @home

        2020-12-01+01:00 (summary)
        @home: 02h00
        @work: 01h00

        <DATE> (all past summaries)
        @home: 02h00
        @work: 01h00

        "#,
    )
    .trim_start()
    .to_string();

    assert_eq!(normalized, expected);
}

#[test]
fn cli_with_summary_output_file_creates_file() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let summary_path = dir.path().join("summary.txt");

    fs::write(
        &activities_path,
        "2020-12-01 10:00\n1 30 Meeting @work\n0 45 Break @personal\n",
    )
    .unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@work"]
            Q3 = ["@personal"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--summary")
        .arg(summary_path.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    assert!(summary_path.exists(), "Summary file should be created");
    let summary_content = fs::read_to_string(&summary_path).unwrap();

    // Replace dynamic date with placeholder
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}\+\d{2}:\d{2} \(all past summaries\)").unwrap();
    let normalized = re.replace(&summary_content, "<DATE> (all past summaries)");

    let expected = dedent(
        r#"
        <DATE> (all past summaries)
        @personal: 00h45
        @work: 01h30

        "#,
    )
    .trim_start()
    .to_string();

    assert_eq!(normalized, expected);
}

#[test]
fn cli_with_plan_output_file_creates_file() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let plan_path = dir.path().join("plan.txt");

    fs::write(
        &activities_path,
        "2020-12-01 14:00\n1 0 Development work @code\n2 0 Code review @code\n",
    )
    .unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q2 = ["@code"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--plan")
        .arg(plan_path.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    assert!(plan_path.exists(), "Plan file should be created");
    let plan_content = fs::read_to_string(&plan_path).unwrap();

    let expected = dedent(
        r#"
        -- 14:00 2020-12-01
        -> 15h00 Development work @code
        -> 17h00 Code review @code

        "#,
    )
    .trim_start()
    .to_string();
    assert_eq!(plan_content, expected);
}

#[test]
fn cli_outputs_both_summary_and_global_summary() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");

    fs::write(
        &activities_path,
        dedent(
            r#"
            2020-12-01 10:00
            2 0 Morning work @project
            1 30 Meeting @meeting
            
            2020-12-02 09:00
            3 0 Development @project
            1 0 Review @meeting
            "#,
        ),
    )
    .unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@project"]
            Q2 = ["@meeting"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config").arg(config_path.to_str().unwrap());

    let output = cmd.assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // Replace dynamic date in global summary with placeholder
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}\+\d{2}:\d{2} \(all past summaries\)").unwrap();
    let normalized = re.replace(&stdout, "<DATE> (all past summaries)");

    let expected = dedent(
        r#"
        -- 10:00 2020-12-01
        -> 12h00 Morning work @project
        -> 13h30 Meeting @meeting

        -- 09:00 2020-12-02
        -> 12h00 Development @project
        -> 13h00 Review @meeting

        2020-12-01+01:00 (summary)
        @meeting: 01h30
        @project: 02h00

        2020-12-02+01:00 (summary)
        @meeting: 01h00
        @project: 03h00

        <DATE> (all past summaries)
        @meeting: 02h30
        @project: 05h00

        "#,
    )
    .trim_start()
    .to_string();

    assert_eq!(normalized, expected);
}

#[test]
fn cli_with_separate_summary_and_plan_files() {
    let dir = tempdir().unwrap();
    let activities_path = dir.path().join("activities.txt");
    let config_path = dir.path().join("config.toml");
    let summary_path = dir.path().join("summary_output.txt");
    let plan_path = dir.path().join("plan_output.txt");

    fs::write(
        &activities_path,
        "2020-12-01 10:00\n2 0 Task A @cat1\n1 0 Task B @cat2\n",
    )
    .unwrap();

    fs::write(
        &config_path,
        dedent(&format!(
            r#"
            activity_paths = ["{}"]

            [quadrants]
            Q1 = ["@cat1", "@cat2"]
            "#,
            activities_path.to_str().unwrap()
        )),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .arg("--summary")
        .arg(summary_path.to_str().unwrap())
        .arg("--plan")
        .arg(plan_path.to_str().unwrap())
        .arg("--quiet");

    cmd.assert().success();

    assert!(summary_path.exists(), "Summary file should be created");
    assert!(plan_path.exists(), "Plan file should be created");

    let summary_content = fs::read_to_string(&summary_path).unwrap();
    
    // Replace dynamic date with placeholder
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}\+\d{2}:\d{2} \(all past summaries\)").unwrap();
    let normalized = re.replace(&summary_content, "<DATE> (all past summaries)");

    let expected_summary = dedent(
        r#"
        <DATE> (all past summaries)
        @cat1: 02h00
        @cat2: 01h00

        "#,
    )
    .trim_start()
    .to_string();

    assert_eq!(normalized, expected_summary);

    let plan_content = fs::read_to_string(&plan_path).unwrap();
    let expected_plan = dedent(
        r#"
        -- 10:00 2020-12-01
        -> 12h00 Task A @cat1
        -> 13h00 Task B @cat2

        "#,
    )
    .trim_start()
    .to_string();
    assert_eq!(plan_content, expected_plan);
}

#[test]
fn cli_with_nonexistent_config_fails() {
    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config").arg("/nonexistent/config.toml");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains(
            "Cannot proceed without valid configuration path",
        ));
}

#[test]
fn cli_with_stdin_input_processes_activities() {
    let dir = tempdir().unwrap();
    let config_path = dir.path().join("config.toml");

    fs::write(
        &config_path,
        dedent(
            r#"
            activity_paths = []

            [quadrants]
            Q1 = ["@urgent"]
            "#,
        ),
    )
    .unwrap();

    let mut cmd = assert_cmd::Command::from_std(Command::new(env!("CARGO_BIN_EXE_tiro")));
    cmd.arg("--config")
        .arg(config_path.to_str().unwrap())
        .write_stdin("2020-12-01 09:00\n1 0 Morning task @urgent\n");

    let output = cmd.assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // Replace dynamic date in global summary with placeholder
    let re = Regex::new(r"\d{4}-\d{2}-\d{2}\+\d{2}:\d{2} \(all past summaries\)").unwrap();
    let normalized = re.replace(&stdout, "<DATE> (all past summaries)");

    let expected = dedent(
        r#"
        -- 09:00 2020-12-01
        -> 10h00 Morning task @urgent

        2020-12-01+01:00 (summary)
        @urgent: 01h00

        <DATE> (all past summaries)
        @urgent: 01h00

        "#,
    )
    .trim_start()
    .to_string();

    assert_eq!(normalized, expected);
}
