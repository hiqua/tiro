use std::fs::{create_dir_all, File};
use std::io;
use std::io::Result;
use std::io::Write;
use std::iter::{Extend, Iterator};
use std::path::{Path, PathBuf};

use chrono::Datelike;
use colored::control::{set_override, unset_override};
use colored::Color;
use colored::*;

use crate::config::Quadrant;
use crate::config::Quadrant::*;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use crate::summary::Timestamp;

pub enum OutputTarget {
    Stdout,
    File(PathBuf),
    Managed {
        path: PathBuf,
        dir: PathBuf,
        prefix: String,
    },
}

pub struct OutputWriter {
    target: OutputTarget,
    color: bool,
}

pub fn get_output_writer(
    path: Option<&str>,
    prefix: &str,
    filetime: &str,
    start_time: Timestamp,
) -> OutputWriter {
    let (target, color) = match path {
        Some("-") => (OutputTarget::Stdout, true),
        Some(x) => {
            let path = Path::new(x);
            if path.is_dir() {
                let file_path: PathBuf = [
                    x,
                    &format!(
                        "{:4}-w{:02}",
                        start_time.year(),
                        start_time.iso_week().week()
                    ),
                    &format!("{}", start_time.date().naive_local()),
                    &format!("{}_{}.txt", prefix, filetime),
                ]
                .iter()
                .collect();
                if let Some(parent) = file_path.parent() {
                    create_dir_all(parent)
                        .expect("Could not create some parents of the output files.");
                }
                (
                    OutputTarget::Managed {
                        path: file_path,
                        dir: Path::new(x).join(format!(
                            "{:4}-w{:02}/{}",
                            start_time.year(),
                            start_time.iso_week().week(),
                            start_time.date().naive_local()
                        )),
                        prefix: prefix.to_string(),
                    },
                    false,
                )
            } else {
                (OutputTarget::File(PathBuf::from(x)), false)
            }
        }
        None => (OutputTarget::Stdout, true),
    };

    OutputWriter { target, color }
}

pub fn format_lifelapses(lifelapses: &[LifeLapse]) -> Vec<String> {
    let mut lines = vec![];
    for ll in lifelapses {
        lines.extend(format_list_of_chunks(ll.start(), ll.tokens_as_ref()));
    }
    lines
}

/// Scans the given directory for files starting with `prefix` and ending with `.txt`.
/// If the content of an existing file is a prefix of `new_content`, the existing file is deleted.
/// This helps in removing redundant intermediate files.
fn cleanup_redundant_files(dir: &Path, prefix: &str, new_content: &str) -> Result<()> {
    if !dir.exists() {
        return Ok(());
    }

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
                if file_name.starts_with(prefix) && file_name.ends_with(".txt") {
                    let content = std::fs::read_to_string(&path)?;
                    let content_trimmed = content.trim_end_matches(['\n', '\r']);

                    if let Some(remainder) = new_content.strip_prefix(content_trimmed) {
                        if remainder.is_empty()
                            || remainder.starts_with('\n')
                            || remainder.starts_with('\r')
                        {
                            std::fs::remove_file(&path)?;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

/// Need this producer because the coloring won't be flexible otherwise
pub fn write_to(
    output_producer: impl Fn() -> Vec<String>,
    writer: &mut OutputWriter,
) -> Result<()> {
    let output = output_producer();
    let content = output.join("\n");

    if let OutputTarget::Managed {
        ref dir,
        ref prefix,
        ..
    } = writer.target
    {
        cleanup_redundant_files(dir, prefix, &content)?;
    }

    let mut out_writer: Box<dyn Write> = match &writer.target {
        OutputTarget::Stdout => Box::new(io::stdout()),
        OutputTarget::File(path) | OutputTarget::Managed { path, .. } => {
            Box::new(File::create(path)?)
        }
    };

    if !writer.color {
        set_override(false);
    }
    writeln!(out_writer, "{}", content)?;
    unset_override();
    Ok(())
}

fn format_list_of_chunks(start_time: Timestamp, list_of_lc: &[TimedLifeChunk]) -> Vec<String> {
    let mut desc = vec![];
    // XXX: show date as well
    let format_date = |d: Timestamp| {
        let format = "%H:%M %Y-%m-%d";
        let s = format!("-- {}", d.format(format));
        s.bold().to_string()
    };
    desc.push(format_date(start_time));
    let mut curr_time = start_time;
    for c in list_of_lc {
        if c.start < curr_time {
            desc.push(get_warning_overlapping());
        }
        curr_time = c.start;
        desc.push(format_life_chunk(&c.life_chunk, c.start));
    }
    desc.push("".to_string());
    desc
}

/// Split string into strings of the given size.
fn format_line_in_lines(s: &str, size: usize) -> Vec<String> {
    let mut lines: Vec<Vec<&str>> = vec![vec![]];
    let mut cur_len = 0;
    for w in s.split_whitespace() {
        if cur_len == 0 || cur_len + w.chars().count() < size {
            lines.last_mut().unwrap().push(w);
            cur_len += w.chars().count();
        } else {
            lines.push(vec![w]);
            cur_len = 0;
        }
    }
    lines.iter().map(|lv| lv.join(" ")).collect()
}

fn format_life_chunk(chunk: &LifeChunk, start_time: Timestamp) -> String {
    let screen_size = 70;
    let end_time = start_time + chunk.duration;

    let mut lines_to_print = vec![];
    {
        let string_lines: Vec<String> = format_line_in_lines(chunk.get_input(), screen_size);

        let mut it = string_lines.iter();
        if let Some(first_line) = it.next() {
            lines_to_print.push(color_line(
                format!("-> {} {}", end_time.format("%Hh%M"), first_line),
                chunk.quadrant,
            ));
        }
        it.for_each(|s| lines_to_print.push(color_line(format!("         {}", s), chunk.quadrant)));
    }

    lines_to_print.join("\n")
}

/// XXX: should be configurable
fn color_line(s: String, q: Quadrant) -> String {
    let c = match q {
        Q1 => Color::Red,
        Q2 => Color::Yellow,
        Q3 => Color::Cyan,
        Q4 => Color::Blue,
        Q5 => Color::Green,
        Q6 => Color::White,
    };
    format!("{}", s.color(c))
}

fn get_warning_overlapping() -> String {
    "\n/!\\ Overlapping activities /!\\\n".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::io::Write;
    use tempfile::tempdir;

    #[test]
    fn test_cleanup_redundant_files_superset() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create an existing file
        let file_path = dir_path.join(format!("{}_old.txt", prefix));
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "line 1").unwrap();
        writeln!(file, "line 2").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content is a superset
        let new_content = "line 1\nline 2\nline 3\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(!file_path.exists(), "Old file should be deleted");
    }

    #[test]
    fn test_cleanup_redundant_files_not_superset() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create an existing file
        let file_path = dir_path.join(format!("{}_old.txt", prefix));
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "line 1").unwrap();
        writeln!(file, "line 2").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content is different
        let new_content = "line 1\nline 3\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(file_path.exists(), "Old file should be preserved");
    }

    #[test]
    fn test_cleanup_redundant_files_exact_match() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create an existing file
        let file_path = dir_path.join(format!("{}_old.txt", prefix));
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "line 1").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content is exact match
        let new_content = "line 1\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(
            !file_path.exists(),
            "Old file should be deleted (exact match)"
        );
    }

    #[test]
    fn test_cleanup_redundant_files_different_prefix() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create a file with different prefix
        let file_path = dir_path.join("other_prefix_old.txt");
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "line 1").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content is superset
        let new_content = "line 1\nline 2\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(
            file_path.exists(),
            "File with different prefix should be preserved"
        );
    }

    #[test]
    fn test_cleanup_redundant_files_multiple_redundant() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create multiple existing files
        let file_path1 = dir_path.join(format!("{}_old1.txt", prefix));
        let file_path2 = dir_path.join(format!("{}_old2.txt", prefix));
        let mut file1 = File::create(&file_path1).unwrap();
        let mut file2 = File::create(&file_path2).unwrap();
        writeln!(file1, "line 1").unwrap();
        writeln!(file2, "line 1\nline 2").unwrap();
        assert!(file_path1.exists(), "File 1 should exist before cleanup");
        assert!(file_path2.exists(), "File 2 should exist before cleanup");

        // New content is a superset of both
        let new_content = "line 1\nline 2\nline 3\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(!file_path1.exists(), "Old file 1 should be deleted");
        assert!(!file_path2.exists(), "Old file 2 should be deleted");
    }

    #[test]
    fn test_cleanup_redundant_files_not_a_prefix_on_line_boundary() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create an existing file
        let file_path = dir_path.join(format!("{}_old.txt", prefix));
        let mut file = File::create(&file_path).unwrap();
        write!(file, "line 1 partial").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content starts with the old one, but not on a line boundary
        let new_content = "line 1 partial extra";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(
            file_path.exists(),
            "Old file should be preserved (not a prefix on line boundary)"
        );
    }

    #[test]
    fn test_cleanup_redundant_files_new_content_shorter() {
        let dir = tempdir().unwrap();
        let dir_path = dir.path();
        let prefix = "test_prefix";

        // Create an existing file
        let file_path = dir_path.join(format!("{}_old.txt", prefix));
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "line 1\nline 2").unwrap();
        assert!(file_path.exists(), "File should exist before cleanup");

        // New content is shorter
        let new_content = "line 1\n";

        cleanup_redundant_files(dir_path, prefix, new_content).unwrap();

        assert!(
            file_path.exists(),
            "Old file should be preserved (new content is shorter)"
        );
    }
}
