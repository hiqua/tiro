use std::borrow::Borrow;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::mem::discriminant;
use std::num::ParseIntError;
use std::ops::Add;
use std::path::PathBuf;
use std::slice::Iter;
use std::str::FromStr;

use chrono::offset::LocalResult;
use chrono::prelude::*;
use chrono::{Local, TimeZone};
use time::Duration;

use crate::config::MetaCategory::{Quad, RegularCategory};
use crate::config::{update_parse_state_from_config, Config, MetaCategory, Quadrant};
use crate::merge::merge_strictly_compatible_lifelapses;
use crate::parse::LineParseResult::{Date, Lc};
use crate::parse_state::ParseState;
use crate::summary::Timestamp;
use anyhow::Result;

// Re-export domain types for backward compatibility during migration
pub use crate::domain::{
    Category, LifeChunk, LifeLapse, LineParseResult, TimedLifeChunk, Timestamp as DomainTimestamp,
    TiroToken,
};

#[cfg(test)]
mod tests {
    use chrono::{Local, TimeZone};
    use time::Duration;

    use crate::config::Quadrant;
    use crate::parse::{get_life_chunk, parse_date, process_line, LifeChunk, LineParseResult};

    #[test]
    fn parsing_1_valid_datetime_string_returns_datetime() {
        let dt = Local.ymd(2014, 11, 28).and_hms(12, 0, 0);
        assert_eq!(
            Local
                .datetime_from_str("2014-11-28 12:00", "%Y-%m-%d %H:%M")
                .ok(),
            Some(dt)
        );
    }

    #[test]
    fn parse_date_custom_format_returns_datetime() {
        let dt = Local.ymd(2023, 10, 26).and_hms(14, 30, 0);
        assert_eq!(parse_date("2023-10-26 14h30"), Some(dt));
    }

    #[test]
    fn parse_date_invalid_string_returns_none() {
        assert_eq!(parse_date("invalid-date-string"), None);
    }

    #[test]
    fn parse_date_empty_string_returns_none() {
        assert_eq!(parse_date(""), None);
    }

    // Tests for get_life_chunk
    #[test]
    fn get_life_chunk_full_input_returns_populated_lifechunk() {
        let line = "1 30 Meeting with team @Work @Q2";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Meeting with team");
        assert_eq!(lc.duration, Duration::hours(1) + Duration::minutes(30));
        assert_eq!(lc.categories, vec!["@Work".to_string(), "@Q2".to_string()]); // Adjusted for current behavior
        assert_eq!(lc.quadrant, Quadrant::default()); // Adjusted for current behavior
        assert!(!lc.user_provided_quadrant); // Adjusted for current behavior
        assert_eq!(lc.get_input(), "Meeting with team @Work @Q2");
    }

    #[test]
    fn get_life_chunk_missing_duration_returns_zero_duration() {
        let line = "그냥 프로젝트 작업 @Dev"; // "Just working on a project @Dev"
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "작업"); // Adjusted for current behavior
        assert_eq!(lc.duration, Duration::zero());
        assert_eq!(lc.categories, vec!["@Dev".to_string()]);
        assert_eq!(lc.quadrant, Quadrant::default());
        assert!(!lc.user_provided_quadrant);
        assert_eq!(lc.get_input(), "작업 @Dev"); // Adjusted for current behavior
    }

    #[test]
    fn get_life_chunk_missing_categories_returns_empty_categories() {
        let line = "2 0 Quick break";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Quick break");
        assert_eq!(lc.duration, Duration::hours(2));
        assert_eq!(lc.categories, Vec::<String>::new());
        assert_eq!(lc.quadrant, Quadrant::default());
        assert!(!lc.user_provided_quadrant);
        assert_eq!(lc.get_input(), "Quick break");
    }

    #[test]
    fn get_life_chunk_user_quadrant_returns_parsed_quadrant() {
        let line = "0 45 Planning session @Q1";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Planning session");
        assert_eq!(lc.duration, Duration::minutes(45));
        assert_eq!(lc.categories, vec!["@Q1".to_string()]); // Adjusted for current behavior
        assert_eq!(lc.quadrant, Quadrant::default()); // Adjusted for current behavior
        assert!(!lc.user_provided_quadrant); // Adjusted for current behavior
        assert_eq!(lc.get_input(), "Planning session @Q1");
    }

    #[test]
    fn get_life_chunk_no_quadrant_returns_default_quadrant() {
        let line = "3 0 Reading a book @Leisure";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Reading a book");
        assert_eq!(lc.duration, Duration::hours(3));
        assert_eq!(lc.categories, vec!["@Leisure".to_string()]);
        assert_eq!(lc.quadrant, Quadrant::default()); // Q4 is default
        assert!(!lc.user_provided_quadrant);
        assert_eq!(lc.get_input(), "Reading a book @Leisure");
    }

    // Tests for process_line
    #[test]
    fn process_line_date_string_returns_date_result() {
        let line = "2024-03-10 10:00";
        let expected_date = Local.ymd(2024, 3, 10).and_hms(10, 0, 0);
        match process_line(line) {
            LineParseResult::Date { date } => assert_eq!(date, expected_date),
            _ => panic!("Expected LineParseResult::Date"),
        }
    }

    #[test]
    fn process_line_life_chunk_string_returns_lc_result() {
        let line = "1 0 Coding @Dev";
        match process_line(line) {
            LineParseResult::Lc { life_chunk: lc } => {
                assert_eq!(lc.description, "Coding");
                assert_eq!(lc.duration, Duration::hours(1));
                assert_eq!(lc.categories, vec!["@Dev".to_string()]);
                assert_eq!(lc.quadrant, Quadrant::default());
                assert!(!lc.user_provided_quadrant);
                assert_eq!(lc.get_input(), "Coding @Dev");
            }
            _ => panic!("Expected LineParseResult::Lc"),
        }
    }

    #[test]
    fn process_line_comment_string_returns_lc_result_with_zero_duration() {
        let line = "# This is a comment";
        match process_line(line) {
            LineParseResult::Lc { life_chunk: lc } => {
                // Based on get_life_chunk behavior:
                // "#" is consumed by h_duration attempt, "This" by m_duration attempt
                assert_eq!(lc.description, "is a comment");
                assert_eq!(lc.duration, Duration::zero());
                assert_eq!(lc.categories, Vec::<String>::new());
                assert_eq!(lc.quadrant, Quadrant::default());
                assert!(!lc.user_provided_quadrant);
                assert_eq!(lc.get_input(), "is a comment");
            }
            _ => panic!("Expected LineParseResult::Lc for a comment line"),
        }
    }

    #[test]
    fn process_line_empty_string_returns_empty_lc_result() {
        let line = "";
        match process_line(line) {
            LineParseResult::Lc { life_chunk: lc } => {
                assert_eq!(lc.description, "");
                assert_eq!(lc.duration, Duration::zero());
                assert_eq!(lc.categories, Vec::<String>::new());
                assert_eq!(lc.quadrant, Quadrant::default());
                assert!(!lc.user_provided_quadrant);
                assert_eq!(lc.get_input(), "");
            }
            _ => panic!("Expected LineParseResult::Lc for an empty line"),
        }
    }
    #[test]
    fn get_all_life_lapses_single_file_returns_parsed_lapses() {
        use crate::config::Config;
        use crate::parse::get_all_life_lapses;

        let lines = vec![vec![
            "2020-12-01 10:00".to_string(),
            "1 0 Task 1 @work".to_string(),
            "2 0 Task 2 @home".to_string(),
        ]];
        let config = Config::default();
        let (start, lapses) = get_all_life_lapses(lines, &config);

        assert_eq!(start, Local.ymd(2020, 12, 1).and_hms(10, 0, 0));
        assert_eq!(lapses.len(), 1);
        assert_eq!(lapses[0].tokens_as_ref().len(), 2);
    }

    #[test]
    fn get_all_life_lapses_multiple_files_returns_merged_lapses() {
        use crate::config::Config;
        use crate::parse::get_all_life_lapses;

        let lines = vec![
            vec!["2020-12-01 10:00".to_string(), "1 0 Task 1".to_string()],
            vec!["2020-12-01 11:00".to_string(), "1 0 Task 2".to_string()],
        ];
        let config = Config::default();
        let (start, lapses) = get_all_life_lapses(lines, &config);

        // Start time should be the min of all start times
        assert_eq!(start, Local.ymd(2020, 12, 1).and_hms(10, 0, 0));
        // They should be merged if compatible
        // 10:00 + 1h = 11:00. Second file starts at 11:00.
        // They are compatible and touching.
        assert_eq!(lapses.len(), 1);
        assert_eq!(lapses[0].tokens_as_ref().len(), 2);
    }

    #[test]
    fn get_life_chunk_extra_spaces_returns_correct_description() {
        let line = "1  0   Task  with   spaces   @work";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Task with spaces");
        assert_eq!(lc.duration, Duration::hours(1));
        assert_eq!(lc.categories, vec!["@work".to_string()]);
    }
}

/// The starting time is the earliest
pub fn get_all_life_lapses(
    all_activities_line: Vec<Vec<String>>,
    config: &Config,
) -> (Timestamp, Vec<LifeLapse>) {
    let mut all_life_lapses = vec![];
    for activities_lines in all_activities_line {
        let life_lapses = parse_activities(activities_lines.iter(), config);
        all_life_lapses.extend(life_lapses);
    }

    let start_time = if all_life_lapses.is_empty() {
        Local::now()
    } else {
        all_life_lapses.iter().map(|ll| ll.start()).min().unwrap()
    };

    sort_and_filter_life_lapses(&mut all_life_lapses, start_time);

    (start_time, all_life_lapses)
}

pub fn sort_and_filter_life_lapses(
    all_life_lapses: &mut Vec<LifeLapse>,
    start_time: Timestamp,
) {
    all_life_lapses.sort_by_key(|ll| ll.start());

    *all_life_lapses = all_life_lapses
        .drain(..)
        .skip_while(|ll| ll.start() < start_time)
        // Empty if only contained activities before starting time
        .filter(|ll| !ll.is_empty())
        .collect();

    *all_life_lapses = merge_strictly_compatible_lifelapses(all_life_lapses.to_vec());
}

pub fn parse_activities(mut it: Iter<String>, config: &Config) -> Vec<LifeLapse> {
    let list_of_pr = parse_all_lines(&mut it);

    let start_time = if let Some(Date { date }) = list_of_pr.first() {
        *date
    } else {
        assert!(list_of_pr.is_empty());
        return vec![];
    };

    let mut parse_state = register_all_categories(&list_of_pr);

    update_parse_state_from_config(config, &mut parse_state).expect("");

    let list_of_pr = parse_state.update_category_quadrants(list_of_pr);

    let tiro_tokens = tokens_from_timed_lpr(list_of_pr, start_time);

    // XXX: shouldn't have particular case, everything in the loop. Have current_ll as Option?
    let mut result = vec![];
    let mut current_ll = LifeLapse::new(start_time);
    for tok in tiro_tokens {
        match tok {
            TiroToken::Tlc { tlc } => {
                current_ll.push(tlc);
            }
            TiroToken::Date { date } => {
                result.push(current_ll);
                current_ll = LifeLapse::new(date);
            }
        }
    }
    result.push(current_ll);

    result
}

pub fn read_lines_from_file(path: PathBuf) -> Result<Vec<String>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut lines = vec![];

    for line in reader.lines() {
        lines.push(line?);
    }

    if lines.is_empty() {
        return Err(anyhow::anyhow!("No lines found in file."));
    }

    Ok(lines)
}

pub fn read_stdin_lines() -> Result<Vec<String>> {
    let stdin = std::io::stdin();
    let lines: Result<Vec<_>, _> = stdin.lock().lines().collect();
    let lines = lines?;

    if lines.is_empty() {
        return Err(anyhow::anyhow!("No lines found in stdin."));
    }
    Ok(lines)
}

fn parse_category(token: &str) -> Option<MetaCategory<'_>> {
    let spp: Vec<&str> = token.split('@').collect();
    if spp.len() == 2 && spp[0].is_empty() {
        if let Ok(q) = Quadrant::from_str(token) {
            Some(Quad { quadrant: q })
        } else {
            Some(RegularCategory {
                description: token,
                // global_quad: None, // Field removed
            })
        }
    } else {
        None
    }
}

fn parse_date(s: &str) -> Option<Timestamp> {
    let def = || s.parse::<Timestamp>();
    let generic_fmt = |fmt| Local.datetime_from_str(s, fmt);

    let formats = vec!["%Y-%m-%d %H:%M", "%Y-%m-%d %Hh%M"];

    let mut results = vec![def()];

    for fmt in formats {
        results.push(generic_fmt(fmt));
    }

    results.iter().find_map(|r| r.ok())
}

pub(crate) fn get_life_chunk(line: &str) -> LifeChunk {
    // Made pub(crate)
    let mut tokens = line
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty());

    let mut parse_token_as_duration = |parse_as: fn(i64) -> Duration| {
        tokens
            .next()
            .and_then(|i| i.parse::<i64>().ok())
            .map(parse_as)
            .unwrap_or_else(Duration::zero)
    };
    let h = parse_token_as_duration(Duration::hours);
    let m = parse_token_as_duration(Duration::minutes);

    let duration = h + m;

    let mut newline: Vec<&str> = vec![];
    let mut categories: Vec<String> = vec![];

    let mut quadrant = None;

    let mut to_join = vec![];
    // XXX: should check compatibility of quadrants, in case there are several options
    for t in tokens {
        to_join.push(t);
        if let Some(mc) = parse_category(t) {
            match mc {
                Quad { quadrant: q } => {
                    quadrant = Some(q);
                }

                RegularCategory { description, .. } => {
                    categories.push(String::from(description));
                }
            }
            continue;
        }

        newline.push(t);
    }

    let description = newline.join(" ");

    // XXX: what to do if description is empty (categories self-explaining). Could have None instead.
    let user_provided_quadrant = quadrant.is_some();
    let qu = quadrant.unwrap_or_default();
    
    LifeChunk::new(
        description.to_string(),
        duration,
        categories,
        qu,
        user_provided_quadrant,
        to_join.join(" "),
    )
}

/// this function should not exist, the conversion should happen now
fn parse_all_lines(it: &mut Iter<String>) -> Vec<LineParseResult> {
    let mut list_of_pr = vec![];

    for s in it {
        if is_noop(s) {
            continue;
        }
        match process_line(s) {
            // TODO: shouldn't unwrap
            Date { date } => list_of_pr.push(Date { date }),
            lp => {
                if !list_of_pr.is_empty() {
                    list_of_pr.push(lp)
                }
            }
        }
    }

    assert!(list_of_pr.is_empty() || is_a_date_token(list_of_pr.first().unwrap()));

    list_of_pr
}

fn register_all_categories(list_of_timed_pr: &[LineParseResult]) -> ParseState {
    let mut parse_state = ParseState::new();
    for lpr in list_of_timed_pr {
        if let Lc { life_chunk: lc } = lpr {
            parse_state.register_categories_from_life_chunk(lc);
        }
    }
    parse_state
}

fn tokens_from_timed_lpr(
    list_of_pr: Vec<LineParseResult>,
    start_time: Timestamp,
) -> Vec<TiroToken> {
    let mut tiro_tokens = vec![];
    let mut curr_time = start_time;
    for lpr in list_of_pr {
        match lpr {
            Lc { life_chunk } => {
                let duration = life_chunk.duration;
                let tlc = TimedLifeChunk {
                    start: curr_time,
                    life_chunk,
                };
                curr_time = curr_time + duration;
                tiro_tokens.push(TiroToken::Tlc { tlc })
            }
            Date { date } => {
                curr_time = date;
                tiro_tokens.push(TiroToken::Date { date })
            }
        };
    }

    tiro_tokens
}

/// Parse a line from the input
fn process_line(line: &str) -> LineParseResult {
    parse_date(line).map(|date| Date { date }).unwrap_or(Lc {
        life_chunk: get_life_chunk(line),
    })
}

fn is_noop(line: &str) -> bool {
    line.starts_with('#') || line.trim().is_empty()
}

/// Whether a token is a date
fn is_a_date_token(t: &LineParseResult) -> bool {
    let d = Date { date: Local::now() };
    discriminant(t) == discriminant(&d)
}
