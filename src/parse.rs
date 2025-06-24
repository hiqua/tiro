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
use crate::{TiroError, TiroResult};

#[cfg(test)]
mod tests {
    use chrono::{Local, TimeZone};
    use time::Duration;

    use crate::config::{Config, Quadrant}; // Added Config
    use crate::parse::{get_life_chunk, parse_date, process_line, LifeChunk, LineParseResult};
    use super::parse_activities; // Added parse_activities

    #[test]
    fn parsing_1() {
        let dt = Local.ymd(2014, 11, 28).and_hms(12, 0, 0);
        assert_eq!(
            Local
                .datetime_from_str("2014-11-28 12:00", "%Y-%m-%d %H:%M")
                .ok(),
            Some(dt)
        );
    }

    #[test]
    fn test_parse_date_custom_format() {
        let dt = Local.ymd(2023, 10, 26).and_hms(14, 30, 0);
        assert_eq!(parse_date("2023-10-26 14h30"), Some((dt, false)));
    }

    #[test]
    fn test_parse_date_invalid_string() {
        assert_eq!(parse_date("invalid-date-string"), None);
    }

    #[test]
    fn test_parse_date_empty_string() {
        assert_eq!(parse_date(""), None);
    }

    #[test]
    fn test_parse_date_date_only() {
        let expected_dt = Local.ymd(2024, 3, 15).and_hms(0, 0, 0);
        assert_eq!(parse_date("2024-03-15"), Some((expected_dt, true)));
    }

    #[test]
    fn test_parse_date_datetime_normal() {
        let expected_dt = Local.ymd(2024, 3, 15).and_hms(10, 30, 0);
        assert_eq!(parse_date("2024-03-15 10:30"), Some((expected_dt, false)));
    }

    #[test]
    fn test_parse_date_datetime_custom_h() {
        let expected_dt = Local.ymd(2024, 3, 15).and_hms(11, 20, 0);
        assert_eq!(parse_date("2024-03-15 11h20"), Some((expected_dt, false)));
    }

    // Tests for get_life_chunk
    #[test]
    fn test_get_life_chunk_full_input() {
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
    fn test_get_life_chunk_missing_duration() {
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
    fn test_get_life_chunk_missing_categories() {
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
    fn test_get_life_chunk_user_quadrant() {
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
    fn test_get_life_chunk_default_quadrant() {
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
    fn test_process_line_date() {
        let line = "2024-03-10 10:00";
        let expected_date = Local.ymd(2024, 3, 10).and_hms(10, 0, 0);
        match process_line(line) {
            LineParseResult::Date { date, is_date_only } => {
                assert_eq!(date, expected_date);
                assert!(!is_date_only, "Expected is_date_only to be false for full timestamp");
            }
            _ => panic!("Expected LineParseResult::Date"),
        }
    }

    #[test]
    fn test_process_line_date_only_header() {
        let line = "2024-03-11";
        let expected_date = Local.ymd(2024, 3, 11).and_hms(0, 0, 0);
        match process_line(line) {
            LineParseResult::Date { date, is_date_only } => {
                assert_eq!(date, expected_date);
                assert!(is_date_only, "Expected is_date_only to be true for date-only header");
            }
            _ => panic!("Expected LineParseResult::Date for date-only header"),
        }
    }

    #[test]
    fn test_process_line_life_chunk() {
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
    fn test_process_line_comment() {
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
    fn test_process_line_empty_string() {
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
    fn test_parse_activities_timeless_simple() {
        let lines = vec![
            "2024-03-12".to_string(),
            "0 30 Timeless Activity 1 @TestCat".to_string(),
            "1 15 Timeless Activity 2 @TestCat @AnotherCat".to_string(),
        ];
        let config = Config::default();
        let life_lapses = parse_activities(lines.iter(), &config);

        assert_eq!(life_lapses.len(), 1, "Should produce one LifeLapse");
        let lapse = &life_lapses[0];
        let expected_start_time = Local.ymd(2024, 3, 12).and_hms(0, 0, 0);
        assert_eq!(lapse.start(), expected_start_time, "LifeLapse start time should be midnight");

        let tokens = lapse.tokens_as_ref();
        assert_eq!(tokens.len(), 2, "LifeLapse should have two tokens");

        // Token 1
        assert_eq!(tokens[0].start, expected_start_time, "Token 1 start time");
        assert_eq!(tokens[0].life_chunk.description, "Timeless Activity 1");
        assert_eq!(tokens[0].life_chunk.duration, Duration::minutes(30));
        assert_eq!(tokens[0].life_chunk.categories, vec!["@TestCat".to_string()]);

        // Token 2
        assert_eq!(tokens[1].start, expected_start_time, "Token 2 start time"); // Still midnight
        assert_eq!(tokens[1].life_chunk.description, "Timeless Activity 2");
        assert_eq!(tokens[1].life_chunk.duration, Duration::hours(1) + Duration::minutes(15));
        assert_eq!(tokens[1].life_chunk.categories, vec!["@TestCat".to_string(), "@AnotherCat".to_string()]);

        let expected_total_duration = Duration::minutes(30) + Duration::hours(1) + Duration::minutes(15);
        assert_eq!(lapse.total_duration(), expected_total_duration, "LifeLapse total duration");
        assert_eq!(lapse.end, expected_start_time + expected_total_duration, "LifeLapse end time");
    }

    #[test]
    fn test_parse_activities_mixed_timed_and_timeless() {
        let lines = vec![
            "2024-03-13 10:00".to_string(), // Timed block starts
            "1 0 Timed Activity 1 @Work".to_string(),      // Ends 11:00
            "0 30 Timed Activity 2 @Work".to_string(),      // Ends 11:30
            "2024-03-13".to_string(),       // Timeless block for the same day
            "0 20 Timeless A @Play".to_string(),
            "0 25 Timeless B @Play".to_string(),
            "2024-03-13 14:00".to_string(), // New timed block
            "2 0 Timed Activity 3 @Work".to_string(),      // Ends 16:00
        ];
        let config = Config::default();
        let life_lapses = parse_activities(lines.iter(), &config);

        assert_eq!(life_lapses.len(), 3, "Should produce three LifeLapses");

        // First LifeLapse (Timed)
        let lapse1 = &life_lapses[0];
        let expected_start1 = Local.ymd(2024, 3, 13).and_hms(10, 0, 0);
        assert_eq!(lapse1.start(), expected_start1);
        assert_eq!(lapse1.tokens_as_ref().len(), 2);
        assert_eq!(lapse1.tokens_as_ref()[0].start, expected_start1);
        assert_eq!(lapse1.tokens_as_ref()[0].life_chunk.duration, Duration::hours(1));
        assert_eq!(lapse1.tokens_as_ref()[1].start, expected_start1 + Duration::hours(1));
        assert_eq!(lapse1.tokens_as_ref()[1].life_chunk.duration, Duration::minutes(30));
        assert_eq!(lapse1.end, expected_start1 + Duration::hours(1) + Duration::minutes(30));

        // Second LifeLapse (Timeless)
        let lapse2 = &life_lapses[1];
        let expected_start_timeless = Local.ymd(2024, 3, 13).and_hms(0, 0, 0);
        assert_eq!(lapse2.start(), expected_start_timeless);
        assert_eq!(lapse2.tokens_as_ref().len(), 2);
        assert_eq!(lapse2.tokens_as_ref()[0].start, expected_start_timeless); // Timeless start
        assert_eq!(lapse2.tokens_as_ref()[0].life_chunk.duration, Duration::minutes(20));
        assert_eq!(lapse2.tokens_as_ref()[1].start, expected_start_timeless); // Timeless start
        assert_eq!(lapse2.tokens_as_ref()[1].life_chunk.duration, Duration::minutes(25));
        assert_eq!(lapse2.end, expected_start_timeless + Duration::minutes(20) + Duration::minutes(25));

        // Third LifeLapse (Timed)
        let lapse3 = &life_lapses[2];
        let expected_start3 = Local.ymd(2024, 3, 13).and_hms(14, 0, 0);
        assert_eq!(lapse3.start(), expected_start3);
        assert_eq!(lapse3.tokens_as_ref().len(), 1);
        assert_eq!(lapse3.tokens_as_ref()[0].start, expected_start3);
        assert_eq!(lapse3.tokens_as_ref()[0].life_chunk.duration, Duration::hours(2));
        assert_eq!(lapse3.end, expected_start3 + Duration::hours(2));
    }

    #[test]
    fn test_parse_activities_timeless_then_timed() {
        let lines = vec![
            "2024-03-14".to_string(),       // Timeless block
            "0 10 Timeless X @Errands".to_string(),
            "2024-03-14 09:00".to_string(), // Timed block for the same day
            "1 30 Timed Y @Project".to_string(),     // Ends 10:30
        ];
        let config = Config::default();
        let life_lapses = parse_activities(lines.iter(), &config);

        assert_eq!(life_lapses.len(), 2, "Should produce two LifeLapses");

        // First LifeLapse (Timeless)
        let lapse1 = &life_lapses[0];
        let expected_start_timeless = Local.ymd(2024, 3, 14).and_hms(0, 0, 0);
        assert_eq!(lapse1.start(), expected_start_timeless);
        assert_eq!(lapse1.tokens_as_ref().len(), 1);
        assert_eq!(lapse1.tokens_as_ref()[0].start, expected_start_timeless);
        assert_eq!(lapse1.tokens_as_ref()[0].life_chunk.duration, Duration::minutes(10));
        assert_eq!(lapse1.end, expected_start_timeless + Duration::minutes(10));

        // Second LifeLapse (Timed)
        let lapse2 = &life_lapses[1];
        let expected_start_timed = Local.ymd(2024, 3, 14).and_hms(9, 0, 0);
        assert_eq!(lapse2.start(), expected_start_timed);
        assert_eq!(lapse2.tokens_as_ref().len(), 1);
        assert_eq!(lapse2.tokens_as_ref()[0].start, expected_start_timed);
        assert_eq!(lapse2.tokens_as_ref()[0].life_chunk.duration, Duration::hours(1) + Duration::minutes(30));
        assert_eq!(lapse2.end, expected_start_timed + Duration::hours(1) + Duration::minutes(30));
    }

    #[test]
    fn test_parse_activities_only_header() {
        let lines = vec!["2024-03-15".to_string()];
        let config = Config::default();
        let life_lapses = parse_activities(lines.iter(), &config);

        assert_eq!(life_lapses.len(), 1, "Should produce one LifeLapse even with only a header");
        let lapse = &life_lapses[0];
        let expected_start_time = Local.ymd(2024, 3, 15).and_hms(0, 0, 0);
        assert_eq!(lapse.start(), expected_start_time, "LifeLapse start time should be midnight");
        assert!(lapse.tokens_as_ref().is_empty(), "LifeLapse should have no tokens");
        assert_eq!(lapse.total_duration(), Duration::zero(), "LifeLapse total duration should be zero");
        assert_eq!(lapse.end, expected_start_time, "LifeLapse end time should be same as start");
    }
}

impl From<ParseIntError> for TiroError {
    fn from(e: ParseIntError) -> Self {
        TiroError { e: e.to_string() }
    }
}

/// A continuous series of life chunks.
///
/// Invariant: `start == tokens[0].start`.
/// Invariant: end - start == sum(tok.duration for tok in tokens)
/// XXX: enforce these by making the fields private and using methods
#[derive(Clone, Debug)]
pub struct LifeLapse {
    start: Timestamp,
    end: Timestamp,
    tokens: Vec<TimedLifeChunk>,
}

impl LifeLapse {
    pub(crate) fn new(start: Timestamp) -> LifeLapse {
        LifeLapse {
            start,
            end: start,
            tokens: vec![],
        }
    }

    fn total_duration(&self) -> Duration {
        self.tokens
            .iter()
            .fold(Duration::hours(0), |sum, t| sum.add(t.life_chunk.duration))
    }

    pub fn extend<I: IntoIterator<Item = TimedLifeChunk>>(&mut self, iter: I) {
        self.tokens.extend(iter);
        let d = self.total_duration();
        self.end = self.start + d;
    }

    fn push(&mut self, item: TimedLifeChunk) {
        self.end = self.end + item.life_chunk.duration;
        self.tokens.push(item);
    }

    pub fn tokens(self) -> Vec<TimedLifeChunk> {
        self.tokens
    }

    pub fn tokens_as_ref(&self) -> &Vec<TimedLifeChunk> {
        &self.tokens
    }

    pub fn start(&self) -> Timestamp {
        self.start
    }

    pub fn is_right_before(&self, other: &LifeLapse) -> bool {
        self.end == other.start
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

#[derive(Clone)]
pub enum TiroToken {
    Tlc { tlc: TimedLifeChunk },
    Date { date: Timestamp },
}

#[derive(Clone, Debug)]
pub struct TimedLifeChunk {
    pub start: Timestamp,
    pub life_chunk: LifeChunk,
}

#[derive(Clone)]
pub enum LineParseResult {
    Lc { life_chunk: LifeChunk },
    Date { date: Timestamp, is_date_only: bool },
}

#[derive(Clone, Debug)]
pub struct LifeChunk {
    pub description: String,
    pub duration: Duration,
    pub categories: Vec<String>,
    pub quadrant: Quadrant,
    pub user_provided_quadrant: bool,
    input: String,
}

impl LifeChunk {
    pub(crate) fn get_input(&self) -> &str {
        &self.input
    }
}

/// The starting time is the earli
pub fn get_all_life_lapses(
    all_activities_line: Vec<Vec<String>>,
    config: &Config,
) -> (Timestamp, Vec<LifeLapse>) {
    let mut all_life_lapses = vec![];
    let start_time = {
        for activities_lines in all_activities_line {
            let life_lapses = parse_activities(activities_lines.iter(), config);
            all_life_lapses.extend(life_lapses);
        }
        all_life_lapses.iter().map(|ll| ll.start).min().unwrap()
    };

    all_life_lapses.sort_by_key(|ll| ll.start);

    all_life_lapses = all_life_lapses
        .into_iter()
        .skip_while(|ll| ll.start < start_time)
        // Empty if only contained activities before starting time
        .filter(|ll| !ll.is_empty())
        .collect();

    all_life_lapses = merge_strictly_compatible_lifelapses(all_life_lapses);

    (start_time, all_life_lapses)
}

pub fn parse_activities(mut it: Iter<String>, config: &Config) -> Vec<LifeLapse> {
    let list_of_pr_initial = parse_all_lines(&mut it);

    if list_of_pr_initial.is_empty() {
        return vec![];
    }

    // Process categories based on initial parse results
    let mut parse_state = register_all_categories(&list_of_pr_initial);
    update_parse_state_from_config(config, &mut parse_state).expect("");
    let list_of_pr = parse_state.update_category_quadrants(list_of_pr_initial);

    // After potential modifications by update_category_quadrants,
    // it's theoretically possible list_of_pr is empty if all items were somehow removed.
    // Though current logic of update_category_quadrants doesn't remove items.
    if list_of_pr.is_empty() {
        return vec![];
    }

    // The first element of list_of_pr (which is non-empty here) must be a Date variant,
    // as ensured by parse_all_lines. This date is the effective start_time for tokenization.
    let first_date_in_lpr = if let Some(Date { date, .. }) = list_of_pr.first() {
        *date
    } else {
        // This path should not be taken given the guarantees from parse_all_lines
        // and the non-empty check for list_of_pr.
        unreachable!("list_of_pr is non-empty and should start with a Date.");
    };

    let tiro_tokens = tokens_from_timed_lpr(list_of_pr, first_date_in_lpr);

    // If tiro_tokens is empty, it implies list_of_pr_updated was empty,
    // which should have been caught by the initial list_of_pr.is_empty() check.
    // Or, if tokens_from_timed_lpr could return empty for non-empty input (it shouldn't).
    if tiro_tokens.is_empty() {
        return vec![];
    }

    let mut result = vec![];
    // Initialize current_ll with the date from the *first* TiroToken::Date.
    // We know tiro_tokens is not empty and should start with a Date token.
    let mut current_ll = if let Some(TiroToken::Date { date }) = tiro_tokens.first() {
        LifeLapse::new(*date)
    } else {
        // This indicates a logic error in tokens_from_timed_lpr or assumptions about its output.
        unreachable!("tiro_tokens is non-empty and expected to start with a Date token");
    };

    // Skip the first token as its Date was used to initialize current_ll.
    // Iterate over the rest of the tokens.
    for tok in tiro_tokens.into_iter().skip(1) {
        match tok {
            TiroToken::Tlc { tlc } => {
                current_ll.push(tlc);
            }
            TiroToken::Date { date } => {
                result.push(current_ll); // Push the completed LifeLapse
                current_ll = LifeLapse::new(date); // Start a new one for the new date
            }
        }
    }
    result.push(current_ll); // Push the last LifeLapse (could be the first one if only one Date token)

    result
}

pub fn read_lines_from_file(path: PathBuf) -> TiroResult<Vec<String>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut lines = vec![];

    for line in reader.lines() {
        lines.push(line?);
    }

    if lines.is_empty() {
        return Err(TiroError {
            e: "No lines found in file.".to_string(),
        });
    }

    Ok(lines)
}

pub fn read_stdin_lines() -> TiroResult<Vec<String>> {
    let stdin = std::io::stdin();
    let r: Result<Vec<_>, _> = stdin.lock().lines().collect();

    if let Ok(res) = r {
        if res.is_empty() {
            return Err(TiroError {
                e: "No lines found in file.".to_string(),
            });
        }
        Ok(res)
    } else {
        Err(TiroError {
            e: "Error while reading lines.".to_string(),
        })
    }
}

fn parse_category(token: &str) -> Option<MetaCategory> {
    let spp: Vec<&str> = token.split('@').collect();
    if spp.len() == 2 && spp[0].is_empty() {
        if let Ok(q) = Quadrant::from_str(token) {
            Some(Quad { quadrant: q })
        } else {
            Some(RegularCategory {
                description: token,
                global_quad: None,
            })
        }
    } else {
        None
    }
}

fn parse_date(s: &str) -> Option<(Timestamp, bool)> {
    // Attempt to parse full date-time formats first
    let datetime_formats = vec!["%Y-%m-%d %H:%M", "%Y-%m-%d %Hh%M"];
    for fmt in datetime_formats {
        if let Ok(dt) = Local.datetime_from_str(s, fmt) {
            return Some((dt, false));
        }
    }

    // Attempt to parse s as Timestamp (RFC 3339 / ISO 8601)
    if let Ok(dt) = s.parse::<Timestamp>() {
        return Some((dt, false));
    }

    // Attempt to parse date-only format
    if let Ok(naive_date) = NaiveDate::parse_from_str(s, "%Y-%m-%d") {
        // Convert NaiveDate to NaiveDateTime at midnight, then to local Timestamp
        let naive_datetime = naive_date.and_hms_opt(0, 0, 0).unwrap(); // Midnight
        match Local.from_local_datetime(&naive_datetime) {
            LocalResult::Single(dt) => return Some((dt, true)),
            LocalResult::Ambiguous(dt1, _) => return Some((dt1, true)), // Or handle ambiguity as needed
            LocalResult::None => return None,                           // Invalid local time
        }
    }

    None
}

pub(crate) fn get_life_chunk(line: &str) -> LifeChunk {
    // Made pub(crate)
    let mut tokens = line.split(|c: char| c == ',' || c.is_whitespace());

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
    let qu = quadrant.or_else(|| Some(Default::default())).unwrap();
    LifeChunk {
        description,
        duration,
        categories,
        // XXX: redundant default
        quadrant: qu,
        user_provided_quadrant: quadrant.is_some(),
        input: to_join.join(" "),
    }
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
            // Date { date } => list_of_pr.push(Date { date }),
            // The above line is problematic because we don't know the value of is_date_only here.
            // process_line directly returns the correct LineParseResult variant including is_date_only.
            date_result @ Date { .. } => list_of_pr.push(date_result),
            lc_result @ Lc { .. } => {
                if !list_of_pr.is_empty() {
                    list_of_pr.push(lc_result)
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
    start_time: Timestamp, // This is the timestamp from the very first Date line in the file/input.
) -> Vec<TiroToken> {
    let mut tiro_tokens = vec![];
    let mut curr_time = start_time; // Represents the current time anchor for activities.
                                    // For timed activities, it's the end of the previous activity.
                                    // For timeless activities, it's the start of the day (midnight of the header date).

    // Determine initial state of current_block_is_timeless based on the first item, if it's a Date.
    // This assumes list_of_pr is not empty and starts with a Date, which parse_activities ensures.
    let mut current_block_is_timeless = if let Some(Date { is_date_only, .. }) = list_of_pr.first() {
        *is_date_only
    } else {
        // This case should ideally not be hit if parse_activities ensures the first element is Date.
        // If it can be hit, default to false or handle error.
        false
    };

    for lpr in list_of_pr {
        match lpr {
            Lc { life_chunk } => {
                let activity_start_time = curr_time; // Start time for this specific LC

                let tlc = TimedLifeChunk {
                    start: activity_start_time,
                    life_chunk,
                };
                let duration = tlc.life_chunk.duration; // Get duration before move
                tiro_tokens.push(TiroToken::Tlc { tlc });

                // Only advance curr_time if this block of activities is NOT timeless.
                // If timeless, all activities in this block get the same start time (midnight of header),
                // and their durations do not shift the start of the next one in this block.
                if !current_block_is_timeless {
                    curr_time = activity_start_time + duration; // Use stored duration
                }
                // If current_block_is_timeless, curr_time remains the same (e.g., midnight of the header date)
                // for the next LC in this timeless block.
            }
            Date { date, is_date_only } => {
                curr_time = date; // New anchor time
                current_block_is_timeless = is_date_only; // Update mode for subsequent Lcs
                tiro_tokens.push(TiroToken::Date { date });
            }
        };
    }

    tiro_tokens
}

/// Parse a line from the input
fn process_line(line: &str) -> LineParseResult {
    match parse_date(line) {
        Some((timestamp, is_date_only_flag)) => LineParseResult::Date {
            date: timestamp,
            is_date_only: is_date_only_flag,
        },
        None => LineParseResult::Lc {
            life_chunk: get_life_chunk(line),
        },
    }
}

fn is_noop(line: &str) -> bool {
    line.starts_with('#') || line.trim().is_empty()
}

/// Whether a token is a date
fn is_a_date_token(t: &LineParseResult) -> bool {
    let d = Date { date: Local::now(), is_date_only: false }; // Value of is_date_only doesn't affect discriminant
    discriminant(t) == discriminant(&d)
}
