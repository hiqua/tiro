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

    use chrono::NaiveTime; // Added for new tests
    use crate::config::{Config, Quadrant}; // Added Config
    // Added LifeLapse, parse_activities for new tests
    use crate::parse::{get_life_chunk, parse_date, process_line, LifeChunk, LineParseResult, LifeLapse, parse_activities};

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
        assert_eq!(parse_date("2023-10-26 14h30"), Some(dt));
    }

    #[test]
    fn test_parse_date_invalid_string() {
        assert_eq!(parse_date("invalid-date-string"), None);
    }

    #[test]
    fn test_parse_date_empty_string() {
        assert_eq!(parse_date(""), None);
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
        assert_eq!(lc.description, "그냥 프로젝트 작업"); // Expect full line as description
        assert_eq!(lc.duration, Duration::zero());
        assert_eq!(lc.categories, vec!["@Dev".to_string()]);
        assert_eq!(lc.quadrant, Quadrant::default());
        assert!(!lc.user_provided_quadrant);
        assert_eq!(lc.get_input(), "그냥 프로젝트 작업 @Dev"); // Expect full non-prefix line as input part
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

    #[test]
    fn test_get_life_chunk_end_time_basic() {
        let line = ">17:00 Meeting @Work";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Meeting");
        assert_eq!(lc.duration, Duration::zero()); // Duration is calculated later
        assert!(lc.is_end_time_specified);
        assert_eq!(lc.end_time, Some(NaiveTime::from_hms(17, 0, 0)));
        assert_eq!(lc.categories, vec!["@Work".to_string()]);
        assert_eq!(lc.get_input(), "Meeting @Work");
    }

    #[test]
    fn test_get_life_chunk_end_time_with_category_first() {
        let line = ">09:30 @MorningRoutine Breakfast";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Breakfast"); // Category should not be part of desc if parsed
        assert!(lc.is_end_time_specified);
        assert_eq!(lc.end_time, Some(NaiveTime::from_hms(9, 30, 0)));
        assert_eq!(lc.categories, vec!["@MorningRoutine".to_string()]);
        assert_eq!(lc.get_input(), "@MorningRoutine Breakfast"); // Input is post-time-spec
    }

    #[test]
    fn test_get_life_chunk_end_time_no_description() {
        let line = ">23:00 @Evening";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "");
        assert!(lc.is_end_time_specified);
        assert_eq!(lc.end_time, Some(NaiveTime::from_hms(23, 0, 0)));
        assert_eq!(lc.categories, vec!["@Evening".to_string()]);
        assert_eq!(lc.get_input(), "@Evening");
    }

    #[test]
    fn test_get_life_chunk_end_time_invalid_format() {
        let line = ">1700 Meeting @Work"; // Invalid time format
        let lc = get_life_chunk(line);
        // Should be treated as part of description because time parsing fails
        assert_eq!(lc.description, ">1700 Meeting");
        assert!(!lc.is_end_time_specified);
        assert_eq!(lc.end_time, None);
        assert_eq!(lc.duration, Duration::zero()); // No valid H M duration parsed either
        assert_eq!(lc.categories, vec!["@Work".to_string()]);
        assert_eq!(lc.get_input(), ">1700 Meeting @Work"); // Full line becomes input part
    }

    #[test]
    fn test_get_life_chunk_end_time_just_arrow() {
        let line = "> Meeting @Work"; // Just arrow, no time
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "> Meeting");
        assert!(!lc.is_end_time_specified);
        assert_eq!(lc.end_time, None);
        assert_eq!(lc.duration, Duration::zero());
        assert_eq!(lc.categories, vec!["@Work".to_string()]);
        assert_eq!(lc.get_input(), "> Meeting @Work");
    }

    #[test]
    fn test_get_life_chunk_standard_duration_still_works() { // Renamed for clarity
        let line = "1 15 Coding @Dev";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Coding");
        assert_eq!(lc.duration, Duration::hours(1) + Duration::minutes(15));
        assert!(!lc.is_end_time_specified);
        assert_eq!(lc.end_time, None);
        assert_eq!(lc.categories, vec!["@Dev".to_string()]);
        assert_eq!(lc.get_input(), "Coding @Dev");
    }

    #[test]
    fn test_get_life_chunk_only_description_no_time_spec() { // Renamed for clarity
        let line = "Simple task";
        let lc = get_life_chunk(line);
        assert_eq!(lc.description, "Simple task");
        assert_eq!(lc.duration, Duration::zero());
        assert!(!lc.is_end_time_specified);
        assert_eq!(lc.end_time, None);
        assert_eq!(lc.categories, Vec::<String>::new());
        assert_eq!(lc.get_input(), "Simple task");
    }


    // Tests for process_line
    #[test]
    fn test_process_line_date() {
        let line = "2024-03-10 10:00";
        let expected_date = Local.ymd(2024, 3, 10).and_hms(10, 0, 0);
        match process_line(line) {
            LineParseResult::Date { date } => assert_eq!(date, expected_date),
            _ => panic!("Expected LineParseResult::Date"),
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
                // If a comment line is fed directly to process_line (bypassing is_noop from parse_all_lines),
                // get_life_chunk will treat '#' as part of the description if not parsed as duration/time.
                assert_eq!(lc.description, "# This is a comment");
                assert_eq!(lc.duration, Duration::zero());
                assert_eq!(lc.categories, Vec::<String>::new());
                assert_eq!(lc.quadrant, Quadrant::default());
                assert!(!lc.user_provided_quadrant);
                assert_eq!(lc.get_input(), "# This is a comment");
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

    // Helper function for testing parse_activities
    fn run_parse_activities_test(lines: Vec<String>, config: &Config) -> Vec<LifeLapse> {
        parse_activities(lines.iter(), config)
    }

    #[test]
    fn test_duration_calculation_same_day() {
        let config = Config::default();
        let lines = vec![
            "2024-03-10 10:00".to_string(),
            "1 0 Activity 1".to_string(),      // Ends at 11:00
            ">12:30 Activity 2".to_string(), // Should be 1h 30m
        ];
        let lapses = run_parse_activities_test(lines, &config);
        assert_eq!(lapses.len(), 1);
        let tokens = lapses[0].tokens_as_ref();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[1].life_chunk.duration, Duration::hours(1) + Duration::minutes(30));
        assert_eq!(tokens[1].start, Local.ymd(2024, 3, 10).and_hms(11, 0, 0));
    }

    #[test]
    fn test_duration_calculation_overnight() {
        let config = Config::default();
        let lines = vec![
            "2024-03-10 23:00".to_string(), // Start late
            "1 0 Activity 1".to_string(),      // Ends at 2024-03-11 00:00
            ">02:30 Activity 2".to_string(), // Should be 2h 30m, ends at 2024-03-11 02:30
        ];
        let lapses = run_parse_activities_test(lines, &config);
        assert_eq!(lapses.len(), 1);
        let tokens = lapses[0].tokens_as_ref();
        assert_eq!(tokens.len(), 2);
        let activity2_start_expected = Local.ymd(2024, 3, 11).and_hms(0, 0, 0);
        assert_eq!(tokens[1].start, activity2_start_expected);
        assert_eq!(tokens[1].life_chunk.duration, Duration::hours(2) + Duration::minutes(30));
    }

    #[test]
    fn test_duration_calculation_activity_ends_at_midnight() {
        let config = Config::default();
        let lines = vec![
            "2024-03-10 22:30".to_string(),
            ">00:00 Activity 1".to_string(), // Ends at midnight, duration 1h 30m
        ];
        let lapses = run_parse_activities_test(lines, &config);
        assert_eq!(lapses.len(), 1);
        let tokens = lapses[0].tokens_as_ref();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].life_chunk.duration, Duration::hours(1) + Duration::minutes(30));
        assert_eq!(tokens[0].start, Local.ymd(2024, 3, 10).and_hms(22, 30, 0));
    }

    #[test]
    fn test_mixed_duration_and_endtime_inputs() {
        let config = Config::default();
        let lines = vec![
            "2024-07-01 09:00".to_string(),
            "0 30 Meeting 1".to_string(),    // Ends 09:30
            ">11:00 Project Work".to_string(), // Ends 11:00, duration 1h 30m
            "1 0 Lunch".to_string(),         // Ends 12:00, duration 1h
            ">12:15 Quick Sync".to_string(),  // Ends 12:15, duration 15m
        ];
        let lapses = run_parse_activities_test(lines, &config);
        assert_eq!(lapses.len(), 1);
        let tokens = lapses[0].tokens_as_ref();
        assert_eq!(tokens.len(), 4);

        assert_eq!(tokens[0].life_chunk.description, "Meeting 1");
        assert_eq!(tokens[0].life_chunk.duration, Duration::minutes(30));
        assert_eq!(tokens[0].start, Local.ymd(2024, 7, 1).and_hms(9, 0, 0));

        assert_eq!(tokens[1].life_chunk.description, "Project Work");
        assert_eq!(tokens[1].life_chunk.duration, Duration::hours(1) + Duration::minutes(30));
        assert_eq!(tokens[1].start, Local.ymd(2024, 7, 1).and_hms(9, 30, 0));

        assert_eq!(tokens[2].life_chunk.description, "Lunch");
        assert_eq!(tokens[2].life_chunk.duration, Duration::hours(1));
        assert_eq!(tokens[2].start, Local.ymd(2024, 7, 1).and_hms(11, 0, 0));

        assert_eq!(tokens[3].life_chunk.description, "Quick Sync");
        assert_eq!(tokens[3].life_chunk.duration, Duration::minutes(15));
        assert_eq!(tokens[3].start, Local.ymd(2024, 7, 1).and_hms(12, 0, 0));
    }

    #[test]
    fn test_successive_endtime_inputs() {
        let config = Config::default();
        let lines = vec![
            "2024-07-01 14:00".to_string(),
            ">14:45 Task A".to_string(),    // Ends 14:45, duration 45m
            ">15:15 Task B".to_string(),    // Ends 15:15, duration 30m
        ];
        let lapses = run_parse_activities_test(lines, &config);
        assert_eq!(lapses.len(), 1);
        let tokens = lapses[0].tokens_as_ref();
        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0].life_chunk.description, "Task A");
        assert_eq!(tokens[0].life_chunk.duration, Duration::minutes(45));
        assert_eq!(tokens[0].start, Local.ymd(2024, 7, 1).and_hms(14, 0, 0));

        assert_eq!(tokens[1].life_chunk.description, "Task B");
        assert_eq!(tokens[1].life_chunk.duration, Duration::minutes(30));
        assert_eq!(tokens[1].start, Local.ymd(2024, 7, 1).and_hms(14, 45, 0));
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
    Date { date: Timestamp },
}

use chrono::NaiveTime; // Ensure NaiveTime is imported

#[derive(Clone, Debug)]
pub struct LifeChunk {
    pub description: String,
    pub duration: Duration,
    pub categories: Vec<String>,
    pub quadrant: Quadrant,
    pub user_provided_quadrant: bool,
    input: String,
    pub end_time: Option<NaiveTime>, // Added
    pub is_end_time_specified: bool, // Added
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

    let mut result = vec![];
    let mut current_ll = LifeLapse::new(start_time); // Initialized with the definite start time.
    let mut first_date_token_processed = false;

    // The first token in tiro_tokens should always be a Date token corresponding to start_time
    // if list_of_pr was not empty and started with a Date.

    for tok in tiro_tokens {
        match tok {
            TiroToken::Tlc { tlc } => {
                current_ll.push(tlc);
            }
            TiroToken::Date { date } => {
                if !first_date_token_processed {
                    // This is the first Date token, it usually matches the initial start_time.
                    // Re-initialize current_ll to ensure it's clean and uses this specific date object.
                    current_ll = LifeLapse::new(date);
                    first_date_token_processed = true;
                } else {
                    // This is a subsequent Date token, indicating a new day or explicit time set.
                    // The previous LifeLapse is complete.
                    if !current_ll.is_empty() { // Avoid pushing empty lapses
                        result.push(current_ll);
                    }
                    current_ll = LifeLapse::new(date);
                }
            }
        }
    }

    // Push the last LifeLapse if it contains any tokens.
    if !current_ll.is_empty() {
        result.push(current_ll);
    }

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
    let tokens_iter = line.split(|c: char| c == ',' || c.is_whitespace()); // Removed mut
    // Filter out empty strings that can result from multiple spaces
    let tokens: Vec<&str> = tokens_iter.filter(|s| !s.is_empty()).collect();

    let mut duration = Duration::zero();
    let mut end_time: Option<NaiveTime> = None;
    let mut is_end_time_specified = false;
    let mut description_offset = 0; // Number of tokens consumed for duration/time

    if !tokens.is_empty() {
        let first_token = tokens[0];
        if first_token.starts_with('>') {
            if first_token.len() > 1 { // Ensure there's something after '>' e.g. ">10:00"
                let time_str = &first_token[1..];
                if let Ok(parsed_time) = NaiveTime::parse_from_str(time_str, "%H:%M") {
                    end_time = Some(parsed_time);
                    is_end_time_specified = true;
                    // duration remains Duration::zero(), will be calculated in tokens_from_timed_lpr
                    description_offset = 1;
                } else {
                    // Token starts with '>' but is not a valid time (e.g., ">foo", ">123"). Treat as part of description.
                    // description_offset remains 0. duration, end_time, is_end_time_specified remain default.
                }
            }
            // If token is just ">", it's part of description. description_offset remains 0.
        } else {
            // Try to parse as H M duration
            let mut h_opt: Option<i64> = None;
            let mut m_opt: Option<i64> = None;
            let mut current_offset_for_duration = 0;

            if let Some(token_val_h) = tokens.get(0) {
                if let Ok(h_val) = token_val_h.parse::<i64>() {
                    h_opt = Some(h_val);
                    current_offset_for_duration += 1;
                    if let Some(token_val_m) = tokens.get(1) {
                        if let Ok(m_val) = token_val_m.parse::<i64>() {
                            m_opt = Some(m_val);
                            current_offset_for_duration += 1;
                        }
                        // If the second token exists but is not a number, m_opt remains None.
                        // current_offset_for_duration is already 1 (for hours).
                    }
                }
            }

            if h_opt.is_some() || m_opt.is_some() { // If at least hours or minutes were successfully parsed
                 duration = Duration::hours(h_opt.unwrap_or(0)) + Duration::minutes(m_opt.unwrap_or(0));
                 description_offset = current_offset_for_duration;
                 // is_end_time_specified remains false, end_time remains None
            }
            // If neither h_opt nor m_opt is Some, all fields remain at their initial values (duration=0, offset=0 etc.)
        }
    }

    // Adjust tokens slice to get remaining parts for description and categories
    let remaining_tokens = if tokens.len() >= description_offset {
        &tokens[description_offset..]
    } else {
        &[] // Should not happen if tokens.len() >= description_offset is proper
    };

    let mut newline: Vec<&str> = vec![];
    let mut categories: Vec<String> = vec![];

    let mut quadrant = None;

    let mut to_join = vec![]; // This will be for the 'input' field
    // XXX: should check compatibility of quadrants, in case there are several options
    for &t in remaining_tokens { // Iterate by reference, t is &str
        to_join.push(t); // t is &str
        if let Some(mc) = parse_category(t) { // t is &str
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
        duration, // This is the parsed H M duration, or Duration::zero() if >HH:MM was parsed
        categories,
        quadrant: qu,
        user_provided_quadrant: quadrant.is_some(),
        input: to_join.join(" "),
        end_time, // Use the local variable `end_time`
        is_end_time_specified, // Use the local variable `is_end_time_specified`
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
            Lc { mut life_chunk } => { // Make life_chunk mutable
                let actual_duration = if life_chunk.is_end_time_specified {
                    if let Some(end_naive_time) = life_chunk.end_time {
                        let start_naive_time = curr_time.time();
                        let duration_std = if end_naive_time >= start_naive_time {
                            // Same day
                            end_naive_time.signed_duration_since(start_naive_time)
                        } else {
                            // Overnight case
                            // Combine current date with end_naive_time for today, then add 1 day to this end_datetime
                            let end_datetime_on_current_date = curr_time.date().and_time(end_naive_time);
                            match end_datetime_on_current_date {
                                Some(edt_today) => {
                                    let end_datetime_tomorrow = edt_today + chrono::Duration::days(1);
                                    end_datetime_tomorrow.signed_duration_since(curr_time)
                                }
                                None => {
                                    // This case should ideally not be reached if NaiveTime is valid.
                                    // Fallback to zero duration if date combination fails.
                                    chrono::Duration::zero()
                                }
                            }
                        };
                        // Convert chrono::Duration to time::Duration
                        Duration::seconds(duration_std.num_seconds())
                    } else {
                        // This case implies is_end_time_specified is true, but end_time is None.
                        // Should not happen with correct parsing in get_life_chunk. Fallback to parsed duration (likely zero).
                        life_chunk.duration
                    }
                } else {
                    // Not end-time specified, use duration parsed by get_life_chunk (which is H M or zero)
                    life_chunk.duration
                };

                life_chunk.duration = actual_duration; // Update life_chunk with calculated or original duration

                let tlc = TimedLifeChunk {
                    start: curr_time,
                    life_chunk, // life_chunk now has the correct duration
                };
                curr_time = curr_time + actual_duration; // Advance curr_time by the actual_duration
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
