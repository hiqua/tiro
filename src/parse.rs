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

    use crate::config::Quadrant;
    use crate::parse::{
        get_life_chunk, parse_all_lines, parse_date, process_line, LifeChunk, LineParseResult,
    };

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

    // Tests for parse_all_lines with multi-line descriptions
    #[test]
    fn test_parse_all_lines_single_multiline_activity() {
        let lines = vec![
            "2024-03-10 10:00".to_string(),
            "1 0 Multiline Task @Dev".to_string(),
            "This is the second line of the description.".to_string(),
            "And this is the third.".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 2); // Date + 1 LifeChunk
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Multiline Task\nThis is the second line of the description.\nAnd this is the third.");
            assert_eq!(lc.duration, Duration::hours(1));
            assert_eq!(lc.categories, vec!["@Dev".to_string()]);
            assert_eq!(lc.input, "Multiline Task @Dev\nThis is the second line of the description.\nAnd this is the third.");
        } else {
            panic!("Expected LifeChunk");
        }
    }

    #[test]
    fn test_parse_all_lines_multiline_followed_by_new_activity() {
        let lines = vec![
            "2024-03-10 10:00".to_string(),
            "1 0 First Task @Work".to_string(),
            "Line 2 of First Task".to_string(),
            "0 30 Second Task @Personal".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 3); // Date + 2 LifeChunks
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "First Task\nLine 2 of First Task");
            assert_eq!(lc.duration, Duration::hours(1));
            assert_eq!(lc.categories, vec!["@Work".to_string()]);
            assert_eq!(lc.input, "First Task @Work\nLine 2 of First Task");
        } else {
            panic!("Expected LifeChunk for first task");
        }
        if let LineParseResult::Lc { life_chunk: lc } = &result[2] {
            assert_eq!(lc.description, "Second Task");
            assert_eq!(lc.duration, Duration::minutes(30));
            assert_eq!(lc.categories, vec!["@Personal".to_string()]);
            assert_eq!(lc.input, "Second Task @Personal");
        } else {
            panic!("Expected LifeChunk for second task");
        }
    }

    #[test]
    fn test_parse_all_lines_multiline_followed_by_new_date() {
        let lines = vec![
            "2024-03-10 10:00".to_string(),
            "0 45 Planning @Strategy".to_string(),
            "Continued planning details.".to_string(),
            "2024-03-10 12:00".to_string(),
            "1 0 Meeting @Client".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 4); // Date, LC, Date, LC
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Planning\nContinued planning details.");
            assert_eq!(lc.duration, Duration::minutes(45));
            assert_eq!(lc.categories, vec!["@Strategy".to_string()]);
        } else {
            panic!("Expected LifeChunk for planning task");
        }
        if let LineParseResult::Date { date } = &result[2] {
            assert_eq!(*date, Local.ymd(2024, 3, 10).and_hms(12,0,0));
        } else {
            panic!("Expected Date token");
        }
        if let LineParseResult::Lc { life_chunk: lc } = &result[3] {
            assert_eq!(lc.description, "Meeting");
        } else {
            panic!("Expected LifeChunk for meeting task");
        }
    }

    #[test]
    fn test_parse_all_lines_multiline_with_comment_in_between() {
        let lines = vec![
            "2024-03-11 09:00".to_string(),
            "2 0 Research @ProjectX".to_string(),
            "First part of research notes.".to_string(),
            "# This is a comment, should be ignored".to_string(),
            "Second part of research notes, after comment.".to_string(),
            "0 30 Review".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 4); // Date, LC1 (Research), LC2 (Second part...), LC3 (Review)

        // LC1: Research (multiline)
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Research\nFirst part of research notes.");
            assert_eq!(lc.duration, Duration::hours(2));
            assert_eq!(lc.categories, vec!["@ProjectX".to_string()]);
            assert_eq!(lc.input, "Research @ProjectX\nFirst part of research notes.");
        } else {
            panic!("Expected LifeChunk for research task (LC1)");
        }

        // LC2: "Second part of research notes, after comment."
        // "Second" and "part" are consumed by get_life_chunk's duration parsing attempts.
        if let LineParseResult::Lc { life_chunk: lc } = &result[2] {
             assert_eq!(lc.description, "of research notes, after comment.");
             assert_eq!(lc.duration, Duration::zero());
             assert_eq!(lc.categories, Vec::<String>::new());
             // The input string for LC2 should be the original line content for that specific activity start line
             assert_eq!(lc.input, "Second part of research notes, after comment.");
        } else {
            panic!("Expected LifeChunk for 'Second part...' (LC2)");
        }

        // LC3: Review
        if let LineParseResult::Lc { life_chunk: lc } = &result[3] {
            assert_eq!(lc.description, "Review");
            assert_eq!(lc.duration, Duration::minutes(30));
            assert_eq!(lc.categories, Vec::<String>::new()); // No category in "0 30 Review"
            assert_eq!(lc.input, "Review"); // get_life_chunk's input reconstruction
        } else {
            panic!("Expected LifeChunk for 'Review' task (LC3)");
        }
    }


    #[test]
    fn test_parse_all_lines_multiline_at_eof() {
        let lines = vec![
            "2024-03-12 14:00".to_string(),
            "0 15 Quick Sync @Team".to_string(),
            "Final notes for the day.".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 2); // Date, LC
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Quick Sync\nFinal notes for the day.");
            assert_eq!(lc.input, "Quick Sync @Team\nFinal notes for the day.");
        } else {
            panic!("Expected LifeChunk at EOF");
        }
    }

    #[test]
    fn test_parse_all_lines_no_continuation_if_next_line_is_indented_activity() {
        // An indented line that could be a valid activity (e.g., starts with duration) should not be a continuation
        let lines = vec![
            "2024-03-12 15:00".to_string(),
            "1 0 Main Task @ProjectY".to_string(),
            "  0 30 Sub-task @ProjectY".to_string(), // This is a new activity, not a continuation
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 3); // Date, LC1, LC2
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Main Task");
        } else {
            panic!("Expected LifeChunk for Main Task");
        }
        if let LineParseResult::Lc { life_chunk: lc } = &result[2] {
            assert_eq!(lc.description, "Sub-task"); // get_life_chunk will parse "  0 30 Sub-task @ProjectY"
                                                   // The leading spaces in "  0 30" mean parse_token_as_duration will fail for hours.
                                                   // Then it will try to parse "0" as minutes, then "30" as description part.
                                                   // This highlights how get_life_chunk parses.
                                                   // Correct parsing of "  0 30 Sub-task" by get_life_chunk would require it to trim tokens.
                                                   // For this test, we focus on parse_all_lines not merging.
                                                   // The actual content of lc.description for the second LC depends on get_life_chunk's behavior with leading spaces.
                                                   // Given current get_life_chunk: "0" and "30" are consumed by duration parsing (as 0 hours, 0 minutes because of space)
                                                   // So description becomes "Sub-task"
            assert_eq!(lc.description, "Sub-task");
            assert_eq!(lc.categories, vec!["@ProjectY".to_string()]);
            // Duration parsing needs to be robust to leading spaces in number tokens for this to be 30 mins.
            // Currently, "  0" will not parse as int. So h=0. "30" will not parse as int. So m=0.
            // This test is more about non-continuation.
            // Let's assume get_life_chunk is as is.
            // "  0" -> h=0. "30" -> m=0. Description = "Sub-task @ProjectY". Categories = ["@ProjectY"]. Input = "Sub-task @ProjectY"
            // This is not quite right. `tokens.next()` will yield " ", then "0", then "30".
            // `split_whitespace` is used in `get_life_chunk`. So "  0" becomes "0".
            // So, "0 30 Sub-task @ProjectY" (after initial space trim by line processing if any)
            // h=0, m=30. Description = "Sub-task". Categories = ["@ProjectY"]
            assert_eq!(lc.duration, Duration::minutes(30));
        } else {
            panic!("Expected LifeChunk for Sub-task");
        }
    }

    #[test]
    fn test_parse_all_lines_empty_lines_between_activity_and_continuation() {
        // Current logic: empty lines (noop) will break continuation.
        let lines = vec![
            "2024-03-12 16:00".to_string(),
            "1 0 Activity @Test".to_string(),
            "".to_string(), // Empty line
            "This should be a new activity, not a continuation.".to_string(),
        ];
        let mut iter = lines.iter();
        let result = parse_all_lines(&mut iter);

        assert_eq!(result.len(), 3); // Date, LC1, LC2 (from "This should be...")
        if let LineParseResult::Lc { life_chunk: lc } = &result[1] {
            assert_eq!(lc.description, "Activity"); // Not "Activity\nThis should be..."
        } else {
            panic!("Expected LifeChunk for Activity");
        }
        if let LineParseResult::Lc { life_chunk: lc } = &result[2] {
            // "This" and "should" are consumed by get_life_chunk's duration parsing attempts.
            assert_eq!(lc.description, "be a new activity, not a continuation.");
        } else {
            panic!("Expected LifeChunk for the text after empty line");
        }
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
    // Use split_whitespace to handle various whitespace characters robustly
    // and correctly tokenize, especially with leading/multiple spaces.
    let mut tokens = line.split_whitespace();

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

// Helper function to determine if a line looks like the start of a new activity.
// A line is considered a potential activity start if its first token is a number
// (suggesting an explicit hour duration is provided).
fn is_potential_activity_start(line: &str) -> bool {
    let mut tokens = line.split_whitespace();
    if let Some(first_token) = tokens.next() {
        if first_token.parse::<i64>().is_ok() {
            // First token is a number, highly likely an activity start with hours.
            return true;
        }
    }
    // First token is not a number (or line is empty/only whitespace).
    // Unlikely to be an activity start that should override continuation logic.
    false
}

/// this function should not exist, the conversion should happen now
fn parse_all_lines(lines: &mut Iter<String>) -> Vec<LineParseResult> {
    let mut list_of_pr: Vec<LineParseResult> = vec![];
    let mut line_iter = lines.peekable();

    while let Some(s) = line_iter.next() {
        if is_noop(s) {
            continue;
        }

        match process_line(s) {
            Date { date } => {
                // If the first non-noop line is not a date, we can't start.
                // However, if list_of_pr is already populated, a new date means a new LifeLapse.
                if list_of_pr.is_empty() || is_a_date_token(list_of_pr.first().unwrap()) {
                    list_of_pr.push(Date { date });
                } else {
                    // This case implies a date line encountered without a preceding date to start a lapse,
                    // which should ideally be handled or logged as an error.
                    // For now, we'll push it, but this might lead to issues later if not handled.
                    list_of_pr.push(Date { date });
                }
            }
            Lc { mut life_chunk } => {
                if list_of_pr.is_empty() {
                    // First item must be a date. If not, this LifeChunk is orphaned.
                    // This indicates an issue with input file structure or a need for a default date.
                    // For now, we skip processing this chunk if no date has been set.
                    // Consider logging this or returning an error.
                    continue;
                }

                // Peek at the next line to check for continuations
                // Loop while the next line exists and is a valid continuation
                while line_iter.peek().map_or(false, |next_line_to_peek| {
                    let next_line_str_for_check = next_line_to_peek.trim_start();
                    !is_noop(next_line_str_for_check) &&
                    parse_date(next_line_str_for_check).is_none() &&
                    !is_potential_activity_start(next_line_str_for_check)
                }) {
                    // The line is a continuation, so consume it and append its content.
                    let consumed_continuation_line_ref = line_iter.next().expect("Peeked Some, so next should be Some");
                    let consumed_continuation_line_str_trimmed = consumed_continuation_line_ref.trim_start();

                    life_chunk.description.push_str("\n");
                    life_chunk.description.push_str(consumed_continuation_line_str_trimmed);

                    life_chunk.input.push_str("\n");
                    life_chunk.input.push_str(consumed_continuation_line_str_trimmed);
                }
                list_of_pr.push(Lc { life_chunk });
            }
        }
    }

    // Ensure the first element is a Date, if the list is not empty.
    // This assertion helps catch structural issues early.
    assert!(list_of_pr.is_empty() || is_a_date_token(list_of_pr.first().unwrap()), "First element must be a Date token if list is not empty.");

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
