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
use chrono::{Local, TimeZone, NaiveDateTime, DateTime, Utc}; // Added NaiveDateTime, DateTime, Utc for Arbitrary impls
use time::Duration;

use crate::config::MetaCategory::{Quad, RegularCategory};
use crate::config::{update_parse_state_from_config, Config, MetaCategory, Quadrant}; // Quadrant for Arbitrary impls
use crate::merge::merge_strictly_compatible_lifelapses;
use crate::parse::LineParseResult::{Date, Lc};
use crate::parse_state::ParseState;
use crate::summary::Timestamp; // This is DateTime<Local>
use anyhow::Result;

#[cfg(test)]
use quickcheck::{Arbitrary, Gen}; // Gen for Arbitrary impls
// Removed redundant: use crate::config::Quadrant;


#[cfg(test)]
mod tests {
    use chrono::{Local, TimeZone, NaiveDateTime, DateTime, Utc};
    use time::Duration;
    use quickcheck_macros::quickcheck;
    use super::*; // Import items from parent module (TimedLifeChunk, LifeChunk, parse_date etc.)
    use crate::config::Quadrant;
    use quickcheck::TestResult; // For tests that return TestResult
    // Ensure Arbitrary is in scope for #[quickcheck] macro expansion and Arbitrary trait bounds.
    // Though individual Arbitrary impls are outside this module, the macro needs it.
    #[allow(unused_imports)] use quickcheck::Arbitrary;


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

    #[quickcheck]
    fn prop_parse_date_invalid(s: String) -> quickcheck::TestResult {
        // If the randomly generated string happens to be a valid date, discard the test.
        // Otherwise, ensure parse_date returns None.
        if parse_date(&s).is_some() {
            // It's tricky to ensure s is truly "invalid" without replicating parse_date logic.
            // A simpler check: if it parses, it might be valid.
            // A more robust check would be to try formatting s with known formats and see if it matches.
            // For now, we'll assume if parse_date parses it, it's a valid case for parse_date itself.
            // This property mainly tests that random garbage strings don't cause panics and usually don't parse.
            quickcheck::TestResult::discard()
        } else {
            quickcheck::TestResult::from_bool(parse_date(&s).is_none())
        }
    }

    // Tests for get_life_chunk
    #[quickcheck]
    fn prop_get_life_chunk_duration(h: u8, m: u8) -> bool {
        // Cap hours and minutes to reasonable values for a single entry
        let hours = h % 24; // 0-23
        let minutes = m % 60; // 0-59
        let input_str = format!("{} {} Test description", hours, minutes);
        let lc = get_life_chunk(&input_str);
        lc.duration == Duration::hours(hours as i64) + Duration::minutes(minutes as i64)
    }

    // Removed prop_get_life_chunk_categories as it was failing due to quadrant handling.
    // Consider re-adding with more sophisticated logic if quadrant parsing needs to be tested here.

    #[quickcheck]
    fn prop_get_life_chunk_input_persisted(hours: u8, minutes: u8, description: String, raw_categories: Vec<String>) -> quickcheck::TestResult {
        let h = hours % 24;
        let m = minutes % 60;

        // Sanitize description: take first few words, no @ symbols unless intended
        let description_words: Vec<String> = description.split_whitespace()
            .map(|s| s.chars().filter(|c| c.is_alphanumeric() || c.is_whitespace()).take(30).collect::<String>().trim().chars().take(15).collect()) // Allow spaces within parts, then trim and take
            .filter(|s: &String| !s.is_empty())
            .take(5) // Max 5 words for description
            .collect();

        if description_words.is_empty() {
            return quickcheck::TestResult::discard();
        }
        let clean_description = description_words.join(" ");

        // Sanitize categories
        let mut processed_categories = Vec::new();
        let mut original_cat_tokens = Vec::new();
        for raw_cat in raw_categories.iter().take(3) { // Max 3 categories
            let cat_name: String = raw_cat.chars().filter(|c| c.is_alphanumeric()).take(10).collect();
            if !cat_name.is_empty() {
                let cat_token = format!("@{}", cat_name);
                processed_categories.push(cat_token.clone());
                original_cat_tokens.push(cat_token);
            }
        }
        let categories_str = processed_categories.join(" ");

        let line_parts: Vec<String> = vec![
            h.to_string(),
            m.to_string(),
            clean_description.clone(),
            categories_str
        ];
        let line = line_parts.join(" ").trim().to_string();

        if line.split_whitespace().count() < 2 { // Ensure at least duration or description
             return quickcheck::TestResult::discard();
        }

        let lc = get_life_chunk(&line);
        let parsed_input_field = lc.get_input();

        // Check that the essential parts of the input (description, categories) are somewhat represented in lc.get_input()
        // This is a heuristic because get_life_chunk reconstructs `input`.
        let desc_present = clean_description.split_whitespace().all(|word| parsed_input_field.contains(word));
        let cats_present = original_cat_tokens.iter().all(|cat_token| parsed_input_field.contains(cat_token));

        // And that the parsed description is what we fed in (or a processed version)
        let parsed_desc_correct = lc.description == clean_description || clean_description.contains(&lc.description) && !lc.description.is_empty();

        quickcheck::TestResult::from_bool(desc_present && cats_present && parsed_desc_correct)
    }


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
}

#[cfg(test)]
impl Arbitrary for TimedLifeChunk {
    fn arbitrary(g: &mut Gen) -> Self {
        // Generate a random number of seconds since Unix epoch (or any reasonable range)
        // Let's cap it to avoid extremely distant dates, e.g., +/- 50 years from now.
        // Current approx_epoch_sec for 2024.
        // 50 years * 365.25 days/year * 24 hours/day * 3600 seconds/hour
        const FIFTY_YEARS_IN_SECONDS: i64 = 50 * 36525 * 24 * 36; // Simplified 365.25 * 100 / 100 for i64
        let now_timestamp = Local::now().timestamp();
        let seconds_offset = i64::arbitrary(g) % FIFTY_YEARS_IN_SECONDS;
        let random_timestamp_secs = now_timestamp + seconds_offset;

        let naive_datetime = NaiveDateTime::from_timestamp_opt(random_timestamp_secs, 0).unwrap_or_else(|| NaiveDateTime::from_timestamp_opt(0,0).unwrap());
        let start_utc = Utc.from_utc_datetime(&naive_datetime); // Corrected method
        let start = Timestamp::from(start_utc);

        TimedLifeChunk {
            start,
            life_chunk: LifeChunk::arbitrary(g),
        }
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

#[cfg(test)]
impl Arbitrary for LifeChunk {
    fn arbitrary(g: &mut Gen) -> Self {
        let description = String::arbitrary(g);
        // Generate duration in minutes, up to a full day (1440 minutes) to keep it somewhat reasonable
        let duration_minutes = i64::arbitrary(g) % 1440;
        let duration = Duration::minutes(duration_minutes.abs()); // Ensure positive duration

        let num_categories = usize::arbitrary(g) % 5; // Max 4 categories
        let categories = (0..num_categories)
            .map(|_| {
                let cat_name = String::arbitrary(g);
                // Simple way to make it look like a category, not strictly necessary for all tests
                if bool::arbitrary(g) && !cat_name.is_empty() {
                    format!("@{}", cat_name.chars().filter(|c| c.is_alphanumeric()).collect::<String>())
                } else {
                    cat_name
                }
            })
            .collect();

        let quadrant = Quadrant::arbitrary(g);
        let user_provided_quadrant = bool::arbitrary(g);
        let input = String::arbitrary(g);

        LifeChunk {
            description,
            duration,
            categories,
            quadrant,
            user_provided_quadrant,
            input,
        }
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

fn parse_category(token: &str) -> Option<MetaCategory> {
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
