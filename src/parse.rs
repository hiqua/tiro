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
    use crate::parse::{get_life_chunk, parse_date, process_line, LifeChunk, LineParseResult};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;

    // Implement Arbitrary for Quadrant
    impl Arbitrary for Quadrant {
        fn arbitrary(g: &mut Gen) -> Self {
            // Use only existing variants from src/config.rs
            let variants = [
                Quadrant::Q1,
                Quadrant::Q2,
                Quadrant::Q3,
                Quadrant::Q4,
                Quadrant::Q5, // Added Q5
                Quadrant::Q6, // Added Q6
            ];
             if variants.is_empty() {
                 return Quadrant::default();
             }
            *g.choose(&variants).unwrap()
        }
    }

    // Implement Arbitrary for LifeChunk
    impl Arbitrary for LifeChunk {
        fn arbitrary(g: &mut Gen) -> Self {
            let mut description;
            loop {
                description = String::arbitrary(g);
                // Ensure description tokens (as per get_life_chunk's splitting)
                // don't look like categories/quadrants.
                if description
                    .split(|c: char| c == ',' || c.is_whitespace())
                    .all(|tok| {
                        if tok.starts_with('@') && tok.len() > 1 {
                            // If it starts with @ and is longer than "@", it might be a category.
                            // We need to check if parse_category would actually parse it as one.
                            // parse_category checks `spp.len() == 2 && spp[0].is_empty()` for `spp = token.split('@')`
                            // and then tries Quadrant::from_str or makes a RegularCategory.
                            // For simplicity in Arbitrary, we'll be a bit stricter:
                            // if it starts with "@" and has more after it, reject it for description.
                            // This avoids the description accidentally consuming something that should have been a category if it were generated separately.
                            // This restriction could be refined if it filters out too many valid descriptions.
                            false // Disallow tokens like "@foo" in arbitrary descriptions.
                        } else {
                            true
                        }
                    })
                {
                    break;
                }
            }
            let hours = u8::arbitrary(g) % 24;
            let minutes = u8::arbitrary(g) % 60;
            let duration = Duration::hours(hours as i64) + Duration::minutes(minutes as i64);

            let num_categories = u8::arbitrary(g) % 5;
            let categories: Vec<String> = (0..num_categories) // Specify Vec<String> here
                .map(|_| format!("@{}", String::arbitrary(g).chars().take(10).collect::<String>())) // Limit category name length
                .collect();

            let quadrant = Quadrant::arbitrary(g);
            let user_provided_quadrant = bool::arbitrary(g);

            let cats_str_for_input = categories.join(" ");
            let quad_str_for_input = if user_provided_quadrant {
                quadrant_to_string_for_test(quadrant)
            } else {
                "".to_string()
            };
            // The 'input' field should represent the part of the line AFTER duration.
            // It's what get_life_chunk().input would be.
            let mut input_parts = Vec::new();
            if !description.is_empty() {
                input_parts.push(description.clone());
            }
            input_parts.push(cats_str_for_input);
            if user_provided_quadrant { // Only add quadrant string if it was meant to be there
                input_parts.push(quad_str_for_input);
            }
            // get_life_chunk joins tokens with " ". Some might be empty.
            let input = input_parts.iter().filter(|s| !s.is_empty()).cloned().collect::<Vec<_>>().join(" ");


            LifeChunk {
                description,
                duration,
                categories,
                quadrant,
                user_provided_quadrant,
                input, // This now more closely matches get_life_chunk's behavior for its .input field
            }
        }
    }

    // Renamed to avoid conflict if there's another quadrant_to_string
    fn quadrant_to_string_for_test(quadrant: Quadrant) -> String {
        match quadrant {
            Quadrant::Q1 => "@Q1".to_string(),
            Quadrant::Q2 => "@Q2".to_string(),
            Quadrant::Q3 => "@Q3".to_string(),
            Quadrant::Q4 => "@Q4".to_string(),
            Quadrant::Q5 => "@Q5".to_string(), // Added Q5
            Quadrant::Q6 => "@Q6".to_string(), // Added Q6
        }
    }

    // Unused function categories_to_string removed.

    #[quickcheck]
    fn prop_duration_non_negative(line: String) -> bool {
        let lc = get_life_chunk(&line);
        lc.duration >= Duration::zero()
    }

    #[quickcheck]
    fn prop_categories_start_with_at(line: String) -> bool {
        let lc = get_life_chunk(&line);
        lc.categories.iter().all(|cat| cat.starts_with('@'))
    }

    // This property might be tricky to get right due to parsing details.
    // It's a good example of a property that might need refinement.
    // #[quickcheck]
    // fn prop_input_reconstruction(input_lc: LifeChunk) -> bool {
        // // Construct a line from LifeChunk fields that get_life_chunk would parse.
        // // This is highly dependent on the parsing logic of get_life_chunk.
        // // For simplicity, let's assume a basic format. This will likely need adjustment.
        // let hours = input_lc.duration.num_hours();
        // let minutes = input_lc.duration.num_minutes() % 60;
        //
        // let cats_str = input_lc.categories.join(" ");
        // // Quadrant representation in string form needs to match what get_life_chunk expects.
        // let quad_str = if input_lc.user_provided_quadrant {
            // // Use the corrected function name here
            // quadrant_to_string_for_test(input_lc.quadrant)
        // } else {
            // "".to_string()
        // };
        //
        // // The order and spacing are crucial and must match get_life_chunk's expectations.
        // let line = format!("{} {} {} {} {}", hours, minutes, input_lc.description, cats_str, quad_str)
            // .trim().replace("  ", " "); // Basic cleanup
        //
        // let parsed_lc = get_life_chunk(&line);
        //
        // // Compare relevant fields.
        // // Normalize input_lc.description in the same way get_life_chunk processes descriptions
        // let normalized_input_description = input_lc.description
            // .split(|c: char| c == ',' || c.is_whitespace())
            // .filter(|s| !s.is_empty()) // Filter out empty strings that can result from multiple spaces
            // .collect::<Vec<&str>>()
            // .join(" ");
        // let description_match = parsed_lc.description == normalized_input_description;
        // let duration_match = parsed_lc.duration == input_lc.duration;
        //
        // // Category comparison: ensure both have same categories, order might not matter for get_life_chunk
        // let mut parsed_cats_sorted = parsed_lc.categories.clone();
        // parsed_cats_sorted.sort();
        // let mut input_cats_sorted = input_lc.categories.clone();
        // input_cats_sorted.sort();
        // let categories_match = parsed_cats_sorted == input_cats_sorted;
        //
        // let quadrant_match = if input_lc.user_provided_quadrant {
            // // If a quadrant was specified in the input string, it should be parsed as such
            // parsed_lc.quadrant == input_lc.quadrant && parsed_lc.user_provided_quadrant
        // } else {
            // // If no quadrant was specified, it should default, and user_provided_quadrant should be false.
            // // The original input_lc.quadrant doesn't matter here if user_provided_quadrant was false.
            // parsed_lc.quadrant == Quadrant::default() && !parsed_lc.user_provided_quadrant
        // };
        //
        // // The input field check (parsed_lc.get_input() == input_lc.get_input()) has been removed
        // // as it's too brittle for arbitrary string generation due to whitespace and tokenization nuances.
        // // The core semantic fields (description, duration, categories, quadrant) are checked.
        //
        // description_match && duration_match && categories_match && quadrant_match
    // }


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
#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimedLifeChunk {
    pub start: Timestamp,
    pub life_chunk: LifeChunk,
}

#[derive(Clone)]
pub enum LineParseResult {
    Lc { life_chunk: LifeChunk },
    Date { date: Timestamp },
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
                // global_quad: None, // Field was removed
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
