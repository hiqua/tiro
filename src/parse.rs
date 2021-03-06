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

use chrono::{Local, TimeZone};
use chrono::offset::LocalResult;
use chrono::prelude::*;
use time::Duration;

use crate::{TiroError, TiroResult};
use crate::config::{Config, MetaCategory, Quadrant, update_parse_state_from_config};
use crate::config::MetaCategory::{Quad, RegularCategory};
use crate::merge::merge_strictly_compatible_lifelapses;
use crate::parse::LineParseResult::{Date, Lc};
use crate::summary::Timestamp;

#[cfg(test)]
mod tests {
    use chrono::{Local, TimeZone};

    use crate::parse::parse_date;

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
}

impl From<ParseIntError> for TiroError {
    fn from(e: ParseIntError) -> Self {
        TiroError { e: e.to_string() }
    }
}

pub struct ParseState {
    pub categories_to_quadrant: HashMap<String, Quadrant>,
}

impl ParseState {
    pub fn new() -> ParseState {
        ParseState {
            categories_to_quadrant: HashMap::new(),
        }
    }
}

/// Invariant: start == tokens[0].start
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

    pub fn extend<I: IntoIterator<Item=TimedLifeChunk>>(&mut self, iter: I) {
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
    // LineParseError { line: String },
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

/// The first vector of activities has a special role, in that it defines the starting time.
/// Activities occurring before this starting time (in other activity files) will be ignored.
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
        all_life_lapses.get(0).unwrap().start
    };

    all_life_lapses.sort_by_key(|ll| ll.start);

    all_life_lapses = all_life_lapses
        .into_iter()
        .skip_while(|ll| ll.start < start_time)
        // XXX: why can they be empty at this point?
        .filter(|ll| !ll.is_empty())
        .collect();

    all_life_lapses = merge_strictly_compatible_lifelapses(all_life_lapses);

    (start_time, all_life_lapses)
}

pub fn parse_activities(mut it: Iter<String>, config: &Config) -> Vec<LifeLapse> {
    let list_of_pr = parse_all_lines(&mut it);

    let start_time = if let Some(LineParseResult::Date { date }) = list_of_pr.first() {
        *date
    } else {
        assert!(list_of_pr.is_empty());
        return vec![];
    };

    let mut parse_state = register_all_categories(&list_of_pr);

    update_parse_state_from_config(config, &mut parse_state).expect("");

    let list_of_pr = update_all_quadrants(list_of_pr, &parse_state);

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

pub fn look_for_seen_quadrant(chunk: &LifeChunk, pst: &ParseState) -> Option<Quadrant> {
    chunk
        .categories
        .iter()
        .map(|c| pst.categories_to_quadrant.get(c).copied())
        .flatten()
        .next()
}

pub fn update_quadrant(mut chunk: LifeChunk, pst: &ParseState) -> LifeChunk {
    match look_for_seen_quadrant(&chunk, pst) {
        None => chunk,
        Some(q) => {
            chunk.quadrant = q;
            chunk
        }
    }
}

pub fn register_categories_from_life_chunk(chunk: &LifeChunk, pst: &mut ParseState) {
    if chunk.quadrant == Default::default() {
        return;
    }
    for cat in &chunk.categories {
        pst.categories_to_quadrant
            .insert(cat.to_string(), chunk.quadrant);
    }
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

fn get_life_chunk(line: &str) -> LifeChunk {
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
        match process_line(s) {
            Some(LineParseResult::Date { date }) => list_of_pr.push(LineParseResult::Date { date }),
            // Some(LineParseError { line }) => {
            //     if log_errors {
            //         println!("Found faulty line in input:\n{}\n", line);
            //     }
            // }
            Some(lp) => {
                if !list_of_pr.is_empty() {
                    list_of_pr.push(lp)
                }
            }
            None => {}
        }
    }

    assert!(list_of_pr.is_empty() || is_a_date_token(list_of_pr.first().unwrap()));

    list_of_pr
}

fn register_all_categories(list_of_timed_pr: &[LineParseResult]) -> ParseState {
    let mut parse_state = ParseState::new();
    for lpr in list_of_timed_pr {
        if let LineParseResult::Lc { life_chunk: lc } = lpr {
            register_categories_from_life_chunk(lc, &mut parse_state);
        }
    }
    parse_state
}

fn update_lpr_quadrant(lpr: LineParseResult, parse_state: &ParseState) -> LineParseResult {
    if let LineParseResult::Lc { life_chunk: lc } = lpr {
        if !lc.user_provided_quadrant {
            let new_lc = update_quadrant(lc, &parse_state);
            LineParseResult::Lc { life_chunk: new_lc }
        } else {
            LineParseResult::Lc { life_chunk: lc }
        }
    } else {
        lpr
    }
}

/// Updates the state to contain the quadrant matching the categories
fn update_all_quadrants(
    list_of_timed_pr: Vec<LineParseResult>,
    parse_state: &ParseState,
) -> Vec<LineParseResult> {
    let mut new_list_of_pr = vec![];

    for lpr in list_of_timed_pr {
        new_list_of_pr.push(update_lpr_quadrant(lpr, parse_state));
    }

    new_list_of_pr
}

fn tokens_from_timed_lpr(
    list_of_pr: Vec<LineParseResult>,
    start_time: Timestamp,
) -> Vec<TiroToken> {
    let mut tiro_tokens = vec![];
    let mut curr_time = start_time;
    for lpr in list_of_pr {
        match lpr {
            LineParseResult::Lc { life_chunk } => {
                let duration = life_chunk.duration;
                let tlc = TimedLifeChunk {
                    start: curr_time,
                    life_chunk,
                };
                curr_time = curr_time + duration;
                tiro_tokens.push(TiroToken::Tlc { tlc })
            }
            LineParseResult::Date { date } => {
                curr_time = date;
                tiro_tokens.push(TiroToken::Date { date })
            } // LineParseResult::LineParseError { .. } => {}
        };
    }

    tiro_tokens
}

/// Parse a line from the input
fn process_line(line: &str) -> Option<LineParseResult> {
    if line.starts_with('#') || line.trim().is_empty() {
        return None;
    }

    if let Some(date) = parse_date(line) {
        Some(Date { date })
    } else {
        Some(Lc {
            life_chunk: get_life_chunk(line),
        })
    }
}

/// Whether a token is a date
fn is_a_date_token(t: &LineParseResult) -> bool {
    let d = LineParseResult::Date { date: Local::now() };
    discriminant(t) == discriminant(&d)
}
