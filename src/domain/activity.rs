//! Activity domain types for time tracking.

use chrono::{DateTime, Local, TimeDelta};
use std::ops::Add;

use crate::config::Quadrant;

/// Category for activities (e.g., "@work", "@home")
pub type Category = String;

/// The timestamp of activities.
pub type Timestamp = DateTime<Local>;

/// A continuous series of life chunks.
///
/// # Invariants
/// - If non-empty: `start == tokens[0].start`
/// - `end == start + sum(tok.duration for tok in tokens)`
/// - All tokens are contiguous (no gaps or overlaps)
///
/// These invariants are enforced by the implementation.
#[derive(Clone, Debug)]
pub struct LifeLapse {
    start: Timestamp,
    end: Timestamp,
    tokens: Vec<TimedLifeChunk>,
}

impl LifeLapse {
    /// Creates a new empty LifeLapse starting at the given time.
    pub(crate) fn new(start: Timestamp) -> LifeLapse {
        LifeLapse {
            start,
            end: start,
            tokens: vec![],
        }
    }

    /// Returns the total duration of all tokens.
    fn total_duration(&self) -> TimeDelta {
        self.tokens
            .iter()
            .fold(TimeDelta::hours(0), |sum, t| {
                sum.add(t.life_chunk.duration)
            })
    }

    /// Extends this LifeLapse with an iterator of TimedLifeChunks.
    ///
    /// # Panics
    /// Panics if the first token's start time doesn't match this LifeLapse's start time
    /// when the LifeLapse is empty.
    pub fn extend<I: IntoIterator<Item = TimedLifeChunk>>(&mut self, iter: I) {
        let new_tokens: Vec<_> = iter.into_iter().collect();
        
        // Validate invariant: first token must match start time if this is empty
        if self.tokens.is_empty() && !new_tokens.is_empty() {
            assert_eq!(
                self.start, new_tokens[0].start,
                "First token start time must match LifeLapse start time"
            );
        }
        
        self.tokens.extend(new_tokens);
        
        // Recalculate end time to maintain invariant
        let d = self.total_duration();
        self.end = self.start + d;
    }

    /// Pushes a single TimedLifeChunk to this LifeLapse.
    ///
    /// # Panics
    /// Panics if the token's start time doesn't match this LifeLapse's start time
    /// when the LifeLapse is empty.
    pub(crate) fn push(&mut self, item: TimedLifeChunk) {
        // Validate invariant: first token must match start time if this is empty
        if self.tokens.is_empty() {
            assert_eq!(
                self.start, item.start,
                "First token start time must match LifeLapse start time"
            );
        }
        
        self.end = self.end + item.life_chunk.duration;
        self.tokens.push(item);
    }

    /// Consumes this LifeLapse and returns its tokens.
    pub fn tokens(self) -> Vec<TimedLifeChunk> {
        self.tokens
    }

    /// Returns a reference to the tokens.
    pub fn tokens_as_ref(&self) -> &Vec<TimedLifeChunk> {
        &self.tokens
    }

    /// Returns the start time of this LifeLapse.
    pub fn start(&self) -> Timestamp {
        self.start
    }

    /// Returns the end time of this LifeLapse.
    #[allow(dead_code)]
    pub fn end(&self) -> Timestamp {
        self.end
    }

    /// Returns true if this LifeLapse ends exactly when the other begins.
    pub fn is_right_before(&self, other: &LifeLapse) -> bool {
        self.end == other.start
    }

    /// Returns true if this LifeLapse contains no tokens.
    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

/// A life chunk with a specific start time.
#[derive(Clone, Debug)]
pub struct TimedLifeChunk {
    pub start: Timestamp,
    pub life_chunk: LifeChunk,
}

/// A chunk of time spent on activities with categories and description.
#[derive(Clone, Debug)]
pub struct LifeChunk {
    #[allow(dead_code)]
    pub description: String,
    pub duration: TimeDelta,
    pub categories: Vec<Category>,
    pub quadrant: Quadrant,
    pub user_provided_quadrant: bool,
    input: String,
}

impl LifeChunk {
    /// Creates a new LifeChunk.
    pub(crate) fn new(
        description: String,
        duration: TimeDelta,
        categories: Vec<Category>,
        quadrant: Quadrant,
        user_provided_quadrant: bool,
        input: String,
    ) -> Self {
        LifeChunk {
            description,
            duration,
            categories,
            quadrant,
            user_provided_quadrant,
            input,
        }
    }

    pub(crate) fn get_input(&self) -> &str {
        &self.input
    }
}

/// Token types used during parsing.
#[derive(Clone)]
pub enum TiroToken {
    Tlc { tlc: TimedLifeChunk },
    Date { date: Timestamp },
}

/// Result of parsing a single line.
#[derive(Clone)]
pub enum LineParseResult {
    Lc { life_chunk: LifeChunk },
    Date { date: Timestamp },
}
