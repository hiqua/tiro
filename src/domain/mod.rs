//! Domain model for the tiro time tracking application.
//!
//! This module contains the core domain types and business logic,
//! independent of parsing, I/O, or presentation concerns.

pub mod activity;

pub use activity::{
    Category, LifeChunk, LifeLapse, LineParseResult, TimedLifeChunk, Timestamp, TiroToken,
};
