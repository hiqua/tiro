use std::collections::HashMap;

use crate::config::Quadrant;
use crate::parse::{LifeChunk, LineParseResult};

pub struct ParseState {
    pub categories_to_quadrant: HashMap<String, Quadrant>,
}

impl ParseState {
    pub fn new() -> ParseState {
        ParseState {
            categories_to_quadrant: HashMap::new(),
        }
    }

    /// Updates the state to contain the quadrant matching the categories
    pub fn update_category_quadrants(&self, list_of_timed_pr: Vec<LineParseResult>) -> Vec<LineParseResult> {
        let mut new_list_of_pr = vec![];

        for lpr in list_of_timed_pr {
            new_list_of_pr.push(self.update_lpr_quadrant(lpr));
        }

        new_list_of_pr
    }


    pub fn register_categories_from_life_chunk(&mut self, chunk: &LifeChunk) {
        if chunk.quadrant == Default::default() {
            return;
        }
        for cat in &chunk.categories {
            self.categories_to_quadrant.insert(cat.to_string(), chunk.quadrant);
        }
    }

    fn look_for_seen_quadrant(&self, chunk: &LifeChunk) -> Option<Quadrant> {
        chunk
            .categories
            .iter()
            .map(|c| self.categories_to_quadrant.get(c).copied())
            .flatten()
            .next()
    }

    fn update_quadrant(&self, mut chunk: LifeChunk) -> LifeChunk {
        match self.look_for_seen_quadrant(&chunk) {
            None => chunk,
            Some(q) => {
                chunk.quadrant = q;
                chunk
            }
        }
    }


    fn update_lpr_quadrant(&self, lpr: LineParseResult) -> LineParseResult {
        if let LineParseResult::Lc { life_chunk: lc } = lpr {
            if !lc.user_provided_quadrant {
                let new_lc = self.update_quadrant(lc);
                LineParseResult::Lc { life_chunk: new_lc }
            } else {
                LineParseResult::Lc { life_chunk: lc }
            }
        } else {
            lpr
        }
    }
}
