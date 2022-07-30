use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::iter::Sum;

use colored::Colorize;
use time::Duration;

use crate::config::Category;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use chrono::{Date, Datelike, DateTime, Local};
use std::ops::Add;

pub(crate) type Summary = HashMap<String, Duration>;

pub(crate) type Timestamp = DateTime<Local>;

pub struct CategorySummary<'a> {
    pub name: &'a Category,
    pub duration: Duration,
}

fn merge_summaries(s1: Summary, s2: Summary) -> Summary {
    let mut s = s1;
    for (k, v) in s2.iter() {
        let new_duration = s
            .entry(k.clone())
            .or_insert_with(|| Duration::minutes(0))
            .add(*v);
        s.insert(k.clone(), new_duration);
    }
    s
}

pub fn merge_all_summaries(summaries: &Vec<Summary>) -> Summary {
    let mut result : Summary = HashMap::new();
    for s in summaries {
        result = merge_summaries(result, s.clone());
    }
    result
}

pub fn merge_summaries_on_same_date(
    summaries: Vec<(Timestamp, Summary)>,
) -> Vec<(Timestamp, Summary)> {
    let mut current_date = None;

    // XXX: ugly
    let mut new_summaries: Vec<_> = vec![(Local::now(), HashMap::new())];

    for (timestamp, summary) in summaries {
        assert!(!new_summaries.is_empty());
        if Some(timestamp.date()) == current_date || current_date.is_none() {
            let (_, last) = new_summaries.pop().unwrap();
            new_summaries.push((timestamp, merge_summaries(last, summary)));
        } else {
            new_summaries.push((timestamp, summary));
        }
        current_date = Some(timestamp.date());
    }

    new_summaries
}

pub fn compute_all_summaries(tiro_tokens: &[LifeLapse]) -> Vec<(Timestamp, Summary)> {
    let mut all_summaries = vec![];

    for ll in tiro_tokens {
        all_summaries.push((ll.start(), compute_summary(ll.tokens_as_ref())));
    }
    all_summaries
}

fn compute_summary(tiro_tokens: &[TimedLifeChunk]) -> Summary {
    let mut summary: Summary = HashMap::new();
    for tlc in tiro_tokens {
        update_summary_from_life_chunk(&tlc.life_chunk, &mut summary);
    }

    summary
}

pub fn format_category_summary(
    ctg_summary: Vec<CategorySummary>,
    date: Date<Local>,
) -> Vec<String> {
    format_category_summary_with_note(ctg_summary,date,"(summary)")
}

pub fn format_category_summary_with_note(
    ctg_summary: Vec<CategorySummary>,
    date: Date<Local>,
    note: &str
) -> Vec<String> {
    let mut lines = vec![];
    let f_date = date.to_string().bold();
    lines.push(format!("{} {}", f_date, note));
    for ctxt in ctg_summary {
        lines.push(format!("{}: {}", ctxt.name, format_duration(ctxt.duration)));
    }
    lines.push("".to_string());

    lines
}


pub fn compute_context_summary(contexts: &HashMap<String, Duration>) -> Vec<CategorySummary> {
    let mut kk: Vec<&String> = Vec::new();
    kk.extend(contexts.keys());
    kk.sort();

    let mut struct_c = Vec::new();
    for ctxt in kk {
        let c = CategorySummary {
            name: ctxt,
            duration: contexts[ctxt],
        };
        struct_c.push(c);
    }

    struct_c.sort_by_key(|cont| cont.name);

    struct_c
}

fn format_duration(d: Duration) -> String {
    let mut buf = String::with_capacity(5);
    let m = (d.num_minutes() % 60).to_string();
    let h = d.num_hours().to_string();
    assert_eq!(
        d.num_days(),
        0,
        "Activities lasting longer than 24h are not supported."
    );

    if h.len() == 1 {
        buf.push('0');
    }
    for c in h.chars() {
        buf.push(c);
    }
    buf.push('h');
    if m.len() == 1 {
        buf.push('0');
    }
    for c in m.chars() {
        buf.push(c);
    }

    buf
}

fn update_summary_from_life_chunk(chunk: &LifeChunk, summary: &mut Summary) {
    for cat in &chunk.categories {
        let curr_dur = summary
            .entry(cat.clone().to_string())
            .or_insert_with(Duration::zero);
        *curr_dur = *curr_dur + chunk.duration;
    }
}
