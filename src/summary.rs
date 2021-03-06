use std::collections::HashMap;

use colored::Colorize;
use time::Duration;

use crate::config::Category;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use chrono::{Date, DateTime, Local};
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

pub fn merge_summaries_on_same_date(
    summaries: Vec<(Timestamp, Summary)>,
) -> Vec<(Timestamp, Summary)> {
    let mut cur_date = None;

    // XXX: ugly
    let mut new_summaries: Vec<_> = vec![(Local::now(), HashMap::new())];

    for (date, summary) in summaries {
        assert!(!new_summaries.is_empty());
        if Some(date.date()) == cur_date || cur_date.is_none() {
            let (_, last) = new_summaries.pop().unwrap();
            new_summaries.push((date, merge_summaries(last, summary)));
            cur_date = Some(date.date());
        } else {
            new_summaries.push((date, summary));
        }
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
    let mut lines = vec![];
    let f_date = date.to_string().bold();
    lines.push(format!("{} (summary)", f_date));
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
