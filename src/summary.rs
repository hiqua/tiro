use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::iter::Sum;

use colored::Colorize;
use time::Duration;

use crate::config::Category;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use chrono::{Date, DateTime, Datelike, Local};
use std::ops::Add;

/// A summary matching activities to their total duration in a day.
pub(crate) type Summary = HashMap<String, Duration>;

/// The timestamp of activities.
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
    let mut result: Summary = HashMap::new();
    for s in summaries {
        result = merge_summaries(result, s.clone());
    }
    result
}

pub fn merge_summaries_on_same_date(
    summaries: Vec<(Timestamp, Summary)>,
) -> Vec<(Timestamp, Summary)> {
    let mut current_date = None;

    let mut new_summaries = vec![(Local::now(), HashMap::new())];

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
    format_category_summary_with_note(ctg_summary, date, "(summary)")
}

pub fn format_category_summary_with_note(
    ctg_summary: Vec<CategorySummary>,
    date: Date<Local>,
    note: &str,
) -> Vec<String> {
    let mut lines = vec![];
    let f_date = date.to_string();
    lines.push(format!("{} {}", f_date, note));
    for ctxt in ctg_summary {
        lines.push(format!("{}: {}", ctxt.name, format_duration(ctxt.duration)));
    }
    lines.push("".to_string());

    lines
}

pub fn compute_context_summary(contexts: &HashMap<String, Duration>) -> Vec<CategorySummary<'_>> {
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

#[cfg(test)]
mod tests {
    use crate::config::Quadrant;
    use crate::parse::get_life_chunk;
    use crate::parse::{LifeChunk, TimedLifeChunk};
    use crate::summary::{
        compute_context_summary, compute_summary, format_category_summary,
        format_category_summary_with_note, format_duration, CategorySummary, Summary,
    };
    use chrono::{Date, Datelike, Local, TimeZone, Utc};
    use colored::Colorize;
    use std::collections::HashMap;
    use time::Duration; // For asserting bolded dates

    // Helper to create TimedLifeChunk for tests using get_life_chunk
    // The start time of TimedLifeChunk is not used by compute_summary, so it can be arbitrary.
    fn create_timed_chunk_from_line(line_for_get_life_chunk: &str) -> TimedLifeChunk {
        TimedLifeChunk {
            start: Local::now(), // Arbitrary, not used by the function under test
            life_chunk: get_life_chunk(line_for_get_life_chunk),
        }
    }

    #[test]
    fn format_duration_duration_longer_than_day_returns_hours_greater_than_24() {
        let duration = Duration::hours(25);
        assert_eq!(format_duration(duration), "25h00");
    }

    #[test]
    fn format_duration_single_digit_hours_returns_padded_string() {
        let duration = Duration::hours(2) + Duration::minutes(15);
        assert_eq!(format_duration(duration), "02h15");
    }

    #[test]
    fn compute_summary_basic_chunks_returns_correct_durations() {
        let timed_life_chunks = vec![
            create_timed_chunk_from_line("0 30 Task 1 @work @projA"), // Duration 30m, cats @work, @projA
            create_timed_chunk_from_line("1 0 Task 2 @work"),         // Duration 60m, cats @work
            create_timed_chunk_from_line("0 15 Task 3 @home"),        // Duration 15m, cats @home
        ];

        let summary = compute_summary(&timed_life_chunks);

        // Assertions need to account for get_life_chunk's behavior if categories like @projA are misinterpreted
        // Based on current knowledge of get_life_chunk, quadrant tags like @Q1 can be misparsed as categories.
        // Assuming @work, @projA, @home are not quadrant-like.
        let mut expected_summary = HashMap::new();
        expected_summary.insert("@work".to_string(), Duration::minutes(30 + 60));
        expected_summary.insert("@projA".to_string(), Duration::minutes(30));
        expected_summary.insert("@home".to_string(), Duration::minutes(15));

        assert_eq!(summary.len(), expected_summary.len());
        assert_eq!(summary.get("@work"), expected_summary.get("@work"));
        assert_eq!(summary.get("@projA"), expected_summary.get("@projA"));
        assert_eq!(summary.get("@home"), expected_summary.get("@home"));
    }

    #[test]
    fn compute_summary_empty_input_returns_empty_summary() {
        let timed_life_chunks: Vec<TimedLifeChunk> = vec![];
        let summary = compute_summary(&timed_life_chunks);
        assert!(
            summary.is_empty(),
            "Summary should be empty for empty input"
        );
    }

    #[test]
    fn compute_summary_chunks_without_categories_returns_only_categorized_durations() {
        let timed_life_chunks = vec![
            create_timed_chunk_from_line("0 30 Task 1 no cat"), // No categories
            create_timed_chunk_from_line("1 0 Task 2 with cat @work"), // @work
            create_timed_chunk_from_line("0 15 Task 3 no cat again"), // No categories
        ];

        let summary = compute_summary(&timed_life_chunks);

        let mut expected_summary = HashMap::new();
        expected_summary.insert("@work".to_string(), Duration::minutes(60));

        assert_eq!(summary.len(), expected_summary.len());
        assert_eq!(summary.get("@work"), expected_summary.get("@work"));
        assert_eq!(
            summary.get(""),
            None,
            "Empty category string should not be added by get_life_chunk or summary"
        );
    }

    #[test]
    fn compute_summary_multiple_categories_in_single_chunk_returns_duration_added_to_all_categories(
    ) {
        let timed_life_chunks = vec![
            create_timed_chunk_from_line("0 45 Session 1 @planning @meeting @clientA"),
            create_timed_chunk_from_line("0 30 Session 2 @meeting @internal"),
        ];

        let summary = compute_summary(&timed_life_chunks);

        let mut expected_summary = HashMap::new();
        expected_summary.insert("@planning".to_string(), Duration::minutes(45));
        expected_summary.insert("@meeting".to_string(), Duration::minutes(45 + 30));
        expected_summary.insert("@clientA".to_string(), Duration::minutes(45));
        expected_summary.insert("@internal".to_string(), Duration::minutes(30));

        assert_eq!(summary.len(), expected_summary.len());
        assert_eq!(summary.get("@planning"), expected_summary.get("@planning"));
        assert_eq!(summary.get("@meeting"), expected_summary.get("@meeting"));
        assert_eq!(summary.get("@clientA"), expected_summary.get("@clientA"));
        assert_eq!(summary.get("@internal"), expected_summary.get("@internal"));
    }

    #[test]
    fn compute_summary_zero_duration_chunks_returns_zero_duration_entries() {
        let timed_life_chunks = vec![
            create_timed_chunk_from_line("0 0 Task 1 zero duration @work @projA"),
            create_timed_chunk_from_line("1 0 Task 2 normal duration @work"),
            create_timed_chunk_from_line("0 0 Task 3 zero duration @home"),
        ];

        let summary = compute_summary(&timed_life_chunks);

        let mut expected_summary = HashMap::new();
        expected_summary.insert("@work".to_string(), Duration::minutes(60));
        expected_summary.insert("@projA".to_string(), Duration::minutes(0));
        expected_summary.insert("@home".to_string(), Duration::minutes(0));

        assert_eq!(summary.len(), expected_summary.len());
        assert_eq!(summary.get("@work"), expected_summary.get("@work"));
        assert_eq!(summary.get("@projA"), expected_summary.get("@projA"));
        assert_eq!(summary.get("@home"), expected_summary.get("@home"));
    }

    // Tests for compute_context_summary
    #[test]
    fn compute_context_summary_empty_map_returns_empty_vec() {
        let contexts: HashMap<String, Duration> = HashMap::new();
        let result = compute_context_summary(&contexts);
        assert!(
            result.is_empty(),
            "Expected empty vector for empty input HashMap"
        );
    }

    #[test]
    fn compute_context_summary_basic_map_returns_sorted_summaries() {
        let mut contexts: HashMap<String, Duration> = HashMap::new();
        contexts.insert("@work".to_string(), Duration::hours(2));
        contexts.insert("@home".to_string(), Duration::hours(1));
        contexts.insert("@study".to_string(), Duration::minutes(30));

        let result = compute_context_summary(&contexts);

        assert_eq!(result.len(), 3);
        // Check order and content (sorted alphabetically by name)
        assert_eq!(result[0].name, "@home");
        assert_eq!(result[0].duration, Duration::hours(1));

        assert_eq!(result[1].name, "@study");
        assert_eq!(result[1].duration, Duration::minutes(30));

        assert_eq!(result[2].name, "@work");
        assert_eq!(result[2].duration, Duration::hours(2));
    }

    // Tests for format_category_summary and format_category_summary_with_note
    fn get_test_date() -> Date<Local> {
        Local.ymd(2024, 3, 15) // Directly returns Date<Local>
    }

    #[test]
    fn format_category_summary_empty_vec_returns_date_header_only() {
        let ctg_summary_vec: Vec<CategorySummary> = Vec::new();
        let test_date = get_test_date();
        // Date<Local>.to_string() produces "YYYY-MM-DD" which is then bolded.
        let formatted_date_str = test_date.to_string().bold().to_string();

        let result = format_category_summary(ctg_summary_vec, test_date);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], format!("{} (summary)", formatted_date_str));
        assert_eq!(result[1], "");
    }

    #[test]
    fn format_category_summary_with_note_empty_vec_returns_date_header_with_note() {
        let ctg_summary_vec: Vec<CategorySummary> = Vec::new();
        let test_date = get_test_date();
        let note = "(custom test note)";
        let formatted_date_str = test_date.to_string().bold().to_string();

        let result = format_category_summary_with_note(ctg_summary_vec, test_date, note);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], format!("{} {}", formatted_date_str, note));
        assert_eq!(result[1], "");
    }

    #[test]
    fn format_category_summary_basic_vec_returns_formatted_lines() {
        let test_date = get_test_date();
        let date_str_bold = test_date.to_string().to_string();
        let ctg_summary_vec = vec![
            CategorySummary {
                name: "@food",
                duration: Duration::minutes(45),
            },
            CategorySummary {
                name: "@sleep",
                duration: Duration::hours(8),
            },
        ];
        // compute_context_summary sorts its output. If this Vec is manually created and not sorted,
        // and if format_category_summary relies on a specific order without re-sorting, tests could be flaky.
        // However, format_category_summary iterates the input Vec as-is.
        // For this test, providing in alphabetical order as compute_context_summary would.

        let result = format_category_summary(ctg_summary_vec, test_date);

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], format!("{} (summary)", date_str_bold));
        assert_eq!(result[1], "@food: 00h45");
        assert_eq!(result[2], "@sleep: 08h00");
        assert_eq!(result[3], "");
    }

    #[test]
    fn format_category_summary_with_note_basic_vec_returns_formatted_lines_with_note() {
        let test_date = get_test_date();
        let date_str_bold = test_date.to_string().bold().to_string();
        let note = "(daily workout)";
        let ctg_summary_vec = vec![CategorySummary {
            name: "@exercise",
            duration: Duration::hours(1) + Duration::minutes(15),
        }];

        let result = format_category_summary_with_note(ctg_summary_vec, test_date, note);

        assert_eq!(result.len(), 3);
        assert_eq!(result[0], format!("{} {}", date_str_bold, note));
        assert_eq!(result[1], "@exercise: 01h15");
        assert_eq!(result[2], "");
    }
    #[test]
    fn merge_summaries_disjoint_summaries_returns_merged_summary() {
        let mut s1 = HashMap::new();
        s1.insert("@work".to_string(), Duration::hours(1));
        let mut s2 = HashMap::new();
        s2.insert("@work".to_string(), Duration::hours(2));
        s2.insert("@home".to_string(), Duration::hours(1));

        let merged = super::merge_summaries(s1, s2);
        assert_eq!(merged.len(), 2);
        assert_eq!(merged.get("@work"), Some(&Duration::hours(3)));
        assert_eq!(merged.get("@home"), Some(&Duration::hours(1)));
    }

    #[test]
    fn merge_all_summaries_multiple_summaries_returns_accumulated_summary() {
        let mut s1 = HashMap::new();
        s1.insert("@work".to_string(), Duration::hours(1));
        let mut s2 = HashMap::new();
        s2.insert("@work".to_string(), Duration::hours(2));
        let mut s3 = HashMap::new();
        s3.insert("@home".to_string(), Duration::hours(1));

        let merged = super::merge_all_summaries(&vec![s1, s2, s3]);
        assert_eq!(merged.len(), 2);
        assert_eq!(merged.get("@work"), Some(&Duration::hours(3)));
        assert_eq!(merged.get("@home"), Some(&Duration::hours(1)));
    }

    #[test]
    fn merge_summaries_on_same_date_mixed_dates_returns_merged_by_date() {
        let d1 = Local.ymd(2020, 12, 1).and_hms(10, 0, 0);
        let d2 = Local.ymd(2020, 12, 1).and_hms(12, 0, 0); // Same day
        let d3 = Local.ymd(2020, 12, 2).and_hms(10, 0, 0); // Next day

        let mut s1 = HashMap::new();
        s1.insert("@work".to_string(), Duration::hours(1));
        let mut s2 = HashMap::new();
        s2.insert("@work".to_string(), Duration::hours(2));
        let mut s3 = HashMap::new();
        s3.insert("@home".to_string(), Duration::hours(1));

        let input = vec![(d1, s1), (d2, s2), (d3, s3)];
        let merged = super::merge_summaries_on_same_date(input);

        assert_eq!(merged.len(), 2); // 2 days
                                     // First day (merged)
        assert_eq!(merged[0].0.date(), d1.date());
        assert_eq!(merged[0].1.get("@work"), Some(&Duration::hours(3)));
        // Second day
        assert_eq!(merged[1].0.date(), d3.date());
        assert_eq!(merged[1].1.get("@home"), Some(&Duration::hours(1)));
    }
}
