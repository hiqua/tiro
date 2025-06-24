use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::iter::Sum;

use colored::Colorize;
use time::Duration;

use crate::config::Category;
use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk};
use chrono::{Date, DateTime, Datelike, Local};
use std::ops::Add;

#[cfg(test)]
mod tests {
    use crate::config::Quadrant;
    use crate::parse::get_life_chunk;
    use crate::parse::{LifeChunk, TimedLifeChunk}; // Keep this for existing tests if they use it.
    use crate::summary::{
        compute_context_summary, compute_summary, format_category_summary,
        format_category_summary_with_note, format_duration, CategorySummary, Summary,
        merge_summaries, merge_summaries_on_same_date, // Added for new tests
    };
    use chrono::{Date, Datelike, Local, TimeZone, Utc, NaiveDate}; // Added NaiveDate for date generation
    use colored::Colorize;
    use std::collections::HashMap;
    use time::Duration;
    use quickcheck::{Arbitrary, Gen, TestResult}; // Added for QuickCheck
    use quickcheck_macros::quickcheck;         // Added for QuickCheck macro

    // Need Arbitrary for TimedLifeChunk for some of the tests.
    // If it's not already in scope from crate::parse, we might need to re-declare or import.
    // Assuming it's available or will be made available.
    // For now, let's ensure our Arbitrary impl for TimedLifeChunk from parse.rs is accessible.
    // This might require making the Arbitrary impls in parse.rs pub(crate) or pub if not already.
    // For simplicity, let's assume they are accessible. If not, compilation will guide us.

    // Helper to create TimedLifeChunk for tests using get_life_chunk
    // The start time of TimedLifeChunk is not used by compute_summary, so it can be arbitrary.
    fn create_timed_chunk_from_line(line_for_get_life_chunk: &str) -> crate::parse::TimedLifeChunk { // Explicit path
        crate::parse::TimedLifeChunk { // Ensure we are using the type from parse.rs
            start: Local::now(), // Arbitrary, not used by the function under test
            life_chunk: get_life_chunk(line_for_get_life_chunk),
        }
    }

    #[quickcheck]
    fn prop_format_duration_positive(total_minutes_abs: u32) -> bool {
        // Keep total minutes within a reasonable range, e.g., less than 1000 hours to avoid overflow issues with format.
        // 1000 hours = 60000 minutes. u32 max is large, so let's cap it.
        let total_minutes = (total_minutes_abs % 60000) as i64;
        let duration = Duration::minutes(total_minutes);

        let formatted = format_duration(duration);
        let expected_hours = total_minutes / 60;
        let expected_minutes_in_hour = total_minutes % 60;

        // Expected format: XXhMM or XXXhMM etc.
        // Example: 02h15, 25h00, 100h00

        let parts: Vec<&str> = formatted.split('h').collect();
        if parts.len() != 2 {
            return false; // Should always have one 'h'
        }
        let parsed_hours_str = parts[0];
        let parsed_minutes_str = parts[1];

        if parsed_minutes_str.len() != 2 {
            return false; // Minutes part should always be 2 digits
        }
        // Hours part can be 2 or more digits if hours > 99, or 2 digits if hours < 10 (e.g. "02")
        if expected_hours < 10 && parsed_hours_str.len() != 2 { // e.g. "00" to "09"
             //format_duration prepends '0' if h.len() == 1. So "2h" becomes "02h".
            return false;
        }
         if expected_hours >= 10 && parsed_hours_str.len() != expected_hours.to_string().len() {
            return false;
        }


        if let (Ok(parsed_h), Ok(parsed_m)) = (parsed_hours_str.parse::<i64>(), parsed_minutes_str.parse::<i64>()) {
            parsed_h == expected_hours && parsed_m == expected_minutes_in_hour
        } else {
            false
        }
    }

    #[quickcheck]
    fn prop_compute_summary_category_duration(chunks: Vec<crate::parse::TimedLifeChunk>) -> bool {
        if chunks.is_empty() {
            return compute_summary(&chunks).is_empty();
        }

        let summary = compute_summary(&chunks);

        // For every chunk and every category within that chunk,
        // check if the summary reflects its duration correctly.
        let mut expected_category_durations: HashMap<String, Duration> = HashMap::new();
        for tlc in &chunks {
            for cat in &tlc.life_chunk.categories {
                let entry = expected_category_durations.entry(cat.clone()).or_insert_with(Duration::zero);
                *entry = *entry + tlc.life_chunk.duration;
            }
        }

        if summary.len() != expected_category_durations.len() {
            return false;
        }

        for (cat, expected_dur) in expected_category_durations {
            match summary.get(&cat) {
                Some(actual_dur) => {
                    if *actual_dur != expected_dur {
                        return false;
                    }
                }
                None => return false, // Category missing in summary
            }
        }
        true
    }
    // Add prop_compute_summary_total_duration_consistent if a clear invariant is established.
    // The current thinking is that prop_compute_summary_category_duration is more direct.

    // Helper to generate a Summary for tests
    fn arbitrary_summary(g: &mut Gen) -> Summary {
        let mut summary = Summary::new();
        let num_entries = usize::arbitrary(g) % 10; // Max 9 entries
        for _ in 0..num_entries {
            let cat: String = String::arbitrary(g).chars().take(10).filter(|c| c.is_alphanumeric()).collect();
            // Duration in minutes, positive, up to e.g. 10 hours (600 minutes)
            let duration_minutes = (i64::arbitrary(g) % 600).abs();
            if !cat.is_empty() { // Ensure category is not empty
                 summary.insert(cat, Duration::minutes(duration_minutes));
            }
        }
        summary
    }

    #[derive(Clone, Debug)]
    struct ArbitrarySummary(Summary);

    impl Arbitrary for ArbitrarySummary {
        fn arbitrary(g: &mut Gen) -> Self {
            ArbitrarySummary(arbitrary_summary(g))
        }
    }

    #[quickcheck]
    fn prop_merge_summaries_associativity_and_content(s1_wrapper: ArbitrarySummary, s2_wrapper: ArbitrarySummary, s3_wrapper: ArbitrarySummary) -> bool {
        let s1 = s1_wrapper.0;
        let s2 = s2_wrapper.0;
        let s3 = s3_wrapper.0;

        // Property: (s1 + s2) + s3 == s1 + (s2 + s3) (associativity)
        // And also check content: sum of durations for each key.

        let merged12 = merge_summaries(s1.clone(), s2.clone());
        let result_left_assoc = merge_summaries(merged12, s3.clone());

        let merged23 = merge_summaries(s2.clone(), s3.clone());
        let result_right_assoc = merge_summaries(s1.clone(), merged23);

        if result_left_assoc != result_right_assoc {
            return false; // Associativity failed
        }

        // Check content for result_left_assoc (since they should be equal)
        let mut expected_merged_content: HashMap<String, Duration> = HashMap::new();
        for (k,v) in s1.iter().chain(s2.iter()).chain(s3.iter()) {
            let entry = expected_merged_content.entry(k.clone()).or_insert_with(Duration::zero);
            *entry = *entry + *v;
        }

        if result_left_assoc.len() != expected_merged_content.len() {
            return false;
        }
        for (cat, expected_dur) in expected_merged_content {
            if result_left_assoc.get(&cat) != Some(&expected_dur) {
                return false;
            }
        }
        true
    }

    // Wrapper type for generating test data for merge_summaries_on_same_date
    #[derive(Clone, Debug)]
    struct DatedSummaries(Vec<(super::Timestamp, Summary)>); // Use super::Timestamp

    impl Arbitrary for DatedSummaries {
        fn arbitrary(g: &mut Gen) -> Self {
            let num_entries = usize::arbitrary(g) % 7 + 1; // 1-7 entries
            let mut entries = Vec::new();
            let base_date_naive = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(); // Fixed base date

            for _ in 0..num_entries {
                // Generate a day offset, e.g., 0 to 2, to ensure some same dates
                let day_offset = i64::arbitrary(g) % 3;
                let current_date_naive = base_date_naive + Duration::days(day_offset.abs());
                // Attach a fixed time (e.g., noon) to the NaiveDate to create a DateTime<Local>
                let timestamp = Local.from_local_datetime(&current_date_naive.and_hms_opt(12,0,0).unwrap()).unwrap();

                entries.push((timestamp, arbitrary_summary(g)));
            }
            // Sort by timestamp to mimic the precondition of merge_summaries_on_same_date if any
            // The function itself sorts by date as part of its logic, but input order might matter for some impls.
            // The current implementation of merge_summaries_on_same_date does not require pre-sorting.
            entries.sort_by_key(|k| k.0);
            DatedSummaries(entries)
        }
    }

    #[quickcheck]
    fn prop_merge_summaries_on_same_date_merges(dated_summaries_wrapper: DatedSummaries) -> bool {
        let dated_summaries = dated_summaries_wrapper.0;
        if dated_summaries.is_empty() {
            return merge_summaries_on_same_date(dated_summaries).is_empty();
        }

        let merged = merge_summaries_on_same_date(dated_summaries.clone());

        // 1. Check that all dates in merged output are unique.
        let mut seen_dates = std::collections::HashSet::new();
        for (ts, _) in &merged {
            if !seen_dates.insert(ts.date().naive_local()) { // Changed to date().naive_local()
                return false; // Duplicate date found in output
            }
        }

        // 2. Check that the content for each date is correctly merged.
        let mut expected_merged_by_date: HashMap<NaiveDate, Summary> = HashMap::new();
        for (ts, summary) in dated_summaries {
            let date_key = ts.date().naive_local(); // Changed to date().naive_local()
            let target_summary = expected_merged_by_date.entry(date_key).or_insert_with(HashMap::new);
            for (cat, dur) in summary {
                let entry = target_summary.entry(cat).or_insert_with(Duration::zero);
                *entry = *entry + dur;
            }
        }

        if merged.len() != expected_merged_by_date.len() {
             // println!("Length mismatch: merged.len()={}, expected.len()={}", merged.len(), expected_merged_by_date.len());
            return false; // Number of unique dates doesn't match
        }

        for (ts, actual_summary) in merged {
            match expected_merged_by_date.get(&ts.date().naive_local()) { // Changed to date().naive_local()
                Some(expected_summary) => {
                    if actual_summary.len() != expected_summary.len() {
                        // println!("Summary length mismatch for date {}", ts.date().naive_local());
                        return false;
                    }
                    for (cat, expected_dur) in expected_summary {
                        if actual_summary.get(cat) != Some(expected_dur) {
                            // println!("Duration mismatch for cat {} on date {}", ts.date().naive_local());
                            return false;
                        }
                    }
                }
                None => return false, // Date in merged output not found in expected map
            }
        }
        true
    }


    #[test]
    fn format_duration_longer_than_a_day() {
        let duration = Duration::hours(25);
        assert_eq!(format_duration(duration), "25h00");
    }

    #[test]
    fn format_duration_prepends_a_0() {
        let duration = Duration::hours(2) + Duration::minutes(15);
        assert_eq!(format_duration(duration), "02h15");
    }

    #[test]
    fn test_compute_summary_basic() {
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
    fn test_compute_summary_empty_input() {
        let timed_life_chunks: Vec<TimedLifeChunk> = vec![];
        let summary = compute_summary(&timed_life_chunks);
        assert!(
            summary.is_empty(),
            "Summary should be empty for empty input"
        );
    }

    #[test]
    fn test_compute_summary_no_categories_in_chunks() {
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
    fn test_compute_summary_multiple_categories_in_single_chunk() {
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
    fn test_compute_summary_zero_duration_chunks() {
        let timed_life_chunks = vec![
            create_timed_chunk_from_line("0 0 Task 1 zero duration @work @projA"),
            create_timed_chunk_from_line("1 0 Task 2 normal duration @work"),
            create_timed_chunk_from_line("0 0 Task 3 zero duration @home"),
        ];

        let summary = compute_summary(&timed_life_chunks);

        let mut expected_summary = HashMap::new();
        expected_summary.insert("@work".to_string(), Duration::minutes(0 + 60));
        expected_summary.insert("@projA".to_string(), Duration::minutes(0));
        expected_summary.insert("@home".to_string(), Duration::minutes(0));

        assert_eq!(summary.len(), expected_summary.len());
        assert_eq!(summary.get("@work"), expected_summary.get("@work"));
        assert_eq!(summary.get("@projA"), expected_summary.get("@projA"));
        assert_eq!(summary.get("@home"), expected_summary.get("@home"));
    }

    // Tests for compute_context_summary
    #[test]
    fn test_compute_context_summary_empty() {
        let contexts: HashMap<String, Duration> = HashMap::new();
        let result = compute_context_summary(&contexts);
        assert!(
            result.is_empty(),
            "Expected empty vector for empty input HashMap"
        );
    }

    #[test]
    fn test_compute_context_summary_basic_sorted() {
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
    fn test_format_category_summary_empty() {
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
    fn test_format_category_summary_with_note_empty() {
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
    fn test_format_category_summary_basic() {
        let test_date = get_test_date();
        let date_str_bold = test_date.to_string().bold().to_string();
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
    fn test_format_category_summary_with_note_basic() {
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
}

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
