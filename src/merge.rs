use crate::parse::{LifeLapse, TiroToken};
use chrono::{DateTime, Local};

#[cfg(test)]
mod tests {
    // Corrected imports: merge_strictly_compatible_lifelapses is the only public item needed from crate::merge here for tests.
    // are_compatible and TestInterval are local to this test module.
    use crate::merge::merge_strictly_compatible_lifelapses;
    use crate::parse::{LifeChunk, LifeLapse, TimedLifeChunk, TiroToken}; // Added TiroToken
    use crate::config::Quadrant; // Quadrant is defined in config.rs
    use crate::summary::Timestamp; // This is type alias for chrono::DateTime<Local>
    use time::Duration as OldDuration;

    use chrono::{Date, Local, TimeZone, Duration as ChronoDuration};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;

    // Newtype wrapper for Timestamp (chrono::DateTime<Local>) for Arbitrary impl
    #[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
    struct ArbitraryTimestamp(Timestamp);

    impl Arbitrary for ArbitraryTimestamp {
        fn arbitrary(g: &mut Gen) -> Self {
            let now_ts = Local::now().timestamp();
            let one_year_secs = 365 * 24 * 60 * 60;
            let secs = i64::arbitrary(g) % (one_year_secs * 2) + (now_ts - one_year_secs);
            let nsecs = u32::arbitrary(g) % 1_000_000_000;
            // Use .earliest().unwrap() or .latest().unwrap() for LocalResult
            ArbitraryTimestamp(Local.timestamp_opt(secs, nsecs).earliest().unwrap_or_else(|| Local::now()))
        }
    }

    // Newtype wrapper for OldDuration (time::Duration) for Arbitrary impl
    #[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
    struct ArbitraryOldDuration(OldDuration);

    impl Arbitrary for ArbitraryOldDuration {
        fn arbitrary(g: &mut Gen) -> Self {
            // Generate u32 and cast to i64 to avoid i64::MIN.abs() overflow
            let secs = (u32::arbitrary(g) % (60 * 60 * 5)) as i64; // Max 5 hours, non-negative
            ArbitraryOldDuration(OldDuration::seconds(secs))
        }
    }

    // LifeChunk's Arbitrary is already defined in parse.rs tests.
    // We assume it's accessible or we'd need to move it to a shared location.
    // For TimedLifeChunk and LifeLapse, we'll use the newtypes.

    impl Arbitrary for TimedLifeChunk {
        fn arbitrary(g: &mut Gen) -> Self {
            TimedLifeChunk {
                // Use the newtype ArbitraryTimestamp and then extract the inner value
                start: ArbitraryTimestamp::arbitrary(g).0,
                life_chunk: LifeChunk::arbitrary(g), // Assumes LifeChunk::arbitrary is available
            }
        }
    }

    impl Arbitrary for LifeLapse {
        fn arbitrary(g: &mut Gen) -> Self {
            let start_time = ArbitraryTimestamp::arbitrary(g).0;
            let mut current_time = start_time;
            let num_chunks = usize::arbitrary(g) % 5;

            let tokens: Vec<TimedLifeChunk> = (0..num_chunks).map(|_| {
                let mut lc = LifeChunk::arbitrary(g);
                // Ensure duration is positive and not excessively long
                let arbitrary_duration = ArbitraryOldDuration::arbitrary(g).0;
                if arbitrary_duration < OldDuration::zero() || arbitrary_duration > OldDuration::hours(24) {
                     lc.duration = OldDuration::minutes( (u8::arbitrary(g) % 120 + 1) as i64 );
                } else {
                    lc.duration = arbitrary_duration;
                }

                let tlc = TimedLifeChunk { start: current_time, life_chunk: lc };
                // Handle potential overflow if current_time + duration is too large
                if let Some(next_time) = current_time.checked_add_signed(tlc.life_chunk.duration) {
                    current_time = next_time;
                } else {
                    // If overflow, end token generation or handle appropriately
                    // For simplicity here, we might just not advance current_time further for this token in a real scenario,
                    // or cap duration. But for Arbitrary, better to generate valid sequences.
                    // This might mean limiting num_chunks or individual durations more strictly.
                    // For now, this will lead to some tokens potentially overlapping if not careful.
                    // A robust solution would regenerate or cap.
                }
                tlc
            }).collect();

            let mut lapse = LifeLapse::new(start_time);
            if !tokens.is_empty() {
                // Before extending, ensure tokens are chronologically ordered by their start times
                // and that their durations are correctly reflected if current_time was capped due to overflow.
                // This part needs more robust handling for true Arbitrary generation.
                // For now, we assume the simple current_time advancement is mostly okay for testing purposes.
                lapse.extend(tokens);
            }
            lapse
        }
    }

    // Interval is a test-local struct, so its Arbitrary impl is fine here.
    // Add #[derive(Debug, Clone)] for QuickCheck output and Arbitrary requirement.
    #[derive(Debug, Clone)]
    struct TestInterval { // Renamed from Interval to TestInterval
        start: chrono::DateTime<Local>,
        end: chrono::DateTime<Local>,
    }

    // Implement Arbitrary for TestInterval
    impl Arbitrary for TestInterval {
        fn arbitrary(g: &mut Gen) -> Self {
            let start_secs = i64::arbitrary(g) % (chrono::Utc::now().timestamp() + 31_536_000);
            let duration_secs = u32::arbitrary(g) % (3600 * 24 * 7);

            let start_local_result = Local.timestamp_opt(start_secs, 0);
            let start = start_local_result.earliest().unwrap_or_else(|| Local::now()); // Handle LocalResult

            let end = start + ChronoDuration::seconds(duration_secs as i64);

            if start <= end {
                TestInterval { start, end }
            } else {
                TestInterval { start: end, end: start }
            }
        }
    }

    #[quickcheck]
    fn prop_are_compatible_empty_list() -> bool {
        // are_compatible expects a Vec<&Interval> where Interval is the test-local one.
        // So this should be Vec<&TestInterval>
        are_compatible(vec![])
    }

    #[quickcheck]
    fn prop_are_compatible_single_interval(interval: TestInterval) -> bool {
        are_compatible(vec![&interval])
    }

    #[quickcheck]
    fn prop_are_compatible_sorted_non_overlapping(mut intervals: Vec<TestInterval>) -> bool {
        if intervals.len() < 2 {
            return true;
        }
        intervals.sort_by_key(|i| i.start);

        for i in 1..intervals.len() {
            if intervals[i-1].end > intervals[i].start {
                intervals[i].start = intervals[i-1].end + ChronoDuration::seconds(1);
                if intervals[i].end < intervals[i].start {
                    // Ensure the duration added is i64 for ChronoDuration::seconds
                    // Create a Gen instance if not available in function signature
                    let mut g_temp = Gen::new(10); // Small size for simple u32
                    let random_seconds = (u32::arbitrary(&mut g_temp) % 3600) as i64;
                    intervals[i].end = intervals[i].start + ChronoDuration::seconds(random_seconds);
                }
            }
        }

        let refs: Vec<&TestInterval> = intervals.iter().collect();
        are_compatible(refs)
    }

    #[quickcheck]
    fn prop_are_compatible_implies_sorted_non_overlapping(mut intervals: Vec<TestInterval>) -> bool {
        let refs: Vec<&TestInterval> = intervals.iter().collect();
        if !are_compatible(refs.clone()) {
            return true;
        }

        intervals.sort_by_key(|i| i.start);
        for i in 0..intervals.len().saturating_sub(1) {
            if intervals[i].end > intervals[i+1].start {
                return false;
            }
        }
        true
    }

    // --- Tests for merge_strictly_compatible_lifelapses ---

    #[quickcheck]
    fn prop_merge_empty_list(lapses: Vec<LifeLapse>) -> bool {
        if lapses.is_empty() {
            merge_strictly_compatible_lifelapses(lapses).is_empty()
        } else {
            true // Property only for empty case
        }
    }

    #[quickcheck]
    fn prop_merge_single_lapse(lapse: LifeLapse) -> bool {
        if lapse.is_empty() { return true; } // Skip empty lapses for this specific property logic
        let result = merge_strictly_compatible_lifelapses(vec![lapse.clone()]);
        result.len() == 1 &&
        result[0].start() == lapse.start() &&
        result[0].tokens_as_ref().len() == lapse.tokens_as_ref().len() // Basic check
    }

    #[quickcheck]
    fn prop_merge_total_duration_preserved(mut lapses: Vec<LifeLapse>) -> bool {
        lapses.retain(|ll| !ll.is_empty()); // Remove empty lapses as they don't contribute to duration
        if lapses.is_empty() { return true; }

        lapses.sort_by_key(|ll| ll.start()); // merge function expects sorted input

        let total_input_duration: OldDuration = lapses.iter()
            .flat_map(|ll| ll.tokens_as_ref())
            .map(|tlc| tlc.life_chunk.duration)
            .fold(OldDuration::zero(), |acc, d| acc + d);

        let merged_lapses = merge_strictly_compatible_lifelapses(lapses);

        let total_output_duration: OldDuration = merged_lapses.iter()
            .flat_map(|ll| ll.tokens_as_ref())
            .map(|tlc| tlc.life_chunk.duration)
            .fold(OldDuration::zero(), |acc, d| acc + d);

        // Allow for minor discrepancies if dealing with float conversions, but duration here is discrete.
        total_input_duration == total_output_duration
    }

    #[quickcheck]
    fn prop_merge_all_tokens_present(mut lapses: Vec<LifeLapse>) -> bool {
        lapses.retain(|ll| !ll.is_empty());
        if lapses.is_empty() { return true; }
        lapses.sort_by_key(|ll| ll.start());

        let input_token_count = lapses.iter().map(|ll| ll.tokens_as_ref().len()).sum::<usize>();

        let merged_lapses = merge_strictly_compatible_lifelapses(lapses);
        let output_token_count = merged_lapses.iter().map(|ll| ll.tokens_as_ref().len()).sum::<usize>();

        input_token_count == output_token_count
    }

    #[quickcheck]
    fn prop_merge_no_compatible_lapses(mut lapses: Vec<LifeLapse>) -> bool {
        lapses.retain(|ll| !ll.is_empty());
        if lapses.len() < 2 { return true; } // Need at least two to check non-compatibility

        lapses.sort_by_key(|ll| ll.start());

        // Ensure no lapses are strictly compatible by adding a gap
        let mut non_compatible_lapses = Vec::new();
        if let Some(first) = lapses.first().cloned() {
            non_compatible_lapses.push(first);
            let mut last_end = non_compatible_lapses.last().unwrap().start() +
                               non_compatible_lapses.last().unwrap().tokens_as_ref().iter().map(|t| t.life_chunk.duration).fold(OldDuration::zero(), |a,b| a+b);

            for i in 1..lapses.len() {
                let current_ll = lapses[i].clone(); // Removed mut
                let current_ll_duration = current_ll.tokens_as_ref().iter().map(|t| t.life_chunk.duration).fold(OldDuration::zero(), |a,b| a+b);

                // Create a new start time for current_ll that is definitely after last_end
                let new_start = last_end + OldDuration::minutes(10); // Add a 10-minute gap

                // Reconstruct current_ll with new start times for its tokens
                let mut new_tokens = Vec::new();
                let mut token_time = new_start;
                for token in current_ll.tokens() { // Consumes tokens
                    new_tokens.push(TimedLifeChunk { start: token_time, life_chunk: token.life_chunk });
                    token_time = token_time + new_tokens.last().unwrap().life_chunk.duration;
                }

                let mut new_ll = LifeLapse::new(new_start);
                if !new_tokens.is_empty() {
                    new_ll.extend(new_tokens);
                }

                if !new_ll.is_empty() { // Only add if it has tokens
                    non_compatible_lapses.push(new_ll.clone());
                    last_end = new_ll.start() + current_ll_duration; // current_ll_duration is from original tokens
                }
            }
        }
        non_compatible_lapses.retain(|ll| !ll.is_empty()); // Clean up again
        if non_compatible_lapses.len() < 2 { return true; }


        let merged = merge_strictly_compatible_lifelapses(non_compatible_lapses.clone());

        // If none were compatible, the number of lapses should be the same.
        // Their contents should also match, which is harder to check without full LifeLapse equality.
        // For now, checking length is a good first step.
        // And check that they are still ordered by start time.
        let mut sorted_merged = merged.clone();
        sorted_merged.sort_by_key(|ll| ll.start());

        merged.len() == non_compatible_lapses.len() && merged == sorted_merged
    }

    #[quickcheck]
    fn prop_merge_all_lapses_compatible(count: u8) -> bool {
        if count == 0 { return true; }
        let num_lapses = (count % 5) + 1; // 1 to 5 lapses

        let mut lapses = Vec::new();
        let mut current_time = Local.ymd(2024, 1, 1).and_hms(9, 0, 0);

        for _ in 0..num_lapses {
            let mut tokens = Vec::new();
            // Ensure Gen::new receives a usize
            let mut g_prop = Gen::new(100); // Create a generator for this property test
            let num_tokens_in_lapse = (u8::arbitrary(&mut g_prop) % 3) + 1; // 1 to 3 tokens per lapse
            let lapse_start_time = current_time;

            for j in 0..num_tokens_in_lapse {
                // Ensure Gen::new receives a usize and OldDuration::minutes receives i64
                let random_minute_val = (u8::arbitrary(&mut g_prop) % 59) + 1;
                let duration = OldDuration::minutes(random_minute_val as i64);
                let desc = format!("Activity {}", j);

                // Use arbitrary for LifeChunk and override relevant fields
                let mut lc = LifeChunk::arbitrary(&mut g_prop);
                lc.description = desc;
                lc.duration = duration;
                lc.categories = vec!["@Test".to_string()];
                lc.quadrant = Quadrant::Q1;
                lc.user_provided_quadrant = true;
                // lc.input will be handled by its Arbitrary impl or remain as generated

                tokens.push(TimedLifeChunk { start: current_time, life_chunk: lc });
                current_time = current_time + duration;
            }

            if !tokens.is_empty() {
                let mut lapse = LifeLapse::new(lapse_start_time);
                lapse.extend(tokens);
                lapses.push(lapse);
            }
        }

        if lapses.is_empty() { return true; } // Should not happen with count >=1

        let merged = merge_strictly_compatible_lifelapses(lapses);

        // If all are compatible and contiguous, there should be only one lapse in the result.
        merged.len() == 1 || num_lapses == 0 // Or if original was empty
    }


    #[test]
    fn are_compatible_1() {
        let i1 = TestInterval { // Changed to TestInterval
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
        };
        let i2 = TestInterval { // Changed to TestInterval
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        assert!(are_compatible(vec![&i1, &i2]));
    }
    #[test]
    fn are_compatible_2() {
        let i1 = TestInterval { // Changed to TestInterval
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        let i2 = TestInterval { // Changed to TestInterval
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        let i3 = TestInterval { // Changed to TestInterval
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        assert!(!are_compatible(vec![&i3, &i1, &i2]));
    }

    // Helper functions moved inside mod tests

    /// these functions are expensive with sorting and copying
    fn are_compatible(a: Vec<&TestInterval>) -> bool {
        let mut s = vec![];
        {
            s.extend(a);
            s.sort_by_key(|i| i.start);
        }

        for pair in s.windows(2) {
            let i1 = pair.first().unwrap();
            let i2 = pair.get(1).unwrap();
            if i1.end > i2.start {
                return false;
            }
        }
        true
    }

    #[allow(dead_code)]
    fn merge_plans_interval(plans: Vec<(TestInterval, Vec<TiroToken>)>) -> Vec<TiroToken> {
        {
            let int = plans.iter().map(|(i, _)| i).collect();
            assert!(are_compatible(int));
        }

        let mut all_tokens = vec![];
        {
            let mut pl = vec![];
            pl.extend(plans);
            pl.sort_by_key(|(i, _)| i.start);

            for (_, v) in pl {
                all_tokens.extend(v);
            }
        }
        all_tokens
    }
} // End of mod tests

/// assumes that input is sorted
pub fn merge_strictly_compatible_lifelapses(lifelapses: Vec<LifeLapse>) -> Vec<LifeLapse> {
    let mut result = vec![];
    let mut it = lifelapses.iter();

    while let Some(ll) = it.next() {
        let mut new_ll = ll.clone();

        for o_ll in it.as_slice() {
            if new_ll.is_right_before(o_ll) {
                new_ll.extend(o_ll.tokens_as_ref().clone());
                it.next();
            } else {
                break;
            }
        }
        if !new_ll.is_empty() {
            result.push(new_ll);
        }
    }

    result
}
