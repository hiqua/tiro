use crate::parse::{LifeLapse, TiroToken};
use chrono::{DateTime, Local};

#[cfg(test)]
mod tests {
    use crate::merge::{are_compatible, Interval};

    use crate::merge::merge_strictly_compatible_lifelapses;
    use crate::parse::{get_life_chunk, LifeChunk, LifeLapse, TimedLifeChunk};
    use chrono::{Date, Local, TimeZone};
    use time::Duration;

    fn create_lifelapse(start_h: u32, duration_h: i64) -> LifeLapse {
        let start = Local.ymd(2020, 12, 1).and_hms(start_h, 0, 0);
        let mut ll = LifeLapse::new(start);
        let lc = get_life_chunk(&format!("{} 0 test", duration_h));
        let tlc = TimedLifeChunk {
            start,
            life_chunk: lc,
        };
        ll.extend(vec![tlc]);
        ll
    }

    #[test]
    fn merge_strictly_compatible_lifelapses_contiguous_inputs_returns_single_merged_lapse() {
        let ll1 = create_lifelapse(10, 1); // 10-11
        let ll2 = create_lifelapse(11, 1); // 11-12

        let merged = merge_strictly_compatible_lifelapses(vec![ll1, ll2]);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].tokens_as_ref().len(), 2);
    }

    #[test]
    fn merge_strictly_compatible_lifelapses_inputs_with_gap_returns_separate_lapses() {
        let ll1 = create_lifelapse(10, 1); // 10-11
        let ll2 = create_lifelapse(12, 1); // 12-13

        let merged = merge_strictly_compatible_lifelapses(vec![ll1, ll2]);
        assert_eq!(merged.len(), 2);
    }

    #[test]
    fn merge_strictly_compatible_lifelapses_overlapping_inputs_returns_separate_lapses() {
        // This function assumes input is sorted and strictly compatible means end == start
        // If they overlap, is_right_before will be false
        let ll1 = create_lifelapse(10, 2); // 10-12
        let ll2 = create_lifelapse(11, 1); // 11-12

        let merged = merge_strictly_compatible_lifelapses(vec![ll1, ll2]);
        assert_eq!(merged.len(), 2);
    }

    #[test]
    fn are_compatible_disjoint_intervals_returns_true() {
        let i1 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
        };
        let i2 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        assert!(are_compatible(vec![&i1, &i2]));
    }
    #[test]
    fn are_compatible_three_intervals_with_overlap_returns_false() {
        let i1 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        let i2 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        let i3 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(20, 0, 0),
        };
        assert!(!are_compatible(vec![&i3, &i1, &i2]));
    }
    #[test]
    fn are_compatible_two_overlapping_intervals_returns_false() {
        let i1 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
        };
        let i2 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(15, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(17, 0, 0),
        };
        assert!(!are_compatible(vec![&i1, &i2]));
    }

    #[test]
    fn are_compatible_touching_intervals_returns_true() {
        let i1 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
        };
        let i2 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(18, 0, 0),
        };
        // Touching intervals are compatible in this logic (end == start is allowed)
        // The check is `if i1.end > i2.start { return false; }`
        assert!(are_compatible(vec![&i1, &i2]));
    }

    #[test]
    fn are_compatible_empty_input_returns_true() {
        assert!(are_compatible(vec![]));
    }

    #[test]
    fn are_compatible_single_interval_returns_true() {
        let i1 = Interval {
            start: Local.ymd(2020, 12, 1).and_hms(14, 0, 0),
            end: Local.ymd(2020, 12, 1).and_hms(16, 0, 0),
        };
        assert!(are_compatible(vec![&i1]));
    }
}

struct Interval {
    start: DateTime<Local>,
    end: DateTime<Local>,
}

/// these functions are expensive with sorting and copying
fn are_compatible(a: Vec<&Interval>) -> bool {
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
fn merge_plans_interval(plans: Vec<(Interval, Vec<TiroToken>)>) -> Vec<TiroToken> {
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
