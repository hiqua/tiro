use crate::parse::{LifeLapse, TiroToken};
use chrono::{DateTime, Local};

#[cfg(test)]
mod tests {
    use crate::merge::{are_compatible, Interval};

    use chrono::{Local, TimeZone}; // Removed Date

    #[test]
    fn are_compatible_1() {
        let i1 = Interval {
            start: Local.with_ymd_and_hms(2020, 12, 1, 14, 0, 0).unwrap(),
            end: Local.with_ymd_and_hms(2020, 12, 1, 16, 0, 0).unwrap(),
        };
        let i2 = Interval {
            start: Local.with_ymd_and_hms(2020, 12, 1, 17, 0, 0).unwrap(),
            end: Local.with_ymd_and_hms(2020, 12, 1, 20, 0, 0).unwrap(),
        };
        assert!(are_compatible(vec![&i1, &i2]));
    }
    #[test]
    fn are_compatible_2() {
        let i1 = Interval {
            start: Local.with_ymd_and_hms(2020, 12, 1, 14, 0, 0).unwrap(),
            end: Local.with_ymd_and_hms(2020, 12, 1, 20, 0, 0).unwrap(),
        };
        let i2 = Interval {
            start: Local.with_ymd_and_hms(2020, 12, 1, 17, 0, 0).unwrap(),
            end: Local.with_ymd_and_hms(2020, 12, 1, 20, 0, 0).unwrap(),
        };
        let i3 = Interval {
            start: Local.with_ymd_and_hms(2020, 12, 1, 17, 0, 0).unwrap(),
            end: Local.with_ymd_and_hms(2020, 12, 1, 20, 0, 0).unwrap(),
        };
        assert!(!are_compatible(vec![&i3, &i1, &i2]));
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
