use std::collections::VecDeque;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::thread;

use std::time::Duration as StdDuration;

use chrono::{Duration, Local};
// use notify_rust::Notification;
use chrono::OutOfRangeError;

use crate::parse::{LifeChunk, TimedLifeChunk};

use notify_rust::Notification;
use std::thread::JoinHandle;

pub fn spawn_notification_thread(q: VecDeque<TimedLifeChunk>, rx: Receiver<()>) -> JoinHandle<()> {
    // XXX: should join somehow
    let mut lc_ls = VecDeque::new();
    for tlc in q {
        lc_ls.push_back((tlc.start, tlc.life_chunk));
    }

    thread::spawn(move || {
        while let Some((t, lc)) = lc_ls.pop_front() {
            let notify = || notify(lc);

            {
                let upper_bound = Local::now() - Duration::minutes(5);
                if t < upper_bound {
                    continue;
                } else if t < Local::now() {
                    notify();
                    continue;
                }
            }

            // XXX: probably buggy in case of timeout of 0
            let delay = Duration::minutes(5);
            let time_to_notification = unwrap_dur((t - Local::now() - delay).to_std());
            match rx.recv_timeout(time_to_notification) {
                Ok(()) | Err(RecvTimeoutError::Disconnected) => {
                    return;
                }
                Err(RecvTimeoutError::Timeout) => {
                    if notify() {
                        return;
                    }
                }
            }
        }
        notify_nothing();
    })
}

fn notify(lc: LifeChunk) -> bool {
    let mut n = Notification::new();
    n.summary("Tiro: Time to switch!").timeout(10000);
    n.body(&format!("Next activity: {}", lc.get_input()));
    let r = n.show();

    r.is_err()
}

fn notify_nothing() -> bool {
    let mut n = Notification::new();
    n.summary("Tiro: No activity planned!").timeout(100_000);

    let r = n.show();

    r.is_err()
}

fn unwrap_dur(r: Result<StdDuration, OutOfRangeError>) -> StdDuration {
    match r {
        Ok(rr) => rr,
        Err(_) => StdDuration::new(0, 0),
    }
}
