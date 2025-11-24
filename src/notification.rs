use std::collections::VecDeque;
use std::sync::mpsc::{Receiver, RecvTimeoutError};
use std::thread;

use std::time::Duration as StdDuration;

use chrono::{Duration, Local};
// use notify_rust::Notification;
use time::OutOfRangeError;

use crate::parse::{LifeChunk, TimedLifeChunk};

use notify_rust::Notification;
use std::thread::JoinHandle;

pub trait NotificationHandler {
    fn show_notification(&self, summary: &str, body: &str) -> Result<(), notify_rust::error::Error>;
}

struct DefaultNotificationHandler;

impl NotificationHandler for DefaultNotificationHandler {
    fn show_notification(&self, summary: &str, body: &str) -> Result<(), notify_rust::error::Error> {
        Notification::new()
            .summary(summary)
            .body(body)
            .timeout(10000)
            .show()
            .map(|_| ())
    }
}

pub fn spawn_notification_thread(
    q: VecDeque<TimedLifeChunk>,
    rx: Receiver<()>,
) -> JoinHandle<()> {
    spawn_notification_thread_with_handler(q, rx, DefaultNotificationHandler)
}

pub fn spawn_notification_thread_with_handler<T: NotificationHandler + Send + 'static>(
    q: VecDeque<TimedLifeChunk>,
    rx: Receiver<()>,
    notification_handler: T,
) -> JoinHandle<()> {
    let mut lc_ls = VecDeque::new();
    for tlc in q {
        lc_ls.push_back((tlc.start, tlc.life_chunk));
    }

    thread::spawn(move || {
        while let Some((t, lc)) = lc_ls.pop_front() {
            let notify = || notify(&notification_handler, lc.clone());

            let now = Local::now();
            let upper_bound = now - Duration::minutes(5);

            if t < upper_bound {
                continue;
            } else if t <= now {
                if notify() {
                    return;
                }
                continue;
            }

            let delay = Duration::minutes(5);
            let time_to_notification = (t - now - delay).to_std();

            if let Ok(time_to_notification) = time_to_notification {
                match rx.recv_timeout(time_to_notification) {
                    Ok(()) | Err(RecvTimeoutError::Disconnected) => return,
                    Err(RecvTimeoutError::Timeout) => {
                        if notify() {
                            return;
                        }
                    }
                }
            }
        }
        notify_nothing(&notification_handler);
    })
}


fn notify<T: NotificationHandler>(notification_handler: &T, lc: LifeChunk) -> bool {
    notification_handler
        .show_notification("Tiro: Time to switch!", &format!("Next activity: {}", lc.get_input()))
        .is_err()
}

fn notify_nothing<T: NotificationHandler>(notification_handler: &T) -> bool {
    notification_handler
        .show_notification("Tiro: No activity planned!", "")
        .is_err()
}

fn unwrap_dur(r: Result<StdDuration, OutOfRangeError>) -> StdDuration {
    match r {
        Ok(rr) => rr,
        Err(_) => StdDuration::new(0, 0),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Duration;
    use crate::parse::LifeChunk;
    use std::sync::{Arc, Mutex};
    use notify_rust::error::Error;

    struct MockNotificationHandler {
        invocations: Arc<Mutex<Vec<(String, String)>>>,
    }

    impl NotificationHandler for MockNotificationHandler {
        fn show_notification(&self, summary: &str, body: &str) -> Result<(), Error> {
            let mut invocations = self.invocations.lock().unwrap();
            invocations.push((summary.to_string(), body.to_string()));
            Ok(())
        }
    }

    #[test]
    fn test_notify() {
        let invocations = Arc::new(Mutex::new(Vec::new()));
        let handler = MockNotificationHandler {
            invocations: invocations.clone(),
        };
        let lc = LifeChunk::new(
            "test input".to_string(),
            time::Duration::minutes(1),
            vec![],
            crate::config::Quadrant::Q1,
            false,
            "test input".to_string(),
        );

        notify(&handler, lc);

        let invocations = invocations.lock().unwrap();
        assert_eq!(invocations.len(), 1);
        assert_eq!(invocations[0].0, "Tiro: Time to switch!");
        assert_eq!(invocations[0].1, "Next activity: test input");
    }

    #[test]
    fn test_notify_nothing() {
        let invocations = Arc::new(Mutex::new(Vec::new()));
        let handler = MockNotificationHandler {
            invocations: invocations.clone(),
        };

        notify_nothing(&handler);

        let invocations = invocations.lock().unwrap();
        assert_eq!(invocations.len(), 1);
        assert_eq!(invocations[0].0, "Tiro: No activity planned!");
        assert_eq!(invocations[0].1, "");
    }

    #[test]
    fn test_spawn_notification_thread_with_handler() {
        let invocations = Arc::new(Mutex::new(Vec::new()));
        let handler = MockNotificationHandler {
            invocations: invocations.clone(),
        };
        let (_tx, rx) = std::sync::mpsc::channel();

        let q = VecDeque::new();

        let handle = spawn_notification_thread_with_handler(q, rx, handler);
        handle.join().unwrap();

        let invocations = invocations.lock().unwrap();
        assert_eq!(invocations.len(), 1);
        assert_eq!(invocations[0].0, "Tiro: No activity planned!");
    }

    #[test]
    fn test_unwrap_dur_ok() {
        let dur = StdDuration::new(1, 0);
        let result = Ok(dur);
        assert_eq!(unwrap_dur(result), dur);
    }

    #[test]
    fn test_unwrap_dur_err() {
        let neg_chrono_dur = Duration::seconds(-1);
        let result = neg_chrono_dur.to_std();
        assert!(result.is_err());
        assert_eq!(unwrap_dur(result), StdDuration::new(0, 0));
    }
}
