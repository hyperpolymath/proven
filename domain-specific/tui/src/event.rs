// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Event handling for Proven TUI

use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use anyhow::Result;
use crossterm::event::{self, Event as CrosstermEvent, KeyEvent, MouseEvent};

/// Terminal events
#[derive(Debug)]
pub enum Event {
    /// Terminal tick (for animations and polling)
    Tick,
    /// Key press
    Key(KeyEvent),
    /// Mouse event
    Mouse(MouseEvent),
    /// Terminal resize
    Resize(u16, u16),
}

/// Event handler using a dedicated thread
pub struct EventHandler {
    /// Event receiver
    receiver: mpsc::Receiver<Event>,
    /// Event sender (kept for drop)
    #[allow(dead_code)]
    sender: mpsc::Sender<Event>,
}

impl EventHandler {
    /// Create a new event handler with the given tick rate
    pub fn new(tick_rate_ms: u64) -> Self {
        let tick_rate = Duration::from_millis(tick_rate_ms);
        let (sender, receiver) = mpsc::channel();
        let event_sender = sender.clone();

        thread::spawn(move || {
            let mut last_tick = Instant::now();
            loop {
                // Calculate timeout until next tick
                let timeout = tick_rate
                    .checked_sub(last_tick.elapsed())
                    .unwrap_or(Duration::ZERO);

                // Poll for events
                if event::poll(timeout).unwrap_or(false) {
                    match event::read() {
                        Ok(CrosstermEvent::Key(key)) => {
                            if event_sender.send(Event::Key(key)).is_err() {
                                break;
                            }
                        }
                        Ok(CrosstermEvent::Mouse(mouse)) => {
                            if event_sender.send(Event::Mouse(mouse)).is_err() {
                                break;
                            }
                        }
                        Ok(CrosstermEvent::Resize(w, h)) => {
                            if event_sender.send(Event::Resize(w, h)).is_err() {
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                // Send tick if interval has passed
                if last_tick.elapsed() >= tick_rate {
                    if event_sender.send(Event::Tick).is_err() {
                        break;
                    }
                    last_tick = Instant::now();
                }
            }
        });

        Self { receiver, sender }
    }

    /// Get the next event (blocking)
    pub fn next(&self) -> Result<Event> {
        Ok(self.receiver.recv()?)
    }
}
