// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven TUI - Terminal interface for exploring Proven safety primitives
//!
//! This TUI demonstrates and uses Proven's verified safety operations,
//! proving our commitment by eating our own dog food.

mod app;
mod event;
mod safety;
mod ui;

use std::io;

use anyhow::Result;
use clap::Parser;
use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::prelude::*;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::app::App;
use crate::event::{Event, EventHandler};

/// Proven TUI - Explore verified safety primitives
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Enable debug logging
    #[arg(short, long)]
    debug: bool,

    /// Tick rate in milliseconds
    #[arg(short, long, default_value_t = 250)]
    tick_rate: u64,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Initialize logging
    let log_level = if args.debug { "debug" } else { "info" };
    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(
            std::env::var("RUST_LOG").unwrap_or_else(|_| log_level.into()),
        ))
        .with(tracing_subscriber::fmt::layer().with_target(false))
        .init();

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app and event handler
    let mut app = App::new();
    let event_handler = EventHandler::new(args.tick_rate);

    // Run the main loop
    let result = run_app(&mut terminal, &mut app, &event_handler);

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    // Handle any errors from the main loop
    if let Err(err) = result {
        eprintln!("Error: {err:?}");
        std::process::exit(1);
    }

    Ok(())
}

fn run_app(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
    event_handler: &EventHandler,
) -> Result<()> {
    loop {
        // Render the UI
        terminal.draw(|frame| ui::render(app, frame))?;

        // Handle events
        match event_handler.next()? {
            Event::Tick => app.tick(),
            Event::Key(key_event) => {
                if app.handle_key(key_event) {
                    return Ok(());
                }
            }
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
        }
    }
}
