// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! UI rendering for Proven TUI

mod home;
mod safe_math;
mod validation;
mod bounded;
mod about;

use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph, Tabs},
    Frame,
};

use crate::app::{App, InputMode, Tab};

/// Main render function
pub fn render(app: &App, frame: &mut Frame) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),  // Tabs
            Constraint::Min(0),     // Content
            Constraint::Length(3),  // Status bar
        ])
        .split(frame.area());

    render_tabs(app, frame, chunks[0]);
    render_content(app, frame, chunks[1]);
    render_status_bar(app, frame, chunks[2]);
}

fn render_tabs(app: &App, frame: &mut Frame, area: Rect) {
    let titles: Vec<Line> = Tab::all()
        .iter()
        .enumerate()
        .map(|(i, tab)| {
            let style = if *tab == app.current_tab {
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::White)
            };
            Line::from(vec![
                Span::styled(format!("[{}] ", i + 1), Style::default().fg(Color::DarkGray)),
                Span::styled(tab.title(), style),
            ])
        })
        .collect();

    let tabs = Tabs::new(titles)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Proven TUI ")
                .title_style(Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
        )
        .highlight_style(Style::default().fg(Color::Yellow))
        .select(Tab::all().iter().position(|t| *t == app.current_tab).unwrap_or(0));

    frame.render_widget(tabs, area);
}

fn render_content(app: &App, frame: &mut Frame, area: Rect) {
    match app.current_tab {
        Tab::Home => home::render(app, frame, area),
        Tab::SafeMath => safe_math::render(app, frame, area),
        Tab::Validation => validation::render(app, frame, area),
        Tab::Bounded => bounded::render(app, frame, area),
        Tab::About => about::render(app, frame, area),
    }
}

fn render_status_bar(app: &App, frame: &mut Frame, area: Rect) {
    let mode_indicator = match app.input_mode {
        InputMode::Normal => Span::styled(
            " NORMAL ",
            Style::default().bg(Color::Blue).fg(Color::White),
        ),
        InputMode::Editing => Span::styled(
            " EDIT ",
            Style::default().bg(Color::Yellow).fg(Color::Black),
        ),
    };

    let status_text = app
        .status_message
        .as_deref()
        .unwrap_or("Press ? for help");

    let tick_count = format!("tick: {}", app.tick_count());

    let status = Paragraph::new(Line::from(vec![
        mode_indicator,
        Span::raw(" "),
        Span::styled(status_text, Style::default().fg(Color::White)),
        Span::raw(" | "),
        Span::styled(tick_count, Style::default().fg(Color::DarkGray)),
    ]))
    .block(
        Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::DarkGray)),
    );

    frame.render_widget(status, area);
}
