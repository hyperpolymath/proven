// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe Math tab rendering

use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Frame,
};

use crate::app::{App, InputMode};

pub fn render(app: &App, frame: &mut Frame, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),  // Input
            Constraint::Length(10), // Examples
            Constraint::Min(0),     // History
        ])
        .split(area);

    render_input(app, frame, chunks[0]);
    render_examples(frame, chunks[1]);
    render_history(app, frame, chunks[2]);
}

fn render_input(app: &App, frame: &mut Frame, area: Rect) {
    let input_style = match app.input_mode {
        InputMode::Editing => Style::default().fg(Color::Yellow),
        InputMode::Normal => Style::default().fg(Color::White),
    };

    let input_text = if app.input_buffer.is_empty() {
        match app.input_mode {
            InputMode::Editing => "Type expression...",
            InputMode::Normal => "Press 'i' to enter expression",
        }
    } else {
        &app.input_buffer
    };

    let cursor = if app.input_mode == InputMode::Editing {
        "_"
    } else {
        ""
    };

    let input = Paragraph::new(format!("{}{}", input_text, cursor))
        .style(input_style)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Calculator (Proven SafeMath) ")
                .title_style(Style::default().fg(Color::Cyan)),
        );

    frame.render_widget(input, area);
}

fn render_examples(frame: &mut Frame, area: Rect) {
    let examples = vec![
        Line::from(vec![
            Span::styled("Try these expressions:", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  42 + 7          ", Style::default().fg(Color::Yellow)),
            Span::styled("- Basic addition", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  max i64         ", Style::default().fg(Color::Yellow)),
            Span::styled("- Show max i64 value", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  9223372036854775807 + 1  ", Style::default().fg(Color::Yellow)),
            Span::styled("- Overflow demo", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  overflow demo   ", Style::default().fg(Color::Yellow)),
            Span::styled("- See Proven catch overflow", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  divzero demo    ", Style::default().fg(Color::Yellow)),
            Span::styled("- Division by zero protection", Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let examples_paragraph = Paragraph::new(examples).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Examples ")
            .title_style(Style::default().fg(Color::Green)),
    );

    frame.render_widget(examples_paragraph, area);
}

fn render_history(app: &App, frame: &mut Frame, area: Rect) {
    let history_items: Vec<ListItem> = app
        .calculation_history
        .iter()
        .rev()
        .map(|entry| {
            let style = if entry.contains("Overflow") || entry.contains("Division error") {
                Style::default().fg(Color::Red)
            } else if entry.contains("(safe)") {
                Style::default().fg(Color::Green)
            } else {
                Style::default().fg(Color::White)
            };
            ListItem::new(Line::from(Span::styled(entry.clone(), style)))
        })
        .collect();

    let history_list = List::new(history_items).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Calculation History ")
            .title_style(Style::default().fg(Color::Magenta)),
    );

    frame.render_widget(history_list, area);
}
