// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Validation tab rendering

use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

use crate::app::App;

pub fn render(_app: &App, frame: &mut Frame, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    render_validators(frame, chunks[0]);
    render_examples(frame, chunks[1]);
}

fn render_validators(frame: &mut Frame, area: Rect) {
    let validators = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Proven's Validation Primitives", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("• is_valid_port(n)", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Validates port numbers (1-65535)", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("• is_valid_email(s)", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Basic email format validation", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("• is_valid_url(s)", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  URL structure validation", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("• is_non_empty(s)", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Ensures string is not empty", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("• validate_range(n, min, max)", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Bounds checking for numbers", Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let paragraph = Paragraph::new(validators).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Validators ")
            .title_style(Style::default().fg(Color::Cyan)),
    );

    frame.render_widget(paragraph, area);
}

fn render_examples(frame: &mut Frame, area: Rect) {
    let examples = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Usage Examples (Rust)", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("use proven::Validation;", Style::default().fg(Color::Green)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("// Port validation", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("let port = 8080;", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("if Validation::is_valid_port(port) {", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("    // Safe to use", Style::default().fg(Color::Green)),
        ]),
        Line::from(vec![
            Span::styled("}", Style::default().fg(Color::White)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("// Range validation", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("match Validation::validate_range(", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("    age, 0, 150", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled(") {", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("    Ok(valid_age) => { ... }", Style::default().fg(Color::Green)),
        ]),
        Line::from(vec![
            Span::styled("    Err(e) => { ... }", Style::default().fg(Color::Red)),
        ]),
        Line::from(vec![
            Span::styled("}", Style::default().fg(Color::White)),
        ]),
    ];

    let paragraph = Paragraph::new(examples).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Code Examples ")
            .title_style(Style::default().fg(Color::Green)),
    );

    frame.render_widget(paragraph, area);
}
