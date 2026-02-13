// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Home tab rendering

use ratatui::{
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

use crate::app::App;

const LOGO: &str = r#"
 ____
|  _ \ _ __ _____   _____ _ __
| |_) | '__/ _ \ \ / / _ \ '_ \
|  __/| | | (_) \ V /  __/ | | |
|_|   |_|  \___/ \_/ \___|_| |_|

   Verified Safety Primitives
"#;

pub fn render(_app: &App, frame: &mut Frame, area: Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(10), // Logo
            Constraint::Min(0),     // Description
        ])
        .split(area);

    // Logo
    let logo = Paragraph::new(LOGO)
        .style(Style::default().fg(Color::Cyan))
        .alignment(Alignment::Center)
        .block(Block::default().borders(Borders::NONE));

    frame.render_widget(logo, chunks[0]);

    // Description
    let description = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Welcome to ", Style::default()),
            Span::styled("Proven TUI", Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
            Span::styled("!", Style::default()),
        ]),
        Line::from(""),
        Line::from("Proven provides formally verified safety primitives that protect"),
        Line::from("your code from common runtime errors:"),
        Line::from(""),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("Integer overflow/underflow", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("Division by zero", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("Array bounds violations", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("Invalid input data", Style::default().fg(Color::White)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("This TUI itself uses Proven", Style::default().fg(Color::DarkGray)),
            Span::styled(" — eating our own dog food!", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Press ", Style::default().fg(Color::DarkGray)),
            Span::styled("Tab", Style::default().fg(Color::Yellow)),
            Span::styled(" to explore different safety modules.", Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let desc_paragraph = Paragraph::new(description)
        .alignment(Alignment::Center)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" About Proven ")
                .title_style(Style::default().fg(Color::Cyan)),
        );

    frame.render_widget(desc_paragraph, chunks[1]);
}
