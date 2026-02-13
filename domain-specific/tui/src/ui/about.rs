// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! About tab rendering

use ratatui::{
    layout::{Alignment, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

use crate::app::App;

pub fn render(_app: &App, frame: &mut Frame, area: Rect) {
    let about = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Proven", Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)),
            Span::styled(" v0.9.0", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Formally Verified Safety Primitives", Style::default().fg(Color::White)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Core written in Idris 2 with dependent types.", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("Safety guarantees proven at compile time.", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(""),
        Line::from(vec![
            Span::styled("Available on:", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("crates.io/crates/proven", Style::default().fg(Color::Cyan)),
            Span::styled(" (Rust)", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("pypi.org/project/proven", Style::default().fg(Color::Cyan)),
            Span::styled(" (Python)", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("npmjs.com/package/@hyperpolymath/proven", Style::default().fg(Color::Cyan)),
            Span::styled(" (JS/TS)", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("jsr.io/@proven/core", Style::default().fg(Color::Cyan)),
            Span::styled(" (Deno)", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(vec![
            Span::styled("  • ", Style::default().fg(Color::Green)),
            Span::styled("+ 50 more language bindings", Style::default().fg(Color::White)),
        ]),
        Line::from(""),
        Line::from(""),
        Line::from(vec![
            Span::styled("This TUI:", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("  Built with ", Style::default().fg(Color::DarkGray)),
            Span::styled("ratatui", Style::default().fg(Color::Yellow)),
            Span::styled(" + ", Style::default().fg(Color::DarkGray)),
            Span::styled("proven", Style::default().fg(Color::Cyan)),
        ]),
        Line::from(vec![
            Span::styled("  We eat our own dog food!", Style::default().fg(Color::Green)),
        ]),
        Line::from(""),
        Line::from(""),
        Line::from(vec![
            Span::styled("License: ", Style::default().fg(Color::DarkGray)),
            Span::styled("PMPL-1.0", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("Author: ", Style::default().fg(Color::DarkGray)),
            Span::styled("Hyperpolymath", Style::default().fg(Color::White)),
        ]),
        Line::from(vec![
            Span::styled("Repository: ", Style::default().fg(Color::DarkGray)),
            Span::styled("github.com/hyperpolymath/proven", Style::default().fg(Color::Cyan)),
        ]),
    ];

    let paragraph = Paragraph::new(about)
        .alignment(Alignment::Center)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" About Proven ")
                .title_style(Style::default().fg(Color::Cyan)),
        );

    frame.render_widget(paragraph, area);
}
