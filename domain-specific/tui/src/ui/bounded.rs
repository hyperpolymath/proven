// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Bounded types tab rendering

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
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    render_types(frame, chunks[0]);
    render_theory(frame, chunks[1]);
}

fn render_types(frame: &mut Frame, area: Rect) {
    let types = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Bounded Types in Proven", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Port", Style::default().fg(Color::Cyan)),
            Span::styled(" : Nat {1..65535}", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Network port - always valid by construction", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Percentage", Style::default().fg(Color::Cyan)),
            Span::styled(" : Nat {0..100}", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Percentage value - can't exceed 100", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("NonEmptyString", Style::default().fg(Color::Cyan)),
            Span::styled(" : String {len > 0}", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  String guaranteed non-empty", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("BoundedNat", Style::default().fg(Color::Cyan)),
            Span::styled(" : Nat {min..max}", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("  Custom bounded natural number", Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let paragraph = Paragraph::new(types).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Bounded Types ")
            .title_style(Style::default().fg(Color::Cyan)),
    );

    frame.render_widget(paragraph, area);
}

fn render_theory(frame: &mut Frame, area: Rect) {
    let theory = vec![
        Line::from(""),
        Line::from(vec![
            Span::styled("Why Bounded Types Matter", Style::default().fg(Color::White).add_modifier(Modifier::BOLD)),
        ]),
        Line::from(""),
        Line::from("In traditional programming, a port is just an integer:"),
        Line::from(""),
        Line::from(vec![
            Span::styled("  let port: u16 = 99999;  ", Style::default().fg(Color::Red)),
            Span::styled("// Compiles but wrong!", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from("With Proven's bounded types, invalid values are impossible:"),
        Line::from(""),
        Line::from(vec![
            Span::styled("  let port: Port = Port::new(99999);  ", Style::default().fg(Color::Green)),
        ]),
        Line::from(vec![
            Span::styled("  // Returns Err(InvalidPort) - caught at creation!", Style::default().fg(Color::DarkGray)),
        ]),
        Line::from(""),
        Line::from("The type system proves that if you have a Port value,"),
        Line::from("it is guaranteed to be in the valid range."),
        Line::from(""),
        Line::from(vec![
            Span::styled("This is \"correct by construction\" - ", Style::default().fg(Color::Yellow)),
        ]),
        Line::from(vec![
            Span::styled("bugs become impossible, not just unlikely.", Style::default().fg(Color::Yellow)),
        ]),
    ];

    let paragraph = Paragraph::new(theory).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Theory ")
            .title_style(Style::default().fg(Color::Magenta)),
    );

    frame.render_widget(paragraph, area);
}
