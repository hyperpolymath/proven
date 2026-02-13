// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Application state and logic for Proven TUI

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use proven::SafeMath;

use crate::safety::SafeCalculator;

/// Main application tabs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Tab {
    #[default]
    Home,
    SafeMath,
    Validation,
    Bounded,
    About,
}

impl Tab {
    pub fn title(&self) -> &'static str {
        match self {
            Tab::Home => "Home",
            Tab::SafeMath => "Safe Math",
            Tab::Validation => "Validation",
            Tab::Bounded => "Bounded Types",
            Tab::About => "About",
        }
    }

    pub fn all() -> &'static [Tab] {
        &[Tab::Home, Tab::SafeMath, Tab::Validation, Tab::Bounded, Tab::About]
    }

    pub fn next(&self) -> Tab {
        match self {
            Tab::Home => Tab::SafeMath,
            Tab::SafeMath => Tab::Validation,
            Tab::Validation => Tab::Bounded,
            Tab::Bounded => Tab::About,
            Tab::About => Tab::Home,
        }
    }

    pub fn previous(&self) -> Tab {
        match self {
            Tab::Home => Tab::About,
            Tab::SafeMath => Tab::Home,
            Tab::Validation => Tab::SafeMath,
            Tab::Bounded => Tab::Validation,
            Tab::About => Tab::Bounded,
        }
    }
}

/// Input mode for forms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum InputMode {
    #[default]
    Normal,
    Editing,
}

/// Application state
pub struct App {
    /// Current active tab
    pub current_tab: Tab,

    /// Input mode
    pub input_mode: InputMode,

    /// Input buffer for forms
    pub input_buffer: String,

    /// Safe calculator for demonstrating SafeMath
    pub calculator: SafeCalculator,

    /// Current tick count (using Proven's safe addition)
    tick_count: u64,

    /// Status message
    pub status_message: Option<String>,

    /// Calculation history
    pub calculation_history: Vec<String>,

    /// Selected menu item in current tab
    pub selected_item: usize,
}

impl App {
    pub fn new() -> Self {
        Self {
            current_tab: Tab::default(),
            input_mode: InputMode::default(),
            input_buffer: String::new(),
            calculator: SafeCalculator::new(),
            tick_count: 0,
            status_message: Some("Welcome to Proven TUI! Press ? for help.".into()),
            calculation_history: Vec::new(),
            selected_item: 0,
        }
    }

    /// Handle tick events - uses Proven's add for safety
    pub fn tick(&mut self) {
        // Demonstrate using Proven's SafeMath for tick counting
        match SafeMath::add(self.tick_count as i64, 1) {
            Ok(new_count) => self.tick_count = new_count as u64,
            Err(_) => {
                // Overflow protection - reset counter safely
                self.tick_count = 0;
                self.status_message = Some("Tick counter reset (overflow protected)".into());
            }
        }
    }

    /// Get current tick count
    pub fn tick_count(&self) -> u64 {
        self.tick_count
    }

    /// Handle key events
    /// Returns true if the app should quit
    pub fn handle_key(&mut self, key: KeyEvent) -> bool {
        match self.input_mode {
            InputMode::Normal => self.handle_normal_key(key),
            InputMode::Editing => {
                self.handle_editing_key(key);
                false
            }
        }
    }

    fn handle_normal_key(&mut self, key: KeyEvent) -> bool {
        match key.code {
            // Quit
            KeyCode::Char('q') => return true,
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => return true,

            // Tab navigation
            KeyCode::Tab => self.current_tab = self.current_tab.next(),
            KeyCode::BackTab => self.current_tab = self.current_tab.previous(),
            KeyCode::Char('1') => self.current_tab = Tab::Home,
            KeyCode::Char('2') => self.current_tab = Tab::SafeMath,
            KeyCode::Char('3') => self.current_tab = Tab::Validation,
            KeyCode::Char('4') => self.current_tab = Tab::Bounded,
            KeyCode::Char('5') => self.current_tab = Tab::About,

            // Menu navigation
            KeyCode::Up | KeyCode::Char('k') => {
                if self.selected_item > 0 {
                    self.selected_item -= 1;
                }
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.selected_item += 1;
            }

            // Enter edit mode
            KeyCode::Char('i') | KeyCode::Enter => {
                if self.current_tab == Tab::SafeMath {
                    self.input_mode = InputMode::Editing;
                    self.status_message = Some("Enter expression (e.g., 42 + 7)".into());
                }
            }

            // Help
            KeyCode::Char('?') => {
                self.status_message = Some(
                    "q=quit, Tab=next tab, 1-5=go to tab, i=edit, j/k=up/down".into(),
                );
            }

            _ => {}
        }
        false
    }

    fn handle_editing_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Enter => {
                self.evaluate_expression();
                self.input_mode = InputMode::Normal;
            }
            KeyCode::Esc => {
                self.input_mode = InputMode::Normal;
                self.input_buffer.clear();
                self.status_message = Some("Cancelled".into());
            }
            KeyCode::Char(c) => {
                self.input_buffer.push(c);
            }
            KeyCode::Backspace => {
                self.input_buffer.pop();
            }
            _ => {}
        }
    }

    fn evaluate_expression(&mut self) {
        let expr = self.input_buffer.trim().to_string();
        if expr.is_empty() {
            return;
        }

        let result = self.calculator.evaluate(&expr);
        let history_entry = format!("{} = {}", expr, result);
        self.calculation_history.push(history_entry.clone());
        self.status_message = Some(result);
        self.input_buffer.clear();

        // Keep history bounded (safe!)
        while self.calculation_history.len() > 10 {
            self.calculation_history.remove(0);
        }
    }
}

impl Default for App {
    fn default() -> Self {
        Self::new()
    }
}
