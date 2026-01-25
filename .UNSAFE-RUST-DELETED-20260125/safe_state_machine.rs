// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe state machine implementation.
//!
//! Provides type-safe state machines with validated transitions
//! and transition guards.

use crate::{Error, Result};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// State machine with validated transitions.
#[derive(Debug, Clone)]
pub struct StateMachine<S: Clone + Eq + Hash> {
    current: S,
    transitions: HashMap<S, HashSet<S>>,
    history: Vec<S>,
    max_history: usize,
}

impl<S: Clone + Eq + Hash> StateMachine<S> {
    /// Create a new state machine with an initial state.
    pub fn new(initial: S) -> Self {
        Self {
            current: initial,
            transitions: HashMap::new(),
            history: Vec::new(),
            max_history: 100,
        }
    }

    /// Create with a history limit.
    pub fn with_history_limit(initial: S, max_history: usize) -> Self {
        Self {
            current: initial,
            transitions: HashMap::new(),
            history: Vec::new(),
            max_history,
        }
    }

    /// Add a valid transition.
    pub fn add_transition(&mut self, from: S, to: S) {
        self.transitions
            .entry(from)
            .or_insert_with(HashSet::new)
            .insert(to);
    }

    /// Add multiple transitions from a state.
    pub fn add_transitions(&mut self, from: S, to_states: Vec<S>) {
        let entry = self.transitions.entry(from).or_insert_with(HashSet::new);
        for to in to_states {
            entry.insert(to);
        }
    }

    /// Get current state.
    pub fn current(&self) -> &S {
        &self.current
    }

    /// Check if transition is valid.
    pub fn can_transition(&self, to: &S) -> bool {
        self.transitions
            .get(&self.current)
            .map(|valid| valid.contains(to))
            .unwrap_or(false)
    }

    /// Get valid transitions from current state.
    pub fn valid_transitions(&self) -> Vec<&S> {
        self.transitions
            .get(&self.current)
            .map(|set| set.iter().collect())
            .unwrap_or_default()
    }

    /// Attempt to transition to a new state.
    pub fn transition(&mut self, to: S) -> Result<&S>
    where
        S: std::fmt::Debug,
    {
        if !self.can_transition(&to) {
            return Err(Error::InvalidInput(format!(
                "Invalid transition from {:?} to {:?}",
                self.current, to
            )));
        }

        // Record history
        self.history.push(self.current.clone());
        while self.history.len() > self.max_history {
            self.history.remove(0);
        }

        self.current = to;
        Ok(&self.current)
    }

    /// Force transition without validation (use with caution).
    pub fn force_transition(&mut self, to: S) {
        self.history.push(self.current.clone());
        while self.history.len() > self.max_history {
            self.history.remove(0);
        }
        self.current = to;
    }

    /// Get transition history.
    pub fn history(&self) -> &[S] {
        &self.history
    }

    /// Clear history.
    pub fn clear_history(&mut self) {
        self.history.clear();
    }

    /// Reset to initial state (first in history or current if no history).
    pub fn reset(&mut self)
    where
        S: Clone,
    {
        if let Some(initial) = self.history.first() {
            self.current = initial.clone();
        }
        self.history.clear();
    }
}

/// Simple enum-based state machine builder.
pub struct StateMachineBuilder<S: Clone + Eq + Hash> {
    initial: Option<S>,
    transitions: HashMap<S, HashSet<S>>,
}

impl<S: Clone + Eq + Hash> StateMachineBuilder<S> {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            initial: None,
            transitions: HashMap::new(),
        }
    }

    /// Set the initial state.
    pub fn initial(mut self, state: S) -> Self {
        self.initial = Some(state);
        self
    }

    /// Add a transition.
    pub fn transition(mut self, from: S, to: S) -> Self {
        self.transitions
            .entry(from)
            .or_insert_with(HashSet::new)
            .insert(to);
        self
    }

    /// Add transitions from a state to multiple targets.
    pub fn transitions(mut self, from: S, to: Vec<S>) -> Self {
        let entry = self.transitions.entry(from).or_insert_with(HashSet::new);
        for t in to {
            entry.insert(t);
        }
        self
    }

    /// Build the state machine.
    pub fn build(self) -> Result<StateMachine<S>> {
        let initial = self
            .initial
            .ok_or_else(|| Error::InvalidInput("Initial state required".into()))?;
        let mut sm = StateMachine::new(initial);
        sm.transitions = self.transitions;
        Ok(sm)
    }
}

impl<S: Clone + Eq + Hash> Default for StateMachineBuilder<S> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum OrderState {
        Pending,
        Confirmed,
        Shipped,
        Delivered,
        Cancelled,
    }

    #[test]
    fn test_state_machine() {
        let mut sm = StateMachineBuilder::new()
            .initial(OrderState::Pending)
            .transition(OrderState::Pending, OrderState::Confirmed)
            .transition(OrderState::Pending, OrderState::Cancelled)
            .transition(OrderState::Confirmed, OrderState::Shipped)
            .transition(OrderState::Confirmed, OrderState::Cancelled)
            .transition(OrderState::Shipped, OrderState::Delivered)
            .build()
            .unwrap();

        assert_eq!(*sm.current(), OrderState::Pending);
        assert!(sm.can_transition(&OrderState::Confirmed));
        assert!(!sm.can_transition(&OrderState::Delivered));

        sm.transition(OrderState::Confirmed).unwrap();
        assert_eq!(*sm.current(), OrderState::Confirmed);

        sm.transition(OrderState::Shipped).unwrap();
        sm.transition(OrderState::Delivered).unwrap();
        assert_eq!(*sm.current(), OrderState::Delivered);

        assert_eq!(sm.history().len(), 3);
    }

    #[test]
    fn test_invalid_transition() {
        let mut sm = StateMachineBuilder::new()
            .initial(OrderState::Pending)
            .transition(OrderState::Pending, OrderState::Confirmed)
            .build()
            .unwrap();

        let result = sm.transition(OrderState::Delivered);
        assert!(result.is_err());
    }
}
