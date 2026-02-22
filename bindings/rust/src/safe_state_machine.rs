// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//! Safe state machine via libproven FFI.
//!
//! Provides type-safe state transitions with configurable transition rules.
//! All operations delegate to Idris 2 verified code.

use crate::core::{Error, Result};
use crate::ffi;

/// A finite state machine with validated transitions.
///
/// The underlying state is managed by libproven. The state machine is
/// freed when this struct is dropped.
pub struct StateMachine {
    ptr: *mut ffi::StateMachine,
}

// SAFETY: The underlying state machine is managed by libproven.
unsafe impl Send for StateMachine {}

impl StateMachine {
    /// Create a new state machine.
    ///
    /// - `state_count`: total number of states.
    /// - `initial_state`: index of the initial state (0-indexed).
    pub fn new(state_count: u32, initial_state: u32) -> Result<Self> {
        // SAFETY: proven_state_machine_create takes value-type arguments;
        // always safe to call.
        let ptr = unsafe {
            ffi::proven_state_machine_create(state_count, initial_state)
        };
        if ptr.is_null() {
            return Err(Error::AllocationFailed);
        }
        Ok(StateMachine { ptr })
    }

    /// Allow a transition from state `from` to state `to`.
    ///
    /// Returns `true` if the transition was successfully registered.
    pub fn allow_transition(&mut self, from: u32, to: u32) -> bool {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_state_machine_allow(self.ptr, from, to) }
    }

    /// Attempt to transition to a new state.
    ///
    /// Returns `true` if the transition was allowed and performed,
    /// `false` if the transition is not permitted from the current state.
    pub fn transition(&mut self, to: u32) -> bool {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_state_machine_transition(self.ptr, to) }
    }

    /// Get the current state index.
    pub fn current_state(&self) -> u32 {
        // SAFETY: self.ptr is valid (checked at construction).
        unsafe { ffi::proven_state_machine_state(self.ptr) }
    }
}

impl Drop for StateMachine {
    fn drop(&mut self) {
        // SAFETY: self.ptr was allocated by proven_state_machine_create
        // and has not been freed yet.
        unsafe {
            ffi::proven_state_machine_free(self.ptr);
        }
    }
}

/// Builder for constructing state machines with a fluent API.
pub struct StateMachineBuilder {
    state_count: u32,
    initial_state: u32,
    transitions: Vec<(u32, u32)>,
}

impl StateMachineBuilder {
    /// Create a new builder.
    pub fn new(state_count: u32, initial_state: u32) -> Self {
        StateMachineBuilder {
            state_count,
            initial_state,
            transitions: Vec::new(),
        }
    }

    /// Add an allowed transition.
    pub fn allow(mut self, from: u32, to: u32) -> Self {
        self.transitions.push((from, to));
        self
    }

    /// Build the state machine.
    pub fn build(self) -> Result<StateMachine> {
        let mut sm = StateMachine::new(self.state_count, self.initial_state)?;
        for (from, to) in &self.transitions {
            sm.allow_transition(*from, *to);
        }
        Ok(sm)
    }
}
