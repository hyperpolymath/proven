// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeStateMachine - FFI bindings to libproven state machine operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for state machine operations.
pub const StateMachineError = error{
    CreationFailed,
};

/// Type-safe state machine backed by libproven.
pub const StateMachine = struct {
    ptr: *c.ProvenStateMachine,

    /// Allow a state transition via libproven.
    pub fn allowTransition(self: StateMachine, from: u32, to: u32) bool {
        return c.proven_state_machine_allow(self.ptr, from, to);
    }

    /// Attempt to transition to a new state via libproven.
    pub fn transition(self: StateMachine, to: u32) bool {
        return c.proven_state_machine_transition(self.ptr, to);
    }

    /// Get current state index via libproven.
    pub fn state(self: StateMachine) u32 {
        return c.proven_state_machine_state(self.ptr);
    }

    /// Free state machine via libproven.
    pub fn deinit(self: StateMachine) void {
        c.proven_state_machine_free(self.ptr);
    }
};

/// Create a state machine via libproven.
/// state_count: Total number of states (max 256).
/// initial_state: Initial state index.
pub fn create(state_count: u32, initial_state: u32) StateMachineError!StateMachine {
    const ptr = c.proven_state_machine_create(state_count, initial_state);
    if (ptr == null) return error.CreationFailed;
    return StateMachine{ .ptr = ptr.? };
}

test "create and transition" {
    const sm = try create(3, 0);
    defer sm.deinit();
    try std.testing.expectEqual(@as(u32, 0), sm.state());
    _ = sm.allowTransition(0, 1);
    try std.testing.expect(sm.transition(1));
    try std.testing.expectEqual(@as(u32, 1), sm.state());
}
