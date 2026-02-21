// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe finite state machine implementation.

const std = @import("std");

pub const StateMachineError = error{
    InvalidTransition,
    MaxTransitionsReached,
};

/// Transition definition.
pub fn Transition(comptime State: type, comptime Event: type) type {
    return struct {
        from: State,
        trigger: Event,
        to: State,
    };
}

/// State machine with compile-time state and event types.
pub fn StateMachine(comptime State: type, comptime Event: type, comptime max_transitions: usize) type {
    return struct {
        const Self = @This();
        const Trans = Transition(State, Event);

        transitions: [max_transitions]?Trans = [_]?Trans{null} ** max_transitions,
        transition_count: usize = 0,
        initial_state: State,
        current_state: State,
        final_states: [16]?State = [_]?State{null} ** 16,
        final_count: usize = 0,

        pub fn init(initial: State) Self {
            return .{
                .initial_state = initial,
                .current_state = initial,
            };
        }

        /// Add a transition.
        pub fn addTransition(self: *Self, from: State, event: Event, to: State) StateMachineError!void {
            if (self.transition_count >= max_transitions) return error.MaxTransitionsReached;
            self.transitions[self.transition_count] = .{ .from = from, .trigger = event, .to = to };
            self.transition_count += 1;
        }

        /// Mark a state as final.
        pub fn addFinalState(self: *Self, state: State) void {
            if (self.final_count < 16) {
                self.final_states[self.final_count] = state;
                self.final_count += 1;
            }
        }

        /// Find transition for current state and event.
        fn findTransition(self: *const Self, event: Event) ?State {
            for (self.transitions[0..self.transition_count]) |trans_opt| {
                if (trans_opt) |trans| {
                    if (trans.from == self.current_state and trans.trigger == event) {
                        return trans.to;
                    }
                }
            }
            return null;
        }

        /// Trigger an event.
        pub fn trigger(self: *Self, event: Event) StateMachineError!void {
            if (self.findTransition(event)) |next| {
                self.current_state = next;
            } else {
                return error.InvalidTransition;
            }
        }

        /// Check if event can be triggered.
        pub fn canTrigger(self: *const Self, event: Event) bool {
            return self.findTransition(event) != null;
        }

        /// Get current state.
        pub fn getState(self: *const Self) State {
            return self.current_state;
        }

        /// Check if in final state.
        pub fn isInFinalState(self: *const Self) bool {
            for (self.final_states[0..self.final_count]) |state_opt| {
                if (state_opt) |state| {
                    if (state == self.current_state) return true;
                }
            }
            return false;
        }

        /// Reset to initial state.
        pub fn reset(self: *Self) void {
            self.current_state = self.initial_state;
        }
    };
}

test "StateMachine" {
    const State = enum { idle, running, stopped };
    const Event = enum { start, stop, reset };

    var sm = StateMachine(State, Event, 10).init(.idle);
    try sm.addTransition(.idle, .start, .running);
    try sm.addTransition(.running, .stop, .stopped);
    try sm.addTransition(.stopped, .reset, .idle);
    sm.addFinalState(.stopped);

    try std.testing.expectEqual(State.idle, sm.getState());
    try sm.trigger(.start);
    try std.testing.expectEqual(State.running, sm.getState());
    try sm.trigger(.stop);
    try std.testing.expect(sm.isInFinalState());
}
