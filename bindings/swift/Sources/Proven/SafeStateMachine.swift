// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// State machine delegated to libproven FFI.
///
/// Finite state machine with validated transitions via the
/// formally verified Idris 2 core.

import CProven

/// Finite state machine backed by libproven.
/// This class manages the lifecycle of a C-allocated state machine.
/// States are represented as UInt32 indices (0 to stateCount-1).
public final class SafeStateMachine {
    private let sm: UnsafeMutablePointer<ProvenStateMachine>

    /// Create a state machine with the given number of states.
    /// Returns nil if allocation fails.
    /// - Parameters:
    ///   - stateCount: Total number of states.
    ///   - initialState: The starting state index.
    public init?(stateCount: UInt32, initialState: UInt32) {
        guard let ptr = proven_state_machine_create(stateCount, initialState) else {
            return nil
        }
        self.sm = ptr
    }

    deinit {
        proven_state_machine_free(sm)
    }

    /// Allow a transition from one state to another.
    /// Returns true if the transition rule was added successfully.
    @discardableResult
    public func allow(from: UInt32, to: UInt32) -> Bool {
        proven_state_machine_allow(sm, from, to)
    }

    /// Attempt to transition to the given state.
    /// Returns true if the transition was valid and executed.
    @discardableResult
    public func transition(to state: UInt32) -> Bool {
        proven_state_machine_transition(sm, state)
    }

    /// Get the current state index.
    public var state: UInt32 {
        proven_state_machine_state(sm)
    }
}
