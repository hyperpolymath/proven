// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Circuit breaker delegated to libproven FFI.
///
/// Circuit breaker pattern with failure threshold, success threshold,
/// and timeout via the formally verified Idris 2 core.

import CProven

/// Circuit breaker states.
public enum CircuitState: Int, Equatable, Sendable {
    case closed = 0
    case open = 1
    case halfOpen = 2
}

/// Circuit breaker backed by libproven.
/// This class manages the lifecycle of a C-allocated circuit breaker.
public final class SafeCircuitBreaker {
    private let cb: UnsafeMutablePointer<ProvenCircuitBreaker>

    /// Create a circuit breaker with thresholds and timeout.
    /// Returns nil if allocation fails.
    /// - Parameters:
    ///   - failureThreshold: Number of failures before opening the circuit.
    ///   - successThreshold: Number of successes in half-open before closing.
    ///   - timeoutMs: Milliseconds before transitioning from open to half-open.
    public init?(failureThreshold: UInt32, successThreshold: UInt32, timeoutMs: Int64) {
        guard let ptr = proven_circuit_breaker_create(
            failureThreshold, successThreshold, timeoutMs
        ) else {
            return nil
        }
        self.cb = ptr
    }

    deinit {
        proven_circuit_breaker_free(cb)
    }

    /// Check if the circuit allows a call.
    public func allow() -> Bool {
        proven_circuit_breaker_allow(cb)
    }

    /// Record a successful call.
    public func recordSuccess() {
        proven_circuit_breaker_success(cb)
    }

    /// Record a failed call.
    public func recordFailure() {
        proven_circuit_breaker_failure(cb)
    }

    /// Get the current circuit state.
    public var state: CircuitState {
        let raw = proven_circuit_breaker_state(cb)
        switch raw {
        case PROVEN_CIRCUIT_CLOSED:
            return .closed
        case PROVEN_CIRCUIT_OPEN:
            return .open
        case PROVEN_CIRCUIT_HALF_OPEN:
            return .halfOpen
        default:
            return .closed
        }
    }
}
