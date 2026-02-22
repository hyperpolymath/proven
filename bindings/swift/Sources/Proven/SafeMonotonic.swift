// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Monotonic counter delegated to libproven FFI.
///
/// Monotonically increasing counter with overflow protection
/// via the formally verified Idris 2 core.

import CProven

/// Monotonic counter backed by libproven.
/// This class manages the lifecycle of a C-allocated monotonic counter.
public final class SafeMonotonic {
    private let counter: UnsafeMutablePointer<ProvenMonotonicCounter>

    /// Create a monotonic counter with initial value and maximum.
    /// Returns nil if allocation fails.
    /// - Parameters:
    ///   - initial: Starting value for the counter.
    ///   - maxValue: Maximum allowed value.
    public init?(initial: UInt64, maxValue: UInt64) {
        guard let ptr = proven_monotonic_create(initial, maxValue) else {
            return nil
        }
        self.counter = ptr
    }

    deinit {
        proven_monotonic_free(counter)
    }

    /// Get the next value from the counter.
    /// Returns the incremented value, or an error if the maximum is reached.
    public func next() -> Result<Int64, ProvenError> {
        let result = proven_monotonic_next(counter)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }
}
