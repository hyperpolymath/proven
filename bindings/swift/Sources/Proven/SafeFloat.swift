// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe floating-point operations delegated to libproven FFI.
///
/// Prevents NaN, Infinity, and division-by-zero panics via the
/// formally verified Idris 2 core.

import CProven

public enum SafeFloat {
    /// Safe floating-point division.
    public static func div(_ a: Double, _ b: Double) -> Result<Double, ProvenError> {
        let result = proven_float_div(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Check if a double is finite (not NaN or Infinity).
    public static func isFinite(_ x: Double) -> Bool {
        proven_float_is_finite(x)
    }

    /// Check if a double is NaN.
    public static func isNaN(_ x: Double) -> Bool {
        proven_float_is_nan(x)
    }

    /// Safe square root.
    public static func sqrt(_ x: Double) -> Result<Double, ProvenError> {
        let result = proven_float_sqrt(x)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Safe natural logarithm.
    public static func ln(_ x: Double) -> Result<Double, ProvenError> {
        let result = proven_float_ln(x)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }
}
