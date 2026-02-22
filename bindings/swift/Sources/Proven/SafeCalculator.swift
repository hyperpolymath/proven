// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe expression evaluation delegated to libproven FFI.
///
/// Arithmetic expression parsing and evaluation via the formally
/// verified Idris 2 core.

import CProven

public enum SafeCalculator {
    /// Evaluate an arithmetic expression string.
    ///
    /// Supports: +, -, *, /, parentheses, negative numbers, decimals.
    public static func eval(_ expression: String) -> Result<Double, ProvenError> {
        withStringBytes(expression) { ptr, len in
            let result = proven_calculator_eval(ptr, len)
            if let error = ProvenError.fromStatus(result.status) {
                return .failure(error)
            }
            return .success(result.value)
        }
    }
}
