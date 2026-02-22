// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe arithmetic operations delegated to libproven FFI.
///
/// All computation happens in Idris 2 via the Zig FFI bridge.
/// This module provides Swift-idiomatic wrappers returning Result types.

import CProven

public enum SafeMath {
    /// Safe integer division. Returns .failure(.divisionByZero) if denominator is 0.
    public static func div(_ a: Int64, _ b: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_div(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Safe modulo operation. Returns .failure(.divisionByZero) if denominator is 0.
    public static func mod(_ a: Int64, _ b: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_mod(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Checked addition with overflow detection.
    public static func addChecked(_ a: Int64, _ b: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_add_checked(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Checked subtraction with underflow detection.
    public static func subChecked(_ a: Int64, _ b: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_sub_checked(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Checked multiplication with overflow detection.
    public static func mulChecked(_ a: Int64, _ b: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_mul_checked(a, b)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Safe absolute value. Returns .failure(.overflow) for Int64.min.
    public static func absSafe(_ n: Int64) -> Result<Int64, ProvenError> {
        let result = proven_math_abs_safe(n)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Clamp a value to the range [lo, hi].
    public static func clamp(lo: Int64, hi: Int64, value: Int64) -> Int64 {
        proven_math_clamp(lo, hi, value)
    }

    /// Integer exponentiation with overflow checking.
    public static func powChecked(base: Int64, exp: UInt32) -> Result<Int64, ProvenError> {
        let result = proven_math_pow_checked(base, exp)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }
}
