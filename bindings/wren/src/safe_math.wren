// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath - Overflow-checked arithmetic operations via libproven FFI.
// All computation is performed in verified Idris 2 code via the Zig FFI
// layer. This Wren class is a thin wrapper; it does NOT reimplement any logic.

// Safe arithmetic operations that detect overflow, underflow, and
// division by zero. Returns null on error instead of crashing.
class SafeMath {
    // Checked addition with overflow detection.
    // Returns the sum, or null if overflow would occur.
    foreign static addChecked(a, b)

    // Checked subtraction with underflow detection.
    // Returns the difference, or null if underflow would occur.
    foreign static subChecked(a, b)

    // Checked multiplication with overflow detection.
    // Returns the product, or null if overflow would occur.
    foreign static mulChecked(a, b)

    // Safe integer division.
    // Returns the quotient, or null on division by zero.
    foreign static div(a, b)

    // Safe modulo operation.
    // Returns the remainder, or null on division by zero.
    foreign static mod(a, b)

    // Safe absolute value.
    // Returns |n|, or null for INT64_MIN.
    foreign static absSafe(n)

    // Clamp value to [lo, hi] range.
    // Always returns a value (no error case).
    foreign static clamp(lo, hi, value)

    // Checked exponentiation.
    // Returns base^exp, or null if overflow would occur.
    foreign static powChecked(base, exp)
}
