// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath.pike - Overflow-safe integer arithmetic for Pike.
//
// All operations delegate to libproven via LibProven. Returns UNDEFINED
// on error (overflow, division by zero, etc.).
//
// Usage:
//   import Proven;
//   LibProven.init();
//   int result = SafeMath.add(1000000000, 2000000000);
//   if (!zero_type(result))
//       write("Sum: %d\n", result);
//   LibProven.deinit();

//! @class SafeMath
//! Overflow-checked integer arithmetic.
//!
//! Every method returns @expr{UNDEFINED@} when the operation would produce
//! an invalid result (overflow, underflow, division by zero). This prevents
//! silent wraparound bugs.

protected LibProven lib = LibProven();

//! @decl int|zero add(int a, int b)
//! Checked addition.
//! @returns
//!   Sum of @[a] and @[b], or @expr{UNDEFINED@} on overflow.
int|zero add(int a, int b)
{
    return lib->call_int("math_add_checked", ({(string)a, (string)b}));
}

//! @decl int|zero sub(int a, int b)
//! Checked subtraction.
//! @returns
//!   Difference of @[a] and @[b], or @expr{UNDEFINED@} on underflow.
int|zero sub(int a, int b)
{
    return lib->call_int("math_sub_checked", ({(string)a, (string)b}));
}

//! @decl int|zero mul(int a, int b)
//! Checked multiplication.
//! @returns
//!   Product of @[a] and @[b], or @expr{UNDEFINED@} on overflow.
int|zero mul(int a, int b)
{
    return lib->call_int("math_mul_checked", ({(string)a, (string)b}));
}

//! @decl int|zero div(int a, int b)
//! Safe integer division.
//! @returns
//!   Quotient of @[a] and @[b], or @expr{UNDEFINED@} on division by
//!   zero or overflow (INT64_MIN / -1).
int|zero div(int a, int b)
{
    return lib->call_int("math_div", ({(string)a, (string)b}));
}

//! @decl int|zero mod(int a, int b)
//! Safe modulo operation.
//! @returns
//!   Remainder of @[a] / @[b], or @expr{UNDEFINED@} on division by zero.
int|zero mod(int a, int b)
{
    return lib->call_int("math_mod", ({(string)a, (string)b}));
}

//! @decl int|zero abs(int n)
//! Safe absolute value.
//! @returns
//!   Absolute value of @[n], or @expr{UNDEFINED@} for INT64_MIN.
int|zero abs(int n)
{
    return lib->call_int("math_abs_safe", ({(string)n}));
}

//! @decl int clamp(int lo, int hi, int value)
//! Clamp @[value] to the range [@[lo], @[hi]].
//! @returns
//!   The clamped value.
int clamp(int lo, int hi, int value)
{
    int|zero result = lib->call_int("math_clamp", ({(string)lo, (string)hi, (string)value}));
    // clamp always succeeds for valid integers
    return result || 0;
}

//! @decl int|zero pow(int base, int exp)
//! Checked integer exponentiation.
//! @returns
//!   @[base] raised to the power @[exp], or @expr{UNDEFINED@} on overflow.
int|zero pow(int base, int exp)
{
    return lib->call_int("math_pow_checked", ({(string)base, (string)exp}));
}
