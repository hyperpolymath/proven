// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Safe floating-point operations via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeFloat =

    /// Safe floating-point division. Returns None for division by zero or NaN result.
    let div (a: float) (b: float) : float option =
        FFI.proven_float_div(a, b) |> floatResultToOption

    /// Check if float is finite (not NaN or Inf).
    let isFinite (x: float) : bool =
        FFI.proven_float_is_finite(x)

    /// Check if float is NaN.
    let isNaN (x: float) : bool =
        FFI.proven_float_is_nan(x)

    /// Safe square root. Returns None for negative input.
    let sqrt (x: float) : float option =
        FFI.proven_float_sqrt(x) |> floatResultToOption

    /// Safe natural logarithm. Returns None for non-positive input.
    let ln (x: float) : float option =
        FFI.proven_float_ln(x) |> floatResultToOption
