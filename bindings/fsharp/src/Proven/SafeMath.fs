// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Overflow-checked arithmetic via P/Invoke to libproven.
/// All computation is performed in verified Idris 2 code via the Zig FFI bridge.
/// This module contains ONLY marshaling logic.
namespace Proven

module SafeMath =

    /// Safe division. Returns None on division by zero or INT64_MIN / -1.
    let div (a: int64) (b: int64) : int64 option =
        FFI.proven_math_div(a, b) |> intResultToOption

    /// Safe modulo. Returns None on division by zero.
    let mod' (a: int64) (b: int64) : int64 option =
        FFI.proven_math_mod(a, b) |> intResultToOption

    /// Checked addition. Returns None on overflow.
    let add (a: int64) (b: int64) : int64 option =
        FFI.proven_math_add_checked(a, b) |> intResultToOption

    /// Checked subtraction. Returns None on underflow.
    let sub (a: int64) (b: int64) : int64 option =
        FFI.proven_math_sub_checked(a, b) |> intResultToOption

    /// Checked multiplication. Returns None on overflow.
    let mul (a: int64) (b: int64) : int64 option =
        FFI.proven_math_mul_checked(a, b) |> intResultToOption

    /// Safe absolute value. Returns None for INT64_MIN.
    let abs (n: int64) : int64 option =
        FFI.proven_math_abs_safe(n) |> intResultToOption

    /// Clamp value to [lo, hi] range.
    let clamp (lo: int64) (hi: int64) (value: int64) : int64 =
        FFI.proven_math_clamp(lo, hi, value)

    /// Integer power with overflow checking. Returns None on overflow.
    let pow (base_: int64) (exp: uint32) : int64 option =
        FFI.proven_math_pow_checked(base_, exp) |> intResultToOption

    /// Safely sum a list. Returns None if any addition overflows.
    let safeSum (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> add a x
        ) (Some 0L)

    /// Safely compute product of a list. Returns None if any multiply overflows.
    let safeProduct (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> mul a x
        ) (Some 1L)
