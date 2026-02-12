// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe mathematical operations with overflow protection.
module SafeMath =
    open System

    let MaxLong = Int64.MaxValue
    let MinLong = Int64.MinValue

    /// Checked addition that detects overflow.
    let add (a: int64) (b: int64) : int64 option =
        try
            Some(Checked.(+) a b)
        with
        | :? OverflowException -> None

    /// Checked subtraction that detects overflow.
    let sub (a: int64) (b: int64) : int64 option =
        try
            Some(Checked.(-) a b)
        with
        | :? OverflowException -> None

    /// Checked multiplication that detects overflow.
    let mul (a: int64) (b: int64) : int64 option =
        try
            Some(Checked.(*) a b)
        with
        | :? OverflowException -> None

    /// Checked division that handles division by zero.
    let div (numerator: int64) (denominator: int64) : int64 option =
        if denominator = 0L then None
        elif numerator = MinLong && denominator = -1L then None
        else Some(numerator / denominator)

    /// Checked modulo operation.
    let modSafe (a: int64) (b: int64) : int64 option =
        if b = 0L then None
        else Some(a % b)

    /// Checked negation.
    let neg (a: int64) : int64 option =
        try
            Some(Checked.(~-) a)
        with
        | :? OverflowException -> None

    /// Checked absolute value.
    let absSafe (a: int64) : int64 option =
        if a = MinLong then None
        else Some(Math.Abs(a))

    /// Checked power operation.
    let pow (base': int64) (exponent: int) : int64 option =
        if exponent < 0 then None
        elif exponent = 0 then Some(1L)
        elif base' = 0L then Some(0L)
        elif base' = 1L then Some(1L)
        elif base' = -1L then Some(if exponent % 2 = 0 then 1L else -1L)
        else
            let result = bigint.Pow(bigint base', exponent)
            if result > bigint MaxLong || result < bigint MinLong then None
            else Some(int64 result)

    /// Clamp value to range.
    let clamp (value: int64) (minVal: int64) (maxVal: int64) : int64 =
        Math.Min(maxVal, Math.Max(minVal, value))

    /// Check if addition would overflow.
    let wouldAddOverflow (a: int64) (b: int64) : bool =
        (add a b).IsNone

    /// Check if multiplication would overflow.
    let wouldMulOverflow (a: int64) (b: int64) : bool =
        (mul a b).IsNone

    /// Checked addition for int32.
    let addInt (a: int) (b: int) : int option =
        try
            Some(Checked.(+) a b)
        with
        | :? OverflowException -> None

    /// Checked subtraction for int32.
    let subInt (a: int) (b: int) : int option =
        try
            Some(Checked.(-) a b)
        with
        | :? OverflowException -> None

    /// Checked multiplication for int32.
    let mulInt (a: int) (b: int) : int option =
        try
            Some(Checked.(*) a b)
        with
        | :? OverflowException -> None

    /// Sum a list with overflow checking.
    let safeSum (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> add a x
        ) (Some 0L)

    /// Multiply a list with overflow checking.
    let safeProduct (values: int64 list) : int64 option =
        values |> List.fold (fun acc x ->
            match acc with
            | None -> None
            | Some a -> mul a x
        ) (Some 1L)
