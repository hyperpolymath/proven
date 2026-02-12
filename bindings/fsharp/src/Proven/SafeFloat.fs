// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe floating-point operations with NaN/Infinity handling.
module SafeFloat =
    open System

    /// Floating-point operation errors.
    type FloatError =
        | NotANumber
        | PositiveInfinity
        | NegativeInfinity
        | Underflow
        | DivisionByZero
        | InvalidOperation of string

    /// Check if float is NaN.
    let isNaN (value: float) : bool = Double.IsNaN(value)

    /// Check if float is infinite.
    let isInfinite (value: float) : bool = Double.IsInfinity(value)

    /// Check if float is positive infinity.
    let isPositiveInfinity (value: float) : bool = Double.IsPositiveInfinity(value)

    /// Check if float is negative infinity.
    let isNegativeInfinity (value: float) : bool = Double.IsNegativeInfinity(value)

    /// Check if float is finite (not NaN or Infinity).
    let isFinite (value: float) : bool =
        not (isNaN value) && not (isInfinite value)

    /// Check if float is subnormal (denormalized).
    let isSubnormal (value: float) : bool =
        Double.IsSubnormal(value)

    /// Check if float is normal (not zero, subnormal, infinite, or NaN).
    let isNormal (value: float) : bool =
        Double.IsNormal(value)

    /// Check if float is negative.
    let isNegative (value: float) : bool =
        Double.IsNegative(value)

    /// Check if float is positive.
    let isPositive (value: float) : bool =
        value > 0.0

    /// Check if float is zero (positive or negative).
    let isZero (value: float) : bool =
        value = 0.0 || value = -0.0

    /// Validate float is finite, returning Result.
    let validate (value: float) : Result<float, FloatError> =
        if isNaN value then Error NotANumber
        elif isPositiveInfinity value then Error PositiveInfinity
        elif isNegativeInfinity value then Error NegativeInfinity
        else Ok value

    /// Validate float, returning Option.
    let tryValidate (value: float) : float option =
        match validate value with
        | Ok v -> Some v
        | Error _ -> None

    /// Safe addition.
    let add (a: float) (b: float) : Result<float, FloatError> =
        let result = a + b
        validate result

    /// Safe subtraction.
    let sub (a: float) (b: float) : Result<float, FloatError> =
        let result = a - b
        validate result

    /// Safe multiplication.
    let mul (a: float) (b: float) : Result<float, FloatError> =
        let result = a * b
        validate result

    /// Safe division.
    let div (a: float) (b: float) : Result<float, FloatError> =
        if b = 0.0 then Error DivisionByZero
        else
            let result = a / b
            validate result

    /// Safe modulo.
    let modSafe (a: float) (b: float) : Result<float, FloatError> =
        if b = 0.0 then Error DivisionByZero
        else
            let result = a % b
            validate result

    /// Safe power.
    let pow (base': float) (exponent: float) : Result<float, FloatError> =
        let result = Math.Pow(base', exponent)
        validate result

    /// Safe square root.
    let sqrt (value: float) : Result<float, FloatError> =
        if value < 0.0 then Error NotANumber
        else
            let result = Math.Sqrt(value)
            validate result

    /// Safe logarithm (natural).
    let log (value: float) : Result<float, FloatError> =
        if value <= 0.0 then Error(InvalidOperation "log of non-positive")
        else
            let result = Math.Log(value)
            validate result

    /// Safe logarithm (base 10).
    let log10 (value: float) : Result<float, FloatError> =
        if value <= 0.0 then Error(InvalidOperation "log10 of non-positive")
        else
            let result = Math.Log10(value)
            validate result

    /// Safe logarithm (arbitrary base).
    let logBase (base': float) (value: float) : Result<float, FloatError> =
        if value <= 0.0 || base' <= 0.0 || base' = 1.0 then
            Error(InvalidOperation "invalid log base")
        else
            let result = Math.Log(value, base')
            validate result

    /// Safe exponential.
    let exp (value: float) : Result<float, FloatError> =
        let result = Math.Exp(value)
        validate result

    /// Safe absolute value.
    let abs (value: float) : float = Math.Abs(value)

    /// Safe negation.
    let neg (value: float) : float = -value

    /// Safe sign (-1, 0, or 1).
    let sign (value: float) : int = Math.Sign(value)

    /// Safe ceiling.
    let ceiling (value: float) : Result<float, FloatError> =
        let result = Math.Ceiling(value)
        validate result

    /// Safe floor.
    let floor (value: float) : Result<float, FloatError> =
        let result = Math.Floor(value)
        validate result

    /// Safe round (to nearest integer).
    let round (value: float) : Result<float, FloatError> =
        let result = Math.Round(value)
        validate result

    /// Safe round to specified decimal places.
    let roundTo (decimalPlaces: int) (value: float) : Result<float, FloatError> =
        if decimalPlaces < 0 || decimalPlaces > 15 then
            Error(InvalidOperation "invalid decimal places")
        else
            let result = Math.Round(value, decimalPlaces)
            validate result

    /// Safe truncation (towards zero).
    let truncate (value: float) : Result<float, FloatError> =
        let result = Math.Truncate(value)
        validate result

    /// Clamp value to range.
    let clamp (minVal: float) (maxVal: float) (value: float) : float =
        Math.Clamp(value, minVal, maxVal)

    /// Safe minimum (handles NaN).
    let minSafe (a: float) (b: float) : float option =
        if isNaN a || isNaN b then None
        else Some(Math.Min(a, b))

    /// Safe maximum (handles NaN).
    let maxSafe (a: float) (b: float) : float option =
        if isNaN a || isNaN b then None
        else Some(Math.Max(a, b))

    /// Safe sine.
    let sin (value: float) : Result<float, FloatError> =
        let result = Math.Sin(value)
        validate result

    /// Safe cosine.
    let cos (value: float) : Result<float, FloatError> =
        let result = Math.Cos(value)
        validate result

    /// Safe tangent.
    let tan (value: float) : Result<float, FloatError> =
        let result = Math.Tan(value)
        validate result

    /// Safe arc sine.
    let asin (value: float) : Result<float, FloatError> =
        if value < -1.0 || value > 1.0 then Error(InvalidOperation "asin out of range")
        else
            let result = Math.Asin(value)
            validate result

    /// Safe arc cosine.
    let acos (value: float) : Result<float, FloatError> =
        if value < -1.0 || value > 1.0 then Error(InvalidOperation "acos out of range")
        else
            let result = Math.Acos(value)
            validate result

    /// Safe arc tangent.
    let atan (value: float) : Result<float, FloatError> =
        let result = Math.Atan(value)
        validate result

    /// Safe atan2.
    let atan2 (y: float) (x: float) : Result<float, FloatError> =
        let result = Math.Atan2(y, x)
        validate result

    /// Safe hyperbolic sine.
    let sinh (value: float) : Result<float, FloatError> =
        let result = Math.Sinh(value)
        validate result

    /// Safe hyperbolic cosine.
    let cosh (value: float) : Result<float, FloatError> =
        let result = Math.Cosh(value)
        validate result

    /// Safe hyperbolic tangent.
    let tanh (value: float) : Result<float, FloatError> =
        let result = Math.Tanh(value)
        validate result

    /// Compare floats with tolerance.
    let almostEqual (tolerance: float) (a: float) (b: float) : bool =
        if isNaN a || isNaN b then false
        elif isInfinite a && isInfinite b then a = b
        else abs(a - b) <= tolerance

    /// Compare floats with relative tolerance.
    let almostEqualRelative (relativeTolerance: float) (a: float) (b: float) : bool =
        if isNaN a || isNaN b then false
        elif isInfinite a && isInfinite b then a = b
        elif a = 0.0 && b = 0.0 then true
        else
            let maxAbs = max (abs a) (abs b)
            abs(a - b) <= relativeTolerance * maxAbs

    /// Linear interpolation.
    let lerp (a: float) (b: float) (t: float) : Result<float, FloatError> =
        if t < 0.0 || t > 1.0 then Error(InvalidOperation "t must be in [0, 1]")
        else
            let result = a + (b - a) * t
            validate result

    /// Inverse linear interpolation.
    let inverseLerp (a: float) (b: float) (value: float) : Result<float, FloatError> =
        if a = b then Error DivisionByZero
        else
            let result = (value - a) / (b - a)
            validate result

    /// Safe conversion to int.
    let toInt (value: float) : int option =
        if isNaN value || isInfinite value then None
        elif value < float Int32.MinValue || value > float Int32.MaxValue then None
        else Some(int value)

    /// Safe conversion to int64.
    let toInt64 (value: float) : int64 option =
        if isNaN value || isInfinite value then None
        elif value < float Int64.MinValue || value > float Int64.MaxValue then None
        else Some(int64 value)

    /// Safe conversion from string.
    let parse (input: string) : Result<float, FloatError> =
        match Double.TryParse(input) with
        | true, value -> validate value
        | false, _ -> Error(InvalidOperation "failed to parse")

    /// Safe conversion from string, returning Option.
    let tryParse (input: string) : float option =
        match parse input with
        | Ok v -> Some v
        | Error _ -> None

    /// Format with specified decimal places.
    let formatDecimal (decimalPlaces: int) (value: float) : string =
        if isNaN value then "NaN"
        elif isPositiveInfinity value then "Infinity"
        elif isNegativeInfinity value then "-Infinity"
        else value.ToString(sprintf "F%d" decimalPlaces)

    /// Format in scientific notation.
    let formatScientific (decimalPlaces: int) (value: float) : string =
        if isNaN value then "NaN"
        elif isPositiveInfinity value then "Infinity"
        elif isNegativeInfinity value then "-Infinity"
        else value.ToString(sprintf "E%d" decimalPlaces)

    /// Machine epsilon.
    let epsilon : float = Double.Epsilon

    /// Minimum positive normal value.
    let minPositiveNormal : float = 2.2250738585072014e-308

    /// Maximum finite value.
    let maxValue : float = Double.MaxValue

    /// Minimum finite value.
    let minValue : float = Double.MinValue

    /// Pi constant.
    let pi : float = Math.PI

    /// E constant.
    let e : float = Math.E

    /// Tau constant (2 * pi).
    let tau : float = 2.0 * Math.PI
