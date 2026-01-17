-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeMath exposing
    ( add, sub, mul, div, mod
    , safeAdd, safeSub, safeMul, safeDiv, safeMod
    , clamp, clampInt
    , abs, negate, sign
    , isEven, isOdd
    , BoundedInt, boundedInt, getValue, getBounds, increment, decrement, isAtMin, isAtMax
    , Percentage, percentage, getPercentage, percentOf
    , BasisPoints, basisPoints, getBasisPoints, basisPointsOf
    , Error(..)
    )

{-| Safe mathematical operations with overflow detection.

Provides safe arithmetic operations that return Maybe or Result
instead of silently overflowing or producing incorrect results.


# Basic Safe Operations

@docs add, sub, mul, div, mod


# Result-based Operations

@docs safeAdd, safeSub, safeMul, safeDiv, safeMod


# Clamping

@docs clamp, clampInt


# Utilities

@docs abs, negate, sign
@docs isEven, isOdd


# Bounded Integers

@docs BoundedInt, boundedInt, getValue, getBounds, increment, decrement, isAtMin, isAtMax


# Percentage

@docs Percentage, percentage, getPercentage, percentOf


# Basis Points

@docs BasisPoints, basisPoints, getBasisPoints, basisPointsOf


# Errors

@docs Error

-}


{-| Error types for safe math operations
-}
type Error
    = Overflow
    | Underflow
    | DivisionByZero
    | InvalidInput String



-- ============================================================================
-- BASIC SAFE OPERATIONS
-- ============================================================================


{-| Safe addition returning Maybe. Returns Nothing on overflow.
-}
add : Float -> Float -> Maybe Float
add a b =
    let
        result =
            a + b
    in
    if isInfinite result || isNaN result then
        Nothing

    else
        Just result


{-| Safe subtraction returning Maybe. Returns Nothing on underflow.
-}
sub : Float -> Float -> Maybe Float
sub a b =
    let
        result =
            a - b
    in
    if isInfinite result || isNaN result then
        Nothing

    else
        Just result


{-| Safe multiplication returning Maybe. Returns Nothing on overflow.
-}
mul : Float -> Float -> Maybe Float
mul a b =
    let
        result =
            a * b
    in
    if isInfinite result || isNaN result then
        Nothing

    else
        Just result


{-| Safe division returning Maybe. Returns Nothing on division by zero.
-}
div : Float -> Float -> Maybe Float
div a b =
    if b == 0 then
        Nothing

    else
        let
            result =
                a / b
        in
        if isInfinite result || isNaN result then
            Nothing

        else
            Just result


{-| Safe modulo returning Maybe. Returns Nothing on division by zero.
-}
mod : Int -> Int -> Maybe Int
mod a b =
    if b == 0 then
        Nothing

    else
        Just (modBy b a)



-- ============================================================================
-- RESULT-BASED OPERATIONS
-- ============================================================================


{-| Safe addition returning Result with error type.
-}
safeAdd : Float -> Float -> Result Error Float
safeAdd a b =
    let
        result =
            a + b
    in
    if isInfinite result || isNaN result then
        Err Overflow

    else
        Ok result


{-| Safe subtraction returning Result with error type.
-}
safeSub : Float -> Float -> Result Error Float
safeSub a b =
    let
        result =
            a - b
    in
    if isInfinite result || isNaN result then
        Err Underflow

    else
        Ok result


{-| Safe multiplication returning Result with error type.
-}
safeMul : Float -> Float -> Result Error Float
safeMul a b =
    let
        result =
            a * b
    in
    if isInfinite result || isNaN result then
        Err Overflow

    else
        Ok result


{-| Safe division returning Result with error type.
-}
safeDiv : Float -> Float -> Result Error Float
safeDiv a b =
    if b == 0 then
        Err DivisionByZero

    else
        let
            result =
                a / b
        in
        if isInfinite result || isNaN result then
            Err Overflow

        else
            Ok result


{-| Safe modulo returning Result with error type.
-}
safeMod : Int -> Int -> Result Error Int
safeMod a b =
    if b == 0 then
        Err DivisionByZero

    else
        Ok (modBy b a)



-- ============================================================================
-- CLAMPING
-- ============================================================================


{-| Clamp a value to a range.
-}
clamp : comparable -> comparable -> comparable -> comparable
clamp minVal maxVal value =
    if value < minVal then
        minVal

    else if value > maxVal then
        maxVal

    else
        value


{-| Clamp an integer to a range.
-}
clampInt : Int -> Int -> Int -> Int
clampInt minVal maxVal value =
    clamp minVal maxVal value



-- ============================================================================
-- UTILITIES
-- ============================================================================


{-| Safe absolute value.
-}
abs : number -> number
abs n =
    if n < 0 then
        -n

    else
        n


{-| Safe negate.
-}
negate : number -> number
negate n =
    -n


{-| Get the sign of a number: -1, 0, or 1.
-}
sign : Float -> Int
sign n =
    if n > 0 then
        1

    else if n < 0 then
        -1

    else
        0


{-| Check if an integer is even.
-}
isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


{-| Check if an integer is odd.
-}
isOdd : Int -> Bool
isOdd n =
    modBy 2 n /= 0



-- ============================================================================
-- BOUNDED INTEGERS
-- ============================================================================


{-| A bounded integer with enforced minimum and maximum values.
-}
type BoundedInt
    = BoundedInt
        { value : Int
        , min : Int
        , max : Int
        }


{-| Create a bounded integer. Value is clamped to the range.
-}
boundedInt : Int -> Int -> Int -> BoundedInt
boundedInt value minVal maxVal =
    BoundedInt
        { value = clampInt minVal maxVal value
        , min = minVal
        , max = maxVal
        }


{-| Get the value from a bounded integer.
-}
getValue : BoundedInt -> Int
getValue (BoundedInt r) =
    r.value


{-| Get the bounds from a bounded integer.
-}
getBounds : BoundedInt -> { min : Int, max : Int }
getBounds (BoundedInt r) =
    { min = r.min, max = r.max }


{-| Increment a bounded integer, clamping to max.
-}
increment : Int -> BoundedInt -> BoundedInt
increment delta (BoundedInt r) =
    BoundedInt { r | value = clampInt r.min r.max (r.value + delta) }


{-| Decrement a bounded integer, clamping to min.
-}
decrement : Int -> BoundedInt -> BoundedInt
decrement delta (BoundedInt r) =
    BoundedInt { r | value = clampInt r.min r.max (r.value - delta) }


{-| Check if the bounded integer is at its minimum.
-}
isAtMin : BoundedInt -> Bool
isAtMin (BoundedInt r) =
    r.value <= r.min


{-| Check if the bounded integer is at its maximum.
-}
isAtMax : BoundedInt -> Bool
isAtMax (BoundedInt r) =
    r.value >= r.max



-- ============================================================================
-- PERCENTAGE
-- ============================================================================


{-| A percentage value clamped to 0-100.
-}
type Percentage
    = Percentage Float


{-| Create a percentage (clamped to 0-100).
-}
percentage : Float -> Percentage
percentage n =
    Percentage (clamp 0 100 n)


{-| Get the raw percentage value.
-}
getPercentage : Percentage -> Float
getPercentage (Percentage p) =
    p


{-| Calculate a percentage of an amount.
-}
percentOf : Float -> Percentage -> Maybe Float
percentOf amount (Percentage pct) =
    mul amount pct
        |> Maybe.andThen (\product -> div product 100)



-- ============================================================================
-- BASIS POINTS
-- ============================================================================


{-| Basis points (100 bps = 1%).
-}
type BasisPoints
    = BasisPoints Int


{-| Create basis points (clamped to 0-10000).
-}
basisPoints : Int -> BasisPoints
basisPoints n =
    BasisPoints (clampInt 0 10000 n)


{-| Get the raw basis points value.
-}
getBasisPoints : BasisPoints -> Int
getBasisPoints (BasisPoints bps) =
    bps


{-| Calculate basis points of an amount.
-}
basisPointsOf : Float -> BasisPoints -> Maybe Float
basisPointsOf amount (BasisPoints bps) =
    mul amount (toFloat bps)
        |> Maybe.andThen (\product -> div product 10000)
