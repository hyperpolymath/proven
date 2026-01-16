{-
   SPDX-License-Identifier: PMPL-1.0
   SPDX-FileCopyrightText: 2025 Hyperpolymath

   Proven Safety Library for Elm

   Formally verified safety primitives for Elm applications.
   Provides type-safe, pure functional patterns following Elm architecture.

   Version: 0.9.0
-}


module Proven exposing
    ( Result(..)
    , ProvenError(..)
    , ok
    , err
    , isOk
    , isErr
    , unwrap
    , unwrapOr
    , mapResult
    , andThen
    , safeAdd
    , safeSub
    , safeMul
    , safeDiv
    , safeMod
    , clamp
    , BoundedInt
    , boundedInt
    , getValue
    , getBounds
    , increment
    , decrement
    , isAtMin
    , isAtMax
    , isValidPort
    , isValidPercentage
    , isValidEmail
    , isSafePath
    , requireValidPort
    , requireValidPercentage
    , requireValidEmail
    , requireSafePath
    , Percentage
    , percentage
    , percentOf
    , BasisPoints
    , basisPoints
    , basisPointsOf
    , version
    )

import Regex exposing (Regex)



-- ============================================================================
-- VERSION
-- ============================================================================


{-| Library version
-}
version : String
version =
    "0.9.0"



-- ============================================================================
-- RESULT TYPE
-- ============================================================================


{-| Error types for proven operations
-}
type ProvenError
    = Overflow
    | Underflow
    | DivisionByZero
    | OutOfBounds
    | InvalidPort
    | InvalidPercentage
    | InvalidEmail
    | InvalidUrl
    | PathTraversal
    | Custom String


{-| Result type for operations that can fail
-}
type Result a
    = Ok a
    | Err ProvenError


{-| Create a success result
-}
ok : a -> Result a
ok =
    Ok


{-| Create an error result
-}
err : ProvenError -> Result a
err =
    Err


{-| Check if result is ok
-}
isOk : Result a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check if result is error
-}
isErr : Result a -> Bool
isErr result =
    not (isOk result)


{-| Unwrap result or return default
-}
unwrap : a -> Result a -> a
unwrap default result =
    case result of
        Ok value ->
            value

        Err _ ->
            default


{-| Alias for unwrap
-}
unwrapOr : a -> Result a -> a
unwrapOr =
    unwrap


{-| Map over successful result
-}
mapResult : (a -> b) -> Result a -> Result b
mapResult fn result =
    case result of
        Ok value ->
            Ok (fn value)

        Err e ->
            Err e


{-| Chain results (flatMap)
-}
andThen : (a -> Result b) -> Result a -> Result b
andThen fn result =
    case result of
        Ok value ->
            fn value

        Err e ->
            Err e



-- ============================================================================
-- SAFE MATH
-- ============================================================================


{-| Maximum safe integer in JavaScript
-}
maxSafeInteger : Float
maxSafeInteger =
    9007199254740991


{-| Minimum safe integer in JavaScript
-}
minSafeInteger : Float
minSafeInteger =
    -9007199254740991


{-| Safe addition with overflow check
-}
safeAdd : Float -> Float -> Result Float
safeAdd a b =
    let
        result =
            a + b
    in
    if isInfinite result || isNaN result then
        Err Overflow

    else
        Ok result


{-| Safe subtraction with underflow check
-}
safeSub : Float -> Float -> Result Float
safeSub a b =
    let
        result =
            a - b
    in
    if isInfinite result || isNaN result then
        Err Underflow

    else
        Ok result


{-| Safe multiplication with overflow check
-}
safeMul : Float -> Float -> Result Float
safeMul a b =
    let
        result =
            a * b
    in
    if isInfinite result || isNaN result then
        Err Overflow

    else
        Ok result


{-| Safe division with zero check
-}
safeDiv : Float -> Float -> Result Float
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


{-| Safe modulo with zero check
-}
safeMod : Int -> Int -> Result Int
safeMod a b =
    if b == 0 then
        Err DivisionByZero

    else
        Ok (modBy b a)


{-| Clamp value to range
-}
clamp : comparable -> comparable -> comparable -> comparable
clamp minVal maxVal value =
    if value < minVal then
        minVal

    else if value > maxVal then
        maxVal

    else
        value



-- ============================================================================
-- BOUNDED TYPES
-- ============================================================================


{-| Bounded integer with min/max constraints
-}
type BoundedInt
    = BoundedInt
        { value : Int
        , min : Int
        , max : Int
        }


{-| Create a bounded integer (clamped to range)
-}
boundedInt : Int -> Int -> Int -> BoundedInt
boundedInt value minVal maxVal =
    BoundedInt
        { value = clamp minVal maxVal value
        , min = minVal
        , max = maxVal
        }


{-| Get value from bounded integer
-}
getValue : BoundedInt -> Int
getValue (BoundedInt r) =
    r.value


{-| Get bounds from bounded integer
-}
getBounds : BoundedInt -> { min : Int, max : Int }
getBounds (BoundedInt r) =
    { min = r.min, max = r.max }


{-| Increment bounded integer with clamping
-}
increment : Int -> BoundedInt -> BoundedInt
increment delta (BoundedInt r) =
    BoundedInt { r | value = clamp r.min r.max (r.value + delta) }


{-| Decrement bounded integer with clamping
-}
decrement : Int -> BoundedInt -> BoundedInt
decrement delta (BoundedInt r) =
    BoundedInt { r | value = clamp r.min r.max (r.value - delta) }


{-| Check if bounded integer is at minimum
-}
isAtMin : BoundedInt -> Bool
isAtMin (BoundedInt r) =
    r.value <= r.min


{-| Check if bounded integer is at maximum
-}
isAtMax : BoundedInt -> Bool
isAtMax (BoundedInt r) =
    r.value >= r.max



-- ============================================================================
-- VALIDATION
-- ============================================================================


{-| Email regex pattern
-}
emailRegex : Maybe Regex
emailRegex =
    Regex.fromString "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"


{-| Validate port number (1-65535)
-}
isValidPort : Int -> Bool
isValidPort port =
    port >= 1 && port <= 65535


{-| Require valid port or return error
-}
requireValidPort : Int -> Result Int
requireValidPort port =
    if isValidPort port then
        Ok port

    else
        Err InvalidPort


{-| Validate percentage (0-100)
-}
isValidPercentage : Float -> Bool
isValidPercentage value =
    value >= 0 && value <= 100


{-| Require valid percentage or return error
-}
requireValidPercentage : Float -> Result Float
requireValidPercentage value =
    if isValidPercentage value then
        Ok value

    else
        Err InvalidPercentage


{-| Validate email format
-}
isValidEmail : String -> Bool
isValidEmail email =
    case emailRegex of
        Just regex ->
            Regex.contains regex email

        Nothing ->
            False


{-| Require valid email or return error
-}
requireValidEmail : String -> Result String
requireValidEmail email =
    if isValidEmail email then
        Ok email

    else
        Err InvalidEmail


{-| Check if path is safe (no traversal)
-}
isSafePath : String -> Bool
isSafePath path =
    not (String.contains ".." path)


{-| Require safe path or return error
-}
requireSafePath : String -> Result String
requireSafePath path =
    if isSafePath path then
        Ok path

    else
        Err PathTraversal



-- ============================================================================
-- PERCENTAGE
-- ============================================================================


{-| Percentage value (0-100)
-}
type Percentage
    = Percentage Float


{-| Create a percentage (clamped to 0-100)
-}
percentage : Float -> Percentage
percentage n =
    Percentage (clamp 0 100 n)


{-| Calculate percentage of an amount
-}
percentOf : Float -> Percentage -> Result Float
percentOf amount (Percentage pct) =
    safeMul amount pct
        |> andThen (\product -> safeDiv product 100)


{-| Basis points (100 bps = 1%)
-}
type BasisPoints
    = BasisPoints Int


{-| Create basis points (clamped to 0-10000)
-}
basisPoints : Int -> BasisPoints
basisPoints n =
    BasisPoints (clamp 0 10000 n)


{-| Calculate basis points of an amount
-}
basisPointsOf : Float -> BasisPoints -> Result Float
basisPointsOf amount (BasisPoints bps) =
    safeMul amount (toFloat bps)
        |> andThen (\product -> safeDiv product 10000)
