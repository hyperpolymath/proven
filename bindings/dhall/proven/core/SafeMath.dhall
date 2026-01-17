-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeMath - Overflow-checked arithmetic for Dhall

All operations return Result records with { value, ok } fields.
Never throws, never crashes.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan

-- Maximum 64-bit signed integer
let MAX_INT64 = 9223372036854775807

-- Minimum 64-bit signed integer (as natural, we use 0 for underflow checks)
let MIN_INT64_ABS = 9223372036854775808

-- Result type for safe operations
let Result = { value : Natural, ok : Bool }

-- Create success result
let ok
    : Natural -> Result
    = \(value : Natural) -> { value, ok = True }

-- Create error result
let err
    : Result
    = { value = 0, ok = False }

-- Safe addition with overflow checking
let safeAdd
    : Natural -> Natural -> Result
    = \(a : Natural) -> \(b : Natural) ->
        let result = a + b
        in if Natural/greaterThan result MAX_INT64
           then err
           else ok result

-- Safe subtraction (saturating at 0 for naturals)
let safeSub
    : Natural -> Natural -> Result
    = \(a : Natural) -> \(b : Natural) ->
        if Natural/lessThan a b
        then err
        else ok (Natural/subtract b a)

-- Safe multiplication with overflow checking
let safeMul
    : Natural -> Natural -> Result
    = \(a : Natural) -> \(b : Natural) ->
        let result = a * b
        in if Natural/greaterThan result MAX_INT64
           then err
           else ok result

-- Safe division with zero check
let safeDiv
    : Natural -> Natural -> Result
    = \(a : Natural) -> \(b : Natural) ->
        if Natural/lessThan b 1
        then err
        else ok (a / b)

-- Safe modulo with zero check
let safeMod
    : Natural -> Natural -> Result
    = \(a : Natural) -> \(b : Natural) ->
        if Natural/lessThan b 1
        then err
        else ok (a % b)

-- Safe power with overflow checking
let safePow
    : Natural -> Natural -> Result
    = \(base : Natural) -> \(exp : Natural) ->
        let pow = \(b : Natural) -> \(e : Natural) ->
            let step = \(acc : Natural) -> acc * b
            in Natural/fold e Natural step 1
        let result = pow base exp
        in if Natural/greaterThan result MAX_INT64
           then err
           else ok result

-- Clamp value to range [min, max]
let clamp
    : Natural -> Natural -> Natural -> Natural
    = \(value : Natural) -> \(minVal : Natural) -> \(maxVal : Natural) ->
        if Natural/lessThan value minVal
        then minVal
        else if Natural/greaterThan value maxVal
        then maxVal
        else value

-- Check if value is in range [min, max]
let inRange
    : Natural -> Natural -> Natural -> Bool
    = \(value : Natural) -> \(minVal : Natural) -> \(maxVal : Natural) ->
        Natural/lessThan minVal (value + 1) && Natural/lessThan value (maxVal + 1)

-- Safe increment
let safeInc
    : Natural -> Result
    = \(a : Natural) -> safeAdd a 1

-- Safe decrement
let safeDec
    : Natural -> Result
    = \(a : Natural) -> safeSub a 1

-- Check if would overflow on addition
let wouldOverflowAdd
    : Natural -> Natural -> Bool
    = \(a : Natural) -> \(b : Natural) ->
        Natural/greaterThan (a + b) MAX_INT64

-- Check if would overflow on multiplication
let wouldOverflowMul
    : Natural -> Natural -> Bool
    = \(a : Natural) -> \(b : Natural) ->
        Natural/greaterThan (a * b) MAX_INT64

-- Percentage type (0-100)
let Percentage = { value : Natural }

-- Create validated percentage
let mkPercentage
    : Natural -> Optional Percentage
    = \(n : Natural) ->
        if Natural/greaterThan n 100
        then None Percentage
        else Some { value = n }

-- Port number type (1-65535)
let Port = { port : Natural }

-- Create validated port
let mkPort
    : Natural -> Optional Port
    = \(n : Natural) ->
        if Natural/lessThan n 1
        then None Port
        else if Natural/greaterThan n 65535
        then None Port
        else Some { port = n }

in {
    -- Types
    Result,
    Percentage,
    Port,

    -- Constants
    MAX_INT64,
    MIN_INT64_ABS,

    -- Constructors
    ok,
    err,
    mkPercentage,
    mkPort,

    -- Safe arithmetic
    safeAdd,
    safeSub,
    safeMul,
    safeDiv,
    safeMod,
    safePow,
    safeInc,
    safeDec,

    -- Utilities
    clamp,
    inRange,
    wouldOverflowAdd,
    wouldOverflowMul
}
