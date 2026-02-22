-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Safe arithmetic operations via libproven FFI.
|||
||| All computation is performed in the formally verified Idris 2 core
||| through the precompiled shared library. This module provides safe
||| wrappers that return `Maybe` types for fallible operations and
||| use the `HasIO` constraint for effectful C calls.
|||
||| No arithmetic logic is reimplemented here. Every function delegates
||| to the corresponding `proven_math_*` symbol in libproven.
module Proven.SafeMath

import Proven.FFI

%default total

-- ============================================================================
-- Helper: decode (status, value) tuples from C
-- ============================================================================

||| Convert a C int result tuple to Maybe.
||| Status 0 means success; anything else means failure.
intResultToMaybe : (Int, Int64) -> Maybe Int64
intResultToMaybe (s, v) = if isOK s then Just v else Nothing

-- ============================================================================
-- Checked arithmetic
-- ============================================================================

||| Safe addition with overflow detection.
|||
||| Returns `Nothing` if the result would overflow Int64.
||| All computation is performed by the verified Idris 2 core in libproven.
||| @ a First operand
||| @ b Second operand
public export
safeAdd : HasIO io => (a : Int64) -> (b : Int64) -> io (Maybe Int64)
safeAdd a b = do
  result <- primIO $ prim__proven_math_add_checked a b
  pure (intResultToMaybe result)

||| Safe subtraction with underflow detection.
|||
||| Returns `Nothing` if the result would underflow Int64.
||| @ a First operand
||| @ b Second operand
public export
safeSub : HasIO io => (a : Int64) -> (b : Int64) -> io (Maybe Int64)
safeSub a b = do
  result <- primIO $ prim__proven_math_sub_checked a b
  pure (intResultToMaybe result)

||| Safe multiplication with overflow detection.
|||
||| Returns `Nothing` if the result would overflow Int64.
||| @ a First operand
||| @ b Second operand
public export
safeMul : HasIO io => (a : Int64) -> (b : Int64) -> io (Maybe Int64)
safeMul a b = do
  result <- primIO $ prim__proven_math_mul_checked a b
  pure (intResultToMaybe result)

||| Safe integer division.
|||
||| Returns `Nothing` if the denominator is zero, or if the operation
||| would overflow (INT64_MIN / -1).
||| @ numerator   Dividend
||| @ denominator Divisor
public export
safeDiv : HasIO io => (numerator : Int64) -> (denominator : Int64) -> io (Maybe Int64)
safeDiv n d = do
  result <- primIO $ prim__proven_math_div n d
  pure (intResultToMaybe result)

||| Safe modulo operation.
|||
||| Returns `Nothing` if the denominator is zero.
||| @ numerator   Dividend
||| @ denominator Divisor
public export
safeMod : HasIO io => (numerator : Int64) -> (denominator : Int64) -> io (Maybe Int64)
safeMod n d = do
  result <- primIO $ prim__proven_math_mod n d
  pure (intResultToMaybe result)

||| Safe absolute value.
|||
||| Returns `Nothing` for INT64_MIN (whose absolute value cannot be
||| represented as a positive Int64).
||| @ n Value to take the absolute value of
public export
safeAbs : HasIO io => (n : Int64) -> io (Maybe Int64)
safeAbs n = do
  result <- primIO $ prim__proven_math_abs_safe n
  pure (intResultToMaybe result)

||| Integer exponentiation with overflow checking.
|||
||| Returns `Nothing` if the result would overflow Int64 or if the
||| exponent is negative.
||| @ base Base value
||| @ exp  Non-negative exponent (unsigned 32-bit)
public export
safePow : HasIO io => (base : Int64) -> (exp : Int) -> io (Maybe Int64)
safePow b e = do
  result <- primIO $ prim__proven_math_pow_checked b e
  pure (intResultToMaybe result)

-- ============================================================================
-- Pure operations (no error case)
-- ============================================================================

||| Clamp a value to the range [lo, hi].
|||
||| This is a pure function that calls directly into libproven. It cannot
||| fail: if `value < lo` returns `lo`, if `value > hi` returns `hi`,
||| otherwise returns `value`.
||| @ lo    Lower bound (inclusive)
||| @ hi    Upper bound (inclusive)
||| @ value Value to clamp
public export
clamp : (lo : Int64) -> (hi : Int64) -> (value : Int64) -> Int64
clamp = prim__proven_math_clamp

-- ============================================================================
-- Typed error variants (returning Either ProvenError Int64)
-- ============================================================================

||| Convert a C int result tuple to Either ProvenError.
intResultToEither : (Int, Int64) -> Either ProvenError Int64
intResultToEither (s, v) = case statusToError s of
  Nothing  => Right v
  Just err => Left err

||| Safe addition returning a typed error on failure.
|||
||| Returns `Left Overflow` if the result would overflow,
||| `Right value` on success.
public export
safeAddE : HasIO io => Int64 -> Int64 -> io (Either ProvenError Int64)
safeAddE a b = do
  result <- primIO $ prim__proven_math_add_checked a b
  pure (intResultToEither result)

||| Safe subtraction returning a typed error on failure.
public export
safeSubE : HasIO io => Int64 -> Int64 -> io (Either ProvenError Int64)
safeSubE a b = do
  result <- primIO $ prim__proven_math_sub_checked a b
  pure (intResultToEither result)

||| Safe multiplication returning a typed error on failure.
public export
safeMulE : HasIO io => Int64 -> Int64 -> io (Either ProvenError Int64)
safeMulE a b = do
  result <- primIO $ prim__proven_math_mul_checked a b
  pure (intResultToEither result)

||| Safe division returning a typed error on failure.
public export
safeDivE : HasIO io => Int64 -> Int64 -> io (Either ProvenError Int64)
safeDivE n d = do
  result <- primIO $ prim__proven_math_div n d
  pure (intResultToEither result)

||| Safe modulo returning a typed error on failure.
public export
safeModE : HasIO io => Int64 -> Int64 -> io (Either ProvenError Int64)
safeModE n d = do
  result <- primIO $ prim__proven_math_mod n d
  pure (intResultToEither result)
