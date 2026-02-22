{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

{-
  safe_math.orc - Safe arithmetic operations for Orc

  All computation is delegated to libproven's formally verified Idris 2
  core via the JNA FFI bridge. No arithmetic logic is reimplemented here.

  Orc's concurrency model allows these operations to be composed in
  parallel using the `|` operator, enabling concurrent checked arithmetic
  across independent data streams.
-}

include "src/ffi.orc"
include "src/proven.orc"

-- ============================================================================
-- Checked Integer Arithmetic
-- ============================================================================

-- Safe addition with overflow detection.
-- Publishes the sum, or halts silently on overflow.
-- Delegates to proven_math_add_checked via FFI.
def safe_add(a, b) =
  val result = ffi_math_add_checked(a, b)
  extract_int(result)

-- Safe subtraction with underflow detection.
-- Publishes the difference, or halts silently on underflow.
-- Delegates to proven_math_sub_checked via FFI.
def safe_sub(a, b) =
  val result = ffi_math_sub_checked(a, b)
  extract_int(result)

-- Safe multiplication with overflow detection.
-- Publishes the product, or halts silently on overflow.
-- Delegates to proven_math_mul_checked via FFI.
def safe_mul(a, b) =
  val result = ffi_math_mul_checked(a, b)
  extract_int(result)

-- Safe division with division-by-zero protection.
-- Publishes the quotient, or halts silently on division by zero.
-- Delegates to proven_math_div via FFI.
def safe_div(a, b) =
  val result = ffi_math_div(a, b)
  extract_int(result)

-- Safe modulo with division-by-zero protection.
-- Publishes the remainder, or halts silently on zero divisor.
-- Delegates to proven_math_mod via FFI.
def safe_mod(a, b) =
  val result = ffi_math_mod(a, b)
  extract_int(result)

-- Safe absolute value (handles MIN_INT correctly).
-- Publishes the absolute value, or halts on overflow (MIN_INT).
-- Delegates to proven_math_abs_safe via FFI.
def safe_abs(n) =
  val result = ffi_math_abs_safe(n)
  extract_int(result)

-- Clamp value to range [lo, hi].
-- Always succeeds (no error possible).
-- Delegates to proven_math_clamp via FFI.
def clamp(value, lo, hi) = ffi_math_clamp(lo, hi, value)

-- Safe integer exponentiation with overflow detection.
-- Publishes the result, or halts silently on overflow.
-- Delegates to proven_math_pow_checked via FFI.
def safe_pow(base, exp) =
  val result = ffi_math_pow_checked(base, exp)
  extract_int(result)

-- ============================================================================
-- Concurrent Arithmetic Patterns
-- ============================================================================

-- Compute a + b and a * b concurrently, publishing both results.
-- Useful for financial calculations needing both sum and product.
def add_and_mul(a, b) = safe_add(a, b) | safe_mul(a, b)

-- Safe expression: (a + b) * c with overflow checking at each step.
-- Sequential composition ensures intermediate overflow is caught.
def safe_add_then_mul(a, b, c) =
  val sum = safe_add(a, b)
  safe_mul(sum, c)

-- Safe expression: a / b with remainder, published as a pair.
def safe_divmod(a, b) =
  val q = safe_div(a, b)
  val r = safe_mod(a, b)
  (q, r)
