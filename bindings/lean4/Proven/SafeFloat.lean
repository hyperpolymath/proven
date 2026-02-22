/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeFloat - Safe floating-point operations

Provides safe floating-point division, square root, and natural logarithm
with proper handling of NaN, Inf, negative inputs, and division by zero.
Every operation delegates to the formally verified Idris 2 core via the
Zig FFI bridge (`libproven`). No floating-point logic is reimplemented
in Lean.
-/

import Proven.FFI

namespace Proven.SafeFloat

open Proven.FFI

/-- Error type for floating-point operations. -/
structure FloatError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString FloatError := ⟨fun e => s!"FloatError: {e.status}"⟩

/-- Alias for float operation results. -/
abbrev FloatResult' (a : Type) := Except FloatError a

-- Internal helper
private def unwrapFloatResult (r : FloatResult) : FloatResult' Float :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- Safe arithmetic
-- ============================================================================

/-- Safe floating-point division.
    Returns error on division by zero or NaN inputs.
    Delegates to `proven_float_div`. -/
def div (a b : Float) : IO (FloatResult' Float) := do
  let r <- provenFloatDiv a b
  return unwrapFloatResult r

/-- Safe square root.
    Returns error if x is negative or NaN.
    Delegates to `proven_float_sqrt`. -/
def sqrt (x : Float) : IO (FloatResult' Float) := do
  let r <- provenFloatSqrt x
  return unwrapFloatResult r

/-- Safe natural logarithm.
    Returns error if x <= 0 or NaN.
    Delegates to `proven_float_ln`. -/
def ln (x : Float) : IO (FloatResult' Float) := do
  let r <- provenFloatLn x
  return unwrapFloatResult r

-- ============================================================================
-- Classification
-- ============================================================================

/-- Check if a float is finite (not NaN or Inf).
    Delegates to `proven_float_is_finite`. -/
def isFinite (x : Float) : IO Bool :=
  provenFloatIsFinite x

/-- Check if a float is NaN.
    Delegates to `proven_float_is_nan`. -/
def isNan (x : Float) : IO Bool :=
  provenFloatIsNan x

-- ============================================================================
-- Convenience: Option-returning variants
-- ============================================================================

/-- Division returning `Option`. -/
def div? (a b : Float) : IO (Option Float) := do
  let r <- div a b
  return r.toOption

/-- Square root returning `Option`. -/
def sqrt? (x : Float) : IO (Option Float) := do
  let r <- sqrt x
  return r.toOption

/-- Natural logarithm returning `Option`. -/
def ln? (x : Float) : IO (Option Float) := do
  let r <- ln x
  return r.toOption

end Proven.SafeFloat
