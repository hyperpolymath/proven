/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeMath - Safe arithmetic operations

Provides checked integer arithmetic that cannot crash. Every operation
delegates to the formally verified Idris 2 core via the Zig FFI bridge
(`libproven`). No arithmetic logic is reimplemented in Lean.

## Error handling

Operations return `Option Int64`: `some v` on success, `none` on overflow,
underflow, or division by zero. Use `Except` variants for richer error
reporting.

## Example

```lean
open Proven.SafeMath in
do
  let sum <- add 1000000000000 2000000000000
  IO.println s!"Sum = {sum}"
```
-/

import Proven.FFI

namespace Proven.SafeMath

open Proven.FFI

/-- Error type for arithmetic operations, carrying the libproven status code. -/
structure MathError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString MathError := ⟨fun e => s!"MathError: {e.status}"⟩

/-- Alias for results that may fail with a `MathError`. -/
abbrev MathResult (a : Type) := Except MathError a

-- Internal helper: unwrap an IntResult into an Except
private def unwrapIntResult (r : IntResult) : MathResult Int64 :=
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => .ok r.value
  | _   => .error { status := s }

-- ============================================================================
-- Checked arithmetic
-- ============================================================================

/-- Safe addition with overflow detection.
    Delegates to `proven_math_add_checked`. -/
def add (a b : Int64) : IO (MathResult Int64) := do
  let r <- provenMathAddChecked a b
  return unwrapIntResult r

/-- Safe subtraction with underflow detection.
    Delegates to `proven_math_sub_checked`. -/
def sub (a b : Int64) : IO (MathResult Int64) := do
  let r <- provenMathSubChecked a b
  return unwrapIntResult r

/-- Safe multiplication with overflow detection.
    Delegates to `proven_math_mul_checked`. -/
def mul (a b : Int64) : IO (MathResult Int64) := do
  let r <- provenMathMulChecked a b
  return unwrapIntResult r

/-- Safe integer division.
    Returns error on division by zero or `Int64.min / -1` overflow.
    Delegates to `proven_math_div`. -/
def div (numerator denominator : Int64) : IO (MathResult Int64) := do
  let r <- provenMathDiv numerator denominator
  return unwrapIntResult r

/-- Safe modulo operation.
    Returns error on division by zero.
    Delegates to `proven_math_mod`. -/
def mod (numerator denominator : Int64) : IO (MathResult Int64) := do
  let r <- provenMathMod numerator denominator
  return unwrapIntResult r

/-- Safe absolute value.
    Returns error for `Int64.min` (cannot be represented as positive).
    Delegates to `proven_math_abs_safe`. -/
def abs (n : Int64) : IO (MathResult Int64) := do
  let r <- provenMathAbsSafe n
  return unwrapIntResult r

/-- Clamp value to `[lo, hi]` range.
    This operation is total and cannot fail.
    Delegates to `proven_math_clamp`. -/
def clamp (lo hi value : Int64) : IO Int64 :=
  provenMathClamp lo hi value

/-- Checked exponentiation with overflow detection.
    Delegates to `proven_math_pow_checked`. -/
def pow (base : Int64) (exp : UInt32) : IO (MathResult Int64) := do
  let r <- provenMathPowChecked base exp
  return unwrapIntResult r

-- ============================================================================
-- Convenience: Option-returning variants
-- ============================================================================

/-- Addition returning `Option`. -/
def add? (a b : Int64) : IO (Option Int64) := do
  let r <- add a b
  return r.toOption

/-- Subtraction returning `Option`. -/
def sub? (a b : Int64) : IO (Option Int64) := do
  let r <- sub a b
  return r.toOption

/-- Multiplication returning `Option`. -/
def mul? (a b : Int64) : IO (Option Int64) := do
  let r <- mul a b
  return r.toOption

/-- Division returning `Option`. -/
def div? (a b : Int64) : IO (Option Int64) := do
  let r <- div a b
  return r.toOption

/-- Modulo returning `Option`. -/
def mod? (a b : Int64) : IO (Option Int64) := do
  let r <- mod a b
  return r.toOption

/-- Absolute value returning `Option`. -/
def abs? (n : Int64) : IO (Option Int64) := do
  let r <- abs n
  return r.toOption

/-- Exponentiation returning `Option`. -/
def pow? (base : Int64) (exp : UInt32) : IO (Option Int64) := do
  let r <- pow base exp
  return r.toOption

end Proven.SafeMath
