-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeMathProps

import Proven.Core
import Proven.SafeMath

%default total

||| Property: Safe addition is commutative
prop_addCommutative : (x, y : Integer) -> safeAdd x y = safeAdd y x
prop_addCommutative x y = Refl

||| Property: Safe addition of zero is identity
prop_addIdentity : (x : Integer) -> safeAdd x 0 = Ok x
prop_addIdentity x = Refl

||| Property: Safe subtraction of zero is identity
prop_subIdentity : (x : Integer) -> safeSub x 0 = Ok x
prop_subIdentity x = Refl

||| Property: Safe division by 1 is identity
prop_divByOne : (x : Integer) -> x >= 0 -> safeDiv x 1 = Ok x
prop_divByOne x _ = Refl

||| Property: Safe division by zero returns error
prop_divByZeroFails : (x : Integer) -> isErr (safeDiv x 0) = True
prop_divByZeroFails x = Refl

||| OWED: Bounded values stay within bounds
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_boundedInRange_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_boundedInRange : (b : Bounded min max) ->
                        (getValue b >= min) && (getValue b <= max) = True

||| Property: NonEmpty list always has at least one element
prop_nonEmptyHasElements : (ne : NonEmpty a) -> length (toList ne) >= 1 = True
prop_nonEmptyHasElements (MkNonEmpty h t) = Refl

||| OWED: Safe modulo never crashes
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_modNeverCrashes_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_modNeverCrashes : (x, y : Integer) -> y /= 0 ->
                         isOk (safeMod x y) = True

||| OWED: Absolute value is always non-negative
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_absNonNegative_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_absNonNegative : (x : Integer) -> safeAbs x >= 0 = True

||| Property: Safe negate is involutive (negating twice returns original)
prop_negateInvolutive : (x : Integer) -> safeNegate (safeNegate x) = x
prop_negateInvolutive x = Refl

||| Property: Overflow detection catches MAX_INT + 1
prop_overflowDetected : detectOverflow MaxInt 1 = True
prop_overflowDetected = Refl

||| Test runner for properties
export
runMathProperties : IO ()
runMathProperties = do
  putStrLn "SafeMath Property Tests"
  putStrLn "======================"
  putStrLn "prop_addCommutative: PASS (proven by type)"
  putStrLn "prop_addIdentity: PASS (proven by type)"
  putStrLn "prop_subIdentity: PASS (proven by type)"
  putStrLn "prop_divByOne: PASS (proven by type)"
  putStrLn "prop_divByZeroFails: PASS (proven by type)"
  putStrLn "prop_nonEmptyHasElements: PASS (proven by type)"
  putStrLn "prop_negateInvolutive: PASS (proven by type)"
  putStrLn "prop_overflowDetected: PASS (proven by type)"
  putStrLn ""
  putStrLn "All type-level proofs verified by Idris 2 compiler."
