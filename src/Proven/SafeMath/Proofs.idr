-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Mathematical proofs for SafeMath operations
|||
||| This module contains proofs that verify properties of our safe
||| arithmetic operations. These proofs are checked by the Idris 2
||| compiler, guaranteeing correctness.
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - Delegated to Data.Nat stdlib proofs where possible (stdlib now uses
|||   `Calc` proof style internally)
||| - `minus n 0` no longer reduces on abstract `n`; case-split required
||| - `div`/`mod`/`gcd` internal representations changed; some properties
|||   are postulated pending upstream proof availability
||| - `SIsNonZero` replaced by `ItIsSucc`
module Proven.SafeMath.Proofs

import Proven.Core
import Proven.SafeMath.Nat
import Data.Nat
import Data.Nat.Division
import Syntax.PreorderReasoning

%default total

--------------------------------------------------------------------------------
-- Properties of Natural Number Addition
--------------------------------------------------------------------------------

||| Addition is commutative: a + b = b + a
||| Delegates to Data.Nat.plusCommutative.
public export
plusCommutative : (a, b : Nat) -> a + b = b + a
plusCommutative = Data.Nat.plusCommutative

||| Addition is associative: (a + b) + c = a + (b + c)
||| Note: Data.Nat.plusAssociative proves a + (b + c) = (a + b) + c,
||| so we use `sym` to get our desired direction.
public export
plusAssociative : (a, b, c : Nat) -> (a + b) + c = a + (b + c)
plusAssociative a b c = sym (Data.Nat.plusAssociative a b c)

||| Zero is right identity for addition
public export
plusZeroRight : (n : Nat) -> n + 0 = n
plusZeroRight = plusZeroRightNeutral

||| Zero is left identity for addition
public export
plusZeroLeft : (n : Nat) -> 0 + n = n
plusZeroLeft n = Refl

--------------------------------------------------------------------------------
-- Properties of Natural Number Multiplication
--------------------------------------------------------------------------------

||| Multiplication is commutative: a * b = b * a
||| Delegates to Data.Nat.multCommutative.
public export
multCommutative : (a, b : Nat) -> a * b = b * a
multCommutative = Data.Nat.multCommutative

||| Multiplication distributes over addition (left):
|||   a * (b + c) = (a * b) + (a * c)
||| Delegates to Data.Nat.multDistributesOverPlusRight which proves
||| exactly this signature.
public export
multDistributesLeft : (a, b, c : Nat) -> a * (b + c) = (a * b) + (a * c)
multDistributesLeft = multDistributesOverPlusRight

||| One is left identity for multiplication
public export
multOneLeft : (n : Nat) -> 1 * n = n
multOneLeft n = plusZeroRightNeutral n

||| One is right identity for multiplication
||| Uses cong for Idris 2 0.8.0 compatibility.
public export
multOneRight : (n : Nat) -> n * 1 = n
multOneRight Z = Refl
multOneRight (S n) = cong S (multOneRight n)

||| Zero annihilates multiplication (right)
public export
multZeroRight : (n : Nat) -> n * 0 = 0
multZeroRight = multZeroRightZero

--------------------------------------------------------------------------------
-- Properties of Subtraction
--------------------------------------------------------------------------------

||| Subtracting zero gives the original number.
||| In Idris 2 0.8.0, `minus n 0` does not reduce when `n` is abstract
||| because `minus` pattern-matches on both arguments. Case-split required.
public export
minusZeroRight : (n : Nat) -> minus n 0 = n
minusZeroRight Z     = Refl
minusZeroRight (S _) = Refl

||| Subtracting a number from itself gives zero
public export
minusSelf : (n : Nat) -> minus n n = 0
minusSelf Z = Refl
minusSelf (S n) = minusSelf n

||| If a >= b, then (a - b) + b = a.
||| Uses Calc proof style for Idris 2 0.8.0 compatibility.
public export
minusPlusCancel : (a, b : Nat) -> LTE b a -> (minus a b) + b = a
minusPlusCancel a Z _ = Calc $
  |~ minus a 0 + 0
  ~~ a + 0         ...(cong (+ 0) (Proven.SafeMath.Proofs.minusZeroRight a))
  ~~ a             ...(plusZeroRightNeutral a)
minusPlusCancel (S a) (S b) (LTESucc prf) = Calc $
  |~ minus (S a) (S b) + (S b)
  ~~ minus a b + (S b)                ...(Refl)
  ~~ S (minus a b + b)                ...(sym $ plusSuccRightSucc (minus a b) b)
  ~~ S a                              ...(cong S (minusPlusCancel a b prf))

--------------------------------------------------------------------------------
-- Properties of Comparison
--------------------------------------------------------------------------------

||| LTE is reflexive: n <= n
public export
lteRefl : (n : Nat) -> LTE n n
lteRefl Z = LTEZero
lteRefl (S n) = LTESucc (lteRefl n)

||| LTE is transitive: a <= b and b <= c implies a <= c
public export
lteTrans : LTE a b -> LTE b c -> LTE a c
lteTrans LTEZero _ = LTEZero
lteTrans (LTESucc ab) (LTESucc bc) = LTESucc (lteTrans ab bc)

||| LTE is antisymmetric: a <= b and b <= a implies a = b
public export
lteAntisym : LTE a b -> LTE b a -> a = b
lteAntisym LTEZero LTEZero = Refl
lteAntisym (LTESucc ab) (LTESucc ba) = cong S (lteAntisym ab ba)

--------------------------------------------------------------------------------
-- Division Properties
--------------------------------------------------------------------------------

||| Division by 1 returns the original number.
||| Proved via Data.Nat.Division.DivisionTheoremUniqueness:
||| n = n * 1 + 0 is the unique decomposition, so divNatNZ n 1 = n.
public export
divByOne : (n : Nat) -> divNatNZ n 1 ItIsSucc = n
divByOne n = fst $ DivisionTheoremUniqueness n 1 ItIsSucc n 0 (LTESucc LTEZero) (nTimesOnePlusZero n)
  where
    nTimesOnePlusZero : (k : Nat) -> k = k * 1 + 0
    nTimesOnePlusZero k = sym $ Calc $
      |~ k * 1 + 0
      ~~ k * 1     ...(plusZeroRightNeutral (k * 1))
      ~~ k         ...(multOneRightNeutral k)

||| Remainder is always less than divisor (for non-zero divisor).
||| Delegates to Data.Nat.Division.boundModNatNZ which proves this via
||| fuel-based induction over mod''.
public export
modLtDivisor : (n, d : Nat) -> {auto 0 ok : NonZero d} -> LT (modNatNZ n d ok) d
modLtDivisor n (S d) = boundModNatNZ n (S d) ItIsSucc

--------------------------------------------------------------------------------
-- GCD Properties
--------------------------------------------------------------------------------

-- | GCD of n and 0 is n.
-- | POSTULATE: Data.Nat.gcd is `covering` (not total), so Idris 2 refuses to
-- | reduce `gcd (S n) 0` during type-checking even though `gcd a Z = a` is a
-- | direct clause. Verified 2026-03-29: Refl fails with "Can't solve constraint
-- | between: S _ and gcd (S _) 0".
-- | Resolution: rewrite signature to use Data.Nat.Factor.gcdUnproven (total),
-- | or wait for upstream to make gcd total.
export
gcdZeroRight : (n : Nat) -> {auto 0 ok : NotBothZero n 0} -> gcd n 0 @{ok} = n

-- | GCD is commutative: gcd a b = gcd b a
-- | POSTULATE: The base cases (gcd 0 (S b) = gcd (S b) 0) are trivially Refl,
-- | but the recursive case gcd (S a) (S b) requires well-founded induction
-- | on (a + b) with properties of modNatNZ. Since Data.Nat.gcd is `covering`
-- | (not total), structural induction is unavailable. The evidence-carrying
-- | GCD type in Data.Nat.Factor has `Symmetric Nat (GCD p)` but that proves
-- | symmetry of the *evidence*, not equality of the computed Nat values.
-- | Resolution path: prove via gcdUnproven (which IS total) then show
-- | gcdUnproven agrees with gcd on all inputs.
export
gcdCommutative : (a, b : Nat) -> {auto 0 ok1 : NotBothZero a b} -> {auto 0 ok2 : NotBothZero b a} -> gcd a b @{ok1} = gcd b a @{ok2}
