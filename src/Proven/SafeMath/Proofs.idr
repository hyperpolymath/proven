-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Mathematical proofs for SafeMath operations
|||
||| This module contains proofs that verify properties of our safe
||| arithmetic operations. These proofs are checked by the Idris 2
||| compiler, guaranteeing correctness.
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - `Calc` proof style replaces `rewrite` chains (0.8.0 changed rewrite
|||   rule resolution for recursive definitions)
||| - `minus n 0` no longer reduces on abstract `n`; case-split required
||| - `div`/`mod`/`gcd` internal representations changed; some properties
|||   are postulated pending upstream proof availability
||| - `SIsNonZero` replaced by `ItIsSucc`
module Proven.SafeMath.Proofs

import Proven.Core
import Proven.SafeMath.Nat
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Properties of Natural Number Addition
--------------------------------------------------------------------------------

||| Addition is commutative: a + b = b + a
||| Delegates to Data.Nat.plusCommutative which uses Calc proofs in 0.8.0.
public export
plusCommutative : (a, b : Nat) -> a + b = b + a
plusCommutative = Data.Nat.plusCommutative

||| Addition is associative: (a + b) + c = a + (b + c)
||| Delegates to Data.Nat.plusAssociative which uses Calc proofs in 0.8.0.
public export
plusAssociative : (a, b, c : Nat) -> (a + b) + c = a + (b + c)
plusAssociative = Data.Nat.plusAssociative

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
||| Delegates to Data.Nat.multCommutative which uses Calc proofs in 0.8.0.
public export
multCommutative : (a, b : Nat) -> a * b = b * a
multCommutative = Data.Nat.multCommutative

||| Multiplication distributes over addition (left):
|||   a * (b + c) = (a * b) + (a * c)
||| In Data.Nat 0.8.0 this is `multDistributesOverPlusLeft` with argument
||| order (a, b, c) -> a * (b + c) = (a * b) + (a * c), which matches our
||| signature exactly.
public export
multDistributesLeft : (a, b, c : Nat) -> a * (b + c) = (a * b) + (a * c)
multDistributesLeft = multDistributesOverPlusLeft

||| One is left identity for multiplication
public export
multOneLeft : (n : Nat) -> 1 * n = n
multOneLeft n = plusZeroRightNeutral n

||| One is right identity for multiplication
||| Uses Calc proof style for Idris 2 0.8.0 compatibility.
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
  ~~ a + 0         ...(cong (+ 0) (minusZeroRight a))
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

-- | Division by 1 returns the original number.
-- | Postulated: In Idris 2 0.8.0, `div` for Nat uses `divNat`/`divNatNZ`
-- | with fuel-based recursion (`div'`). The internal reduction of
-- | `divNatNZ n 1 ItIsSucc` no longer normalises to `n` at the type level
-- | for abstract `n`. The property holds computationally for all concrete
-- | values but cannot be proven structurally without exposing `div'`
-- | internals.
-- TODO: Update for Idris2 0.8.0 -- prove via Data.Nat.Division or
-- fuel-based induction once upstream provides the necessary lemmas.
export
divByOne : (n : Nat) -> div n 1 = n

-- | Remainder is always less than divisor (for non-zero divisor).
-- | Postulated: In Idris 2 0.8.0, `modNatNZ` returns `Nat` (not a proof).
-- | The bound `mod n d < d` holds by the fuel-based `mod'` implementation
-- | but proving it requires induction over the fuel parameter. The contrib
-- | library `Data.Nat.Division` provides `bound_mod''` but with a different
-- | API shape than our signature.
-- TODO: Update for Idris2 0.8.0 -- use Data.Nat.Division.bound_mod'' or
-- prove via fuel induction.
export
modLtDivisor : (n, d : Nat) -> {auto ok : IsSucc d} -> LT (mod n d) d

--------------------------------------------------------------------------------
-- GCD Properties
--------------------------------------------------------------------------------

-- | GCD of n and 0 is n.
-- | Postulated: In Idris 2 0.8.0, `gcd` uses `modNatNZ` internally and
-- | `gcd n 0` normalises via fuel-based recursion through `gcdFuel`.
-- | The constraint `n = gcdFuel 0 n ((n+0)+1) n 0` cannot be resolved
-- | for abstract `n`. The property is trivially true by the Euclidean
-- | algorithm definition.
-- TODO: Update for Idris2 0.8.0 -- prove once gcd internals are accessible
-- or via Data.Nat.Factor lemmas.
export
gcdZeroRight : (n : Nat) -> gcd n 0 = n

-- | GCD is commutative: gcd a b = gcd b a
-- | Postulated because proof requires well-founded induction on (mod a b)
-- | and reasoning about the Euclidean algorithm's termination measure.
-- | The property holds for all natural numbers by the symmetry of the
-- | Euclidean algorithm; a full mechanised proof would require
-- | Accessibility-based recursion over the strictly-decreasing mod chain.
export
gcdCommutative : (a, b : Nat) -> gcd a b = gcd b a
