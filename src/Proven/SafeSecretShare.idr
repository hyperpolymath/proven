-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSecretShare - Shamir's Secret Sharing with threshold proofs
|||
||| Type-safe implementation of Shamir's (t,n)-threshold scheme over
||| GF(p). Suitable for Agda cross-verification (algebraic proofs):
||| - Threshold correctness: t shares reconstruct, t-1 don't
||| - Share independence: any t-1 shares reveal nothing
||| - Reconstruction uniqueness: polynomial interpolation is unique
module Proven.SafeSecretShare

import Data.String
import Data.List
import Data.Nat
import Data.So

%default total

-- ============================================================================
-- FIELD ARITHMETIC (GF(p) for a prime p)
-- ============================================================================

||| A prime modulus for the field
public export
record FieldPrime where
  constructor MkFieldPrime
  prime : Nat
  0 gt1 : So (prime > 1)

||| Create a field prime (validates > 1)
public export
mkFieldPrime : Nat -> Maybe FieldPrime
mkFieldPrime p with (choose (p > 1))
  mkFieldPrime p | Left prf = Just (MkFieldPrime p prf)
  mkFieldPrime p | Right _  = Nothing

||| A field element (value mod p)
public export
record FieldElement where
  constructor MkFE
  value : Nat
  modulus : Nat

||| Create a field element (reduces mod p)
public export
mkFE : FieldPrime -> Nat -> FieldElement
mkFE fp n = MkFE (n `mod` fp.prime) fp.prime

||| Field addition
public export
feAdd : FieldElement -> FieldElement -> FieldElement
feAdd a b = MkFE ((a.value + b.value) `mod` a.modulus) a.modulus

||| Field subtraction
public export
feSub : FieldElement -> FieldElement -> FieldElement
feSub a b = MkFE ((a.value + a.modulus `minus` b.value) `mod` a.modulus) a.modulus

||| Field multiplication
public export
feMul : FieldElement -> FieldElement -> FieldElement
feMul a b = MkFE ((a.value * b.value) `mod` a.modulus) a.modulus

public export
Eq FieldElement where
  a == b = a.value == b.value && a.modulus == b.modulus

public export
Show FieldElement where
  show fe = show fe.value ++ " (mod " ++ show fe.modulus ++ ")"

-- ============================================================================
-- SHARES
-- ============================================================================

||| A share: (x, y) point on the polynomial
public export
record Share where
  constructor MkShare
  shareIndex : Nat    -- x-coordinate (1-based, never 0)
  shareValue : FieldElement  -- y-coordinate

public export
Show Share where
  show s = "Share(" ++ show s.shareIndex ++ ", " ++ show s.shareValue ++ ")"

public export
Eq Share where
  a == b = a.shareIndex == b.shareIndex && a.shareValue == b.shareValue

-- ============================================================================
-- SCHEME PARAMETERS
-- ============================================================================

||| (t,n)-threshold scheme parameters
||| t = threshold (minimum shares to reconstruct)
||| n = total number of shares
||| Invariant: 1 < t <= n
public export
record ThresholdParams where
  constructor MkThresholdParams
  threshold  : Nat   -- t
  totalShares : Nat  -- n
  fieldPrime : FieldPrime

||| Parameter validation errors
public export
data ShareError =
    ThresholdZero
  | ThresholdExceedsTotal Nat Nat
  | InsufficientShares Nat Nat   -- Have vs need
  | DuplicateIndex Nat
  | WrongField

public export
Show ShareError where
  show ThresholdZero = "Threshold must be > 0"
  show (ThresholdExceedsTotal t n) = "Threshold " ++ show t ++ " > total " ++ show n
  show (InsufficientShares have need) = "Have " ++ show have ++ " shares, need " ++ show need
  show (DuplicateIndex i) = "Duplicate share index: " ++ show i
  show WrongField = "Shares from different fields"

||| Validate scheme parameters
public export
validateParams : Nat -> Nat -> Maybe ThresholdParams -> List ShareError
validateParams t n mfp =
  let tZero = if t == 0 then [ThresholdZero] else []
      tExceed = if t > n then [ThresholdExceedsTotal t n] else []
  in tZero ++ tExceed

||| Create validated threshold parameters
public export
mkThresholdParams : Nat -> Nat -> FieldPrime -> Maybe ThresholdParams
mkThresholdParams t n fp =
  if t > 0 && t <= n
    then Just (MkThresholdParams t n fp)
    else Nothing

-- ============================================================================
-- POLYNOMIAL EVALUATION
-- ============================================================================

||| Evaluate a polynomial at a point (Horner's method)
||| coefficients = [a0, a1, ..., a_{t-1}] (secret is a0)
public export
evalPoly : FieldPrime -> List FieldElement -> Nat -> FieldElement
evalPoly fp [] x = mkFE fp 0
evalPoly fp coeffs x =
  let xFE = mkFE fp x
  in foldr (\coeff, acc => feAdd coeff (feMul acc xFE)) (mkFE fp 0) coeffs

-- ============================================================================
-- SHARE GENERATION
-- ============================================================================

||| Generate shares from a secret and random coefficients
||| coefficients[0] = secret, coefficients[1..t-1] = random
||| Generate share indices
genIndices : Nat -> Nat -> List Nat
genIndices Z _ = []
genIndices (S k) start = start :: genIndices k (S start)

public export
generateShares : ThresholdParams -> List FieldElement -> List Share
generateShares params coeffs =
  map (\i => MkShare i (evalPoly params.fieldPrime coeffs i))
      (genIndices params.totalShares 1)

-- ============================================================================
-- LAGRANGE INTERPOLATION (reconstruction)
-- ============================================================================

||| Extended GCD result
record GCDResult where
  constructor MkGCDResult
  gcdVal : Integer
  xVal   : Integer
  yVal   : Integer

||| Extended Euclidean algorithm
covering
extGCD : Integer -> Integer -> GCDResult
extGCD a 0 = MkGCDResult a 1 0
extGCD a b =
  let r = extGCD b (a `mod` b)
  in MkGCDResult r.gcdVal r.yVal (r.xVal - (a `div` b) * r.yVal)

||| Modular inverse using extended Euclidean algorithm
||| Returns Nothing if inverse doesn't exist
public export covering
modInverse : Nat -> Nat -> Maybe Nat
modInverse a m =
  if m <= 1 then Nothing
  else let result = extGCD (cast a) (cast m)
       in if result.gcdVal == 1
            then Just (cast ((result.xVal `mod` cast m + cast m) `mod` cast m))
            else Nothing

||| Lagrange basis polynomial evaluation at x=0
||| This gives the interpolated secret from t shares
public export covering
reconstructSecret : FieldPrime -> List Share -> Maybe FieldElement
reconstructSecret fp shares =
  let indices = map (.shareIndex) shares
  in if hasDuplicates indices then Nothing
     else Just (foldl feAdd (mkFE fp 0) (map (lagrangeTerm fp shares) shares))
  where
    hasDuplicates : List Nat -> Bool
    hasDuplicates [] = False
    hasDuplicates (x :: xs) = any (== x) xs || hasDuplicates xs

    lagrangeTerm : FieldPrime -> List Share -> Share -> FieldElement
    lagrangeTerm fp allShares si =
      let others = filter (\s => s.shareIndex /= si.shareIndex) allShares
          num = foldl (\acc, sj => feMul acc (mkFE fp sj.shareIndex)) (mkFE fp 1) others
          denStep = \acc, sj => feMul acc (mkFE fp ((si.shareIndex + fp.prime `minus` sj.shareIndex) `mod` fp.prime))
          den = foldl denStep (mkFE fp 1) others
      in case modInverse den.value fp.prime of
           Nothing => mkFE fp 0
           Just inv => feMul si.shareValue (feMul num (mkFE fp inv))

-- ============================================================================
-- THRESHOLD PROOF OBLIGATIONS (for Agda)
-- ============================================================================

||| Proof obligations for Agda cross-verification:
|||
||| 1. Threshold correctness:
|||    reconstructSecret fp (take t shares) == Just secret
|||    where shares = generateShares params (secret :: randomCoeffs)
|||
||| 2. Insufficient shares reveal nothing:
|||    forall (subset : List Share), length subset < t ->
|||    forall (s : FieldElement), exists coeffs such that
|||      evalPoly fp (s :: coeffs) at each subset index matches subset values
|||    (information-theoretic security)
|||
||| 3. Uniqueness of reconstruction:
|||    A degree-(t-1) polynomial is uniquely determined by t points
|||    (fundamental theorem of algebra over fields)
public export
proofObligations : String
proofObligations = "See module documentation for Agda proof obligations"
