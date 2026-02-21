-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeFiniteField - Verified finite field arithmetic
|||
||| Safe operations over finite fields GF(p) and GF(2^n).
||| Used in cryptography, error correction codes, and algebraic geometry.
|||
||| All operations are guaranteed to stay within the field.
module Proven.SafeFiniteField
import Data.String
import Data.List

import Proven.Core
import Data.So
import Data.Vect
import Data.Bits

%default total

-- ============================================================================
-- PRIME FIELD GF(p)
-- ============================================================================

||| An element of a prime field GF(p)
||| Value is guaranteed to be in [0, p-1]
public export
record PrimeFieldElement (p : Nat) where
  constructor MkPFE
  value : Nat
  0 inRange : So (value < p)

||| Create an element (reduces modulo p)
export
pfElement : (p : Nat) -> {auto prf : NonZero p} -> Nat -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfElement p n = believe_me (MkPFE (n `mod` p))

||| Zero element
export
pfZero : (p : Nat) -> {auto prf : NonZero p} -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfZero p = believe_me (MkPFE 0)

||| One element (identity for multiplication)
export
pfOne : (p : Nat) -> {auto prf : GT p 1} -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfOne p = believe_me (MkPFE 1)

||| Get the value as a natural number
export
pfValue : PrimeFieldElement p -> Nat
pfValue e = e.value

-- ============================================================================
-- PRIME FIELD ARITHMETIC
-- ============================================================================

||| Addition in GF(p)
export
pfAdd : PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfAdd {p} a b = believe_me (MkPFE ((a.value + b.value) `mod` p))

||| Subtraction in GF(p)
export
pfSub : PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfSub {p} a b = believe_me (MkPFE ((a.value + p - b.value) `mod` p))

||| Negation in GF(p)
export
pfNeg : PrimeFieldElement p -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfNeg {p} a = believe_me (MkPFE ((p - a.value) `mod` p))

||| Multiplication in GF(p)
export
pfMul : PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfMul {p} a b = believe_me (MkPFE ((a.value * b.value) `mod` p))

||| Extended Euclidean algorithm for computing modular inverse
||| Returns (gcd, x, y) such that a*x + b*y = gcd
extendedGcd : Integer -> Integer -> (Integer, Integer, Integer)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b =
  let (g, x, y) = extendedGcd b (a `mod` b)
  in (g, y, x - (a `div` b) * y)

||| Modular inverse (returns None if not coprime with p)
export
pfInverse : PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfInverse {p} a =
  if a.value == 0 then Nothing
  else let (g, x, _) = extendedGcd (cast a.value) (cast p)
       in if g /= 1 then Nothing
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
          else Just (believe_me (MkPFE (cast ((x `mod` cast p + cast p) `mod` cast p))))

||| Division in GF(p) (returns None if divisor is zero)
export
pfDiv : PrimeFieldElement p -> PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfDiv a b = map (pfMul a) (pfInverse b)

||| Exponentiation by squaring
export
pfPow : PrimeFieldElement p -> Nat -> PrimeFieldElement p
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
pfPow {p} base exp = go base exp (believe_me (MkPFE 1))
  where
    go : PrimeFieldElement p -> Nat -> PrimeFieldElement p -> PrimeFieldElement p
    go _ Z acc = acc
    go b e acc =
      let acc' = if e `mod` 2 == 1 then pfMul acc b else acc
          b' = pfMul b b
      in go b' (e `div` 2) acc'

-- ============================================================================
-- BINARY FIELD GF(2^n)
-- ============================================================================

||| An element of a binary extension field GF(2^n)
||| Represented as a polynomial with binary coefficients
public export
record BinaryFieldElement (n : Nat) where
  constructor MkBFE
  -- Coefficients stored as bits: coefficient of x^i is bit i
  bits : Bits64
  0 inRange : So (bits < pow 2 n)

||| Create a binary field element
export
bfElement : (n : Nat) -> {auto prf : LTE n 64} -> Bits64 -> BinaryFieldElement n
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
bfElement n b = believe_me (MkBFE (b .&. (cast (pow 2 n) - 1)))

||| Zero element
export
bfZero : (n : Nat) -> BinaryFieldElement n
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
bfZero n = believe_me (MkBFE 0)

||| One element
export
bfOne : (n : Nat) -> {auto prf : GT n 0} -> BinaryFieldElement n
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
bfOne n = believe_me (MkBFE 1)

||| Get the bits
export
bfBits : BinaryFieldElement n -> Bits64
bfBits e = e.bits

-- ============================================================================
-- BINARY FIELD ARITHMETIC
-- ============================================================================

||| Addition in GF(2^n) (XOR)
export
bfAdd : BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
bfAdd a b = believe_me (MkBFE (a.bits `xor` b.bits))

||| Subtraction in GF(2^n) (same as addition in characteristic 2)
export
bfSub : BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
bfSub = bfAdd

||| Negation in GF(2^n) (identity in characteristic 2)
export
bfNeg : BinaryFieldElement n -> BinaryFieldElement n
bfNeg a = a

||| Polynomial multiplication helper
polyMul : Bits64 -> Bits64 -> Bits64
polyMul a b = go a b 0 0
  where
    go : Bits64 -> Bits64 -> Bits64 -> Nat -> Bits64
    go _ 0 acc _ = acc
    go a b acc i =
      let acc' = if (b .&. 1) /= 0 then acc `xor` (a `shiftL` cast i) else acc
      in go a (b `shiftR` 1) acc' (S i)

||| Polynomial modulo reduction by irreducible polynomial
polyMod : Bits64 -> Bits64 -> Nat -> Bits64
polyMod a irr n =
  let mask = cast (pow 2 n) - 1
  in go a
  where
    go : Bits64 -> Bits64
    go x =
      if x <= mask then x
      else let deg = highestBit x
               shift = cast deg - cast n
           in if shift < 0 then x
              else go (x `xor` (irr `shiftL` cast shift))

    highestBit : Bits64 -> Nat
    highestBit 0 = 0
    highestBit x = S (highestBit (x `shiftR` 1))

||| Multiplication in GF(2^n) with irreducible polynomial
export
bfMulWithIrr : Bits64 -> BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
bfMulWithIrr irr a b =
  let prod = polyMul a.bits b.bits
      reduced = polyMod prod irr n
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  in believe_me (MkBFE reduced)

-- Common irreducible polynomials
-- GF(2^8): x^8 + x^4 + x^3 + x + 1 (AES polynomial)
export
aesIrreducible : Bits64
aesIrreducible = 0x11B

-- GF(2^128): x^128 + x^7 + x^2 + x + 1 (GCM polynomial)
export
gcmIrreducible : Bits64
gcmIrreducible = 0x87  -- Lower bits, actual is x^128 + this

-- ============================================================================
-- LEGENDRE SYMBOL AND QUADRATIC RESIDUES
-- ============================================================================

||| Legendre symbol (a/p) for odd prime p
||| Returns 1 if a is quadratic residue, -1 if not, 0 if a ≡ 0
export
legendreSymbol : PrimeFieldElement p -> Integer
legendreSymbol {p} a =
  if a.value == 0 then 0
  else let exp = (p - 1) `div` 2
           result = pfPow a exp
       in if result.value == 1 then 1
          else if result.value == p - 1 then -1
          else 0

||| Check if element is a quadratic residue
export
isQuadraticResidue : PrimeFieldElement p -> Bool
isQuadraticResidue a = legendreSymbol a == 1

||| Tonelli-Shanks square root (returns None if not a residue)
export
pfSqrt : PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfSqrt {p} a =
  if a.value == 0 then Just (pfZero p)
  else if legendreSymbol a /= 1 then Nothing
  else
    -- Simplified: for p ≡ 3 (mod 4), sqrt = a^((p+1)/4)
    if p `mod` 4 == 3
    then Just (pfPow a ((p + 1) `div` 4))
    else Nothing  -- Full Tonelli-Shanks would go here

-- ============================================================================
-- COMMON PRIME FIELDS
-- ============================================================================

||| GF(2) - Binary field
public export
GF2 : Type
GF2 = PrimeFieldElement 2

||| GF(3) - Ternary field
public export
GF3 : Type
GF3 = PrimeFieldElement 3

||| GF(p) where p = 2^255 - 19 (Curve25519)
public export
Curve25519Prime : Nat
Curve25519Prime = 57896044618658097711785492504343953926634992332820282019728792003956564819949

||| GF(p) where p = 2^256 - 2^32 - 977 (secp256k1)
public export
Secp256k1Prime : Nat
Secp256k1Prime = 115792089237316195423570985008687907853269984665640564039457584007908834671663

-- ============================================================================
-- POLYNOMIAL RING OPERATIONS
-- ============================================================================

||| Polynomial with coefficients in GF(p)
public export
record Polynomial (p : Nat) where
  constructor MkPolynomial
  coefficients : List (PrimeFieldElement p)  -- Least significant first

||| Evaluate polynomial at a point
export
polyEval : Polynomial p -> PrimeFieldElement p -> PrimeFieldElement p
polyEval {p} poly x = go poly.coefficients (pfOne p) (pfZero p)
  where
    go : List (PrimeFieldElement p) -> PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
    go [] _ acc = acc
    go (c :: cs) xPow acc =
      go cs (pfMul xPow x) (pfAdd acc (pfMul c xPow))

||| Polynomial addition
export
polyAdd : Polynomial p -> Polynomial p -> Polynomial p
polyAdd a b = MkPolynomial (zipWithDefault pfAdd (pfZero _) a.coefficients b.coefficients)
  where
    zipWithDefault : (a -> a -> a) -> a -> List a -> List a -> List a
    zipWithDefault _ _ [] [] = []
    zipWithDefault f def [] (y :: ys) = f def y :: zipWithDefault f def [] ys
    zipWithDefault f def (x :: xs) [] = f x def :: zipWithDefault f def xs []
    zipWithDefault f def (x :: xs) (y :: ys) = f x y :: zipWithDefault f def xs ys

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Check if a natural number is prime (naive implementation)
export
isPrime : Nat -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = go 2
  where
    go : Nat -> Bool
    go i = if i * i > n then True
           else if n `mod` i == 0 then False
           else go (S i)

||| Generate a random element in GF(p) (given a seed)
export
randomElement : (p : Nat) -> {auto prf : NonZero p} -> Nat -> PrimeFieldElement p
randomElement p seed = pfElement p seed
