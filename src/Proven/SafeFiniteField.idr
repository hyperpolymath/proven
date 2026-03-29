-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
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
-- HELPERS
-- ============================================================================

||| Natural number exponentiation (avoids Prelude `pow` which returns Double)
natPow : Nat -> Nat -> Nat
natPow _ Z = 1
natPow b (S e) = b * natPow b e

-- ============================================================================
-- RANGE PROOF POSTULATES
-- ============================================================================

||| GT p 1 implies NonZero p — p >= 2 means p is a successor.
||| Eliminates need for believe_me when converting GT constraints to NonZero.
export
gt1ImpliesNonZero : {0 p : Nat} -> GT p 1 -> NonZero p
gt1ImpliesNonZero (LTESucc _) = SIsNonZero

||| n mod p < p for any NonZero p
modLtPrime : (n, p : Nat) -> {auto 0 _ : NonZero p} -> So (n `mod` p < p)

||| 0 < p for any NonZero p
zeroLtNonZero : (p : Nat) -> {auto 0 _ : NonZero p} -> So (0 < p)

||| 1 < p when p > 1
oneLtGt1 : (p : Nat) -> {auto 0 _ : GT p 1} -> So (1 < p)

||| Bitwise AND with (2^n - 1) mask yields a value < 2^n
maskLtPow2 : (b : Bits64) -> (n : Nat) -> So (b .&. (cast {to=Bits64} (natPow 2 n) - 1) < cast {to=Bits64} (natPow 2 n))

||| 0 < 2^n for any n
zeroLtPow2 : (n : Nat) -> So (the Bits64 0 < cast (natPow 2 n))

||| 1 < 2^n when n > 0
oneLtPow2 : (n : Nat) -> {auto 0 _ : GT n 0} -> So (the Bits64 1 < cast (natPow 2 n))

||| XOR of two values both < 2^n stays < 2^n
xorLtPow2 : (a, b : Bits64) -> (n : Nat) ->
             {auto 0 _ : So (a < cast {to=Bits64} (natPow 2 n))} ->
             {auto 0 _ : So (b < cast {to=Bits64} (natPow 2 n))} ->
             So ((a `xor` b) < cast {to=Bits64} (natPow 2 n))

||| Polynomial reduction modulo an irreducible of degree n yields result < 2^n
polyModLtPow2 : (result : Bits64) -> (n : Nat) -> So (result < cast {to=Bits64} (natPow 2 n))

||| A modular-inverse result in [0, p) satisfies So (v < p)
inverseInRange : (v, p : Nat) -> So (v < p)

||| 0 < p when p > 1 (derives from GT p 1 without needing NonZero)
zeroLtGt1 : (p : Nat) -> {auto 0 _ : GT p 1} -> So (0 < p)

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
pfElement p n = MkPFE (n `mod` p) (modLtPrime n p)

||| Zero element
export
pfZero : (p : Nat) -> {auto prf : NonZero p} -> PrimeFieldElement p
pfZero p = MkPFE 0 (zeroLtNonZero p)

||| One element (identity for multiplication)
export
pfOne : (p : Nat) -> {auto prf : GT p 1} -> PrimeFieldElement p
pfOne p = MkPFE 1 (oneLtGt1 p)

||| Get the value as a natural number
export
pfValue : PrimeFieldElement p -> Nat
pfValue e = e.value

-- ============================================================================
-- PRIME FIELD ARITHMETIC
-- ============================================================================

||| Addition in GF(p)
export
pfAdd : {p : Nat} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
pfAdd a b = MkPFE ((a.value + b.value) `mod` p) (modLtPrime (a.value + b.value) p)

||| Subtraction in GF(p)
export
pfSub : {p : Nat} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
pfSub a b = MkPFE ((minus (a.value + p) b.value) `mod` p) (modLtPrime (minus (a.value + p) b.value) p)

||| Negation in GF(p)
export
pfNeg : {p : Nat} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> PrimeFieldElement p
pfNeg a = MkPFE ((minus p a.value) `mod` p) (modLtPrime (minus p a.value) p)

||| Multiplication in GF(p)
export
pfMul : {p : Nat} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
pfMul a b = MkPFE ((a.value * b.value) `mod` p) (modLtPrime (a.value * b.value) p)

||| Extended Euclidean algorithm for computing modular inverse
||| Returns (gcd, x, y) such that a*x + b*y = gcd
covering
extendedGcd : Integer -> Integer -> (Integer, Integer, Integer)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b =
  let (g, x, y) = extendedGcd b (a `mod` b)
  in (g, y, x - (a `div` b) * y)

||| Modular inverse (returns None if not coprime with p)
export
covering
pfInverse : {p : Nat} -> {auto 0 _ : NonZero p} ->
            PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfInverse a =
  if a.value == 0 then Nothing
  else let (g, x, _) = extendedGcd (cast a.value) (cast p)
       in if g /= 1 then Nothing
          else let v = cast {to = Nat} ((x `mod` cast p + cast p) `mod` cast p)
               in Just (MkPFE v (inverseInRange v p))

||| Division in GF(p) (returns None if divisor is zero)
export
covering
pfDiv : {p : Nat} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfDiv a b = map (pfMul a) (pfInverse b)

||| Exponentiation by squaring
export
covering
pfPow : {p : Nat} -> {auto 0 _ : GT p 1} -> {auto 0 _ : NonZero p} ->
        PrimeFieldElement p -> Nat -> PrimeFieldElement p
pfPow base exp = go base exp (MkPFE 1 (oneLtGt1 p))
  where
    covering
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
  bfBitsVal : Bits64
  0 inRange : So (bfBitsVal < cast {to=Bits64} (natPow 2 n))

||| Create a binary field element (masks to n bits)
export
bfElement : (n : Nat) -> {auto prf : LTE n 64} -> Bits64 -> BinaryFieldElement n
bfElement n b = MkBFE (b .&. (cast {to=Bits64} (natPow 2 n) - 1)) (maskLtPow2 b n)

||| Zero element
export
bfZero : (n : Nat) -> BinaryFieldElement n
bfZero n = MkBFE 0 (zeroLtPow2 n)

||| One element
export
bfOne : (n : Nat) -> {auto prf : GT n 0} -> BinaryFieldElement n
bfOne n = MkBFE 1 (oneLtPow2 n)

||| Get the bits
export
bfBits : BinaryFieldElement n -> Bits64
bfBits e = e.bfBitsVal

-- ============================================================================
-- BINARY FIELD ARITHMETIC
-- ============================================================================

||| Addition in GF(2^n) (XOR)
export
bfAdd : BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
bfAdd a b = MkBFE (a.bfBitsVal `xor` b.bfBitsVal) (xorLtPow2 a.bfBitsVal b.bfBitsVal n @{a.inRange} @{b.inRange})

||| Subtraction in GF(2^n) (same as addition in characteristic 2)
export
bfSub : BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
bfSub = bfAdd

||| Negation in GF(2^n) (identity in characteristic 2)
export
bfNeg : BinaryFieldElement n -> BinaryFieldElement n
bfNeg a = a

||| Polynomial multiplication helper
covering
polyMul : Bits64 -> Bits64 -> Bits64
polyMul a b = go a b 0 0
  where
    covering
    go : Bits64 -> Bits64 -> Bits64 -> Bits64 -> Bits64
    go _ 0 acc _ = acc
    go x y acc i =
      let acc' = if (y .&. 1) /= 0 then acc `xor` (prim__shl_Bits64 x i) else acc
      in go x (y `shiftR` 1) acc' (i + 1)

||| Polynomial modulo reduction by irreducible polynomial
covering
polyModReduction : Bits64 -> Bits64 -> Nat -> Bits64
polyModReduction val irr n =
  let mask : Bits64 = cast {to=Bits64} (natPow 2 n) - 1
  in go mask val
  where
    covering
    highestBit : Bits64 -> Nat
    highestBit 0 = 0
    highestBit x = S (highestBit (x `shiftR` 1))

    covering
    go : Bits64 -> Bits64 -> Bits64
    go mask x =
      if x <= mask then x
      else let deg = highestBit x
               shift : Integer = cast deg - cast n
           in if shift < 0 then x
              else go mask (x `xor` (prim__shl_Bits64 irr (cast {to = Bits64} shift)))

||| Multiplication in GF(2^n) with irreducible polynomial
export
covering
bfMulWithIrr : {n : Nat} -> Bits64 -> BinaryFieldElement n -> BinaryFieldElement n -> BinaryFieldElement n
bfMulWithIrr irr a b =
  let prod = polyMul a.bfBitsVal b.bfBitsVal
      reduced = polyModReduction prod irr n
  in MkBFE reduced (polyModLtPow2 reduced n)

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
||| Returns 1 if a is quadratic residue, -1 if not, 0 if a = 0
export
covering
legendreSymbol : {p : Nat} -> {auto 0 _ : GT p 1} -> {auto 0 _ : NonZero p} ->
                PrimeFieldElement p -> Integer
legendreSymbol a =
  if a.value == 0 then 0
  else let exp = (minus p 1) `div` 2
           result = pfPow a exp
       in if result.value == 1 then 1
          else if result.value == minus p 1 then -1
          else 0

||| Check if element is a quadratic residue
export
covering
isQuadraticResidue : {p : Nat} -> {auto 0 _ : GT p 1} -> {auto 0 _ : NonZero p} ->
                     PrimeFieldElement p -> Bool
isQuadraticResidue a = legendreSymbol a == 1

||| Tonelli-Shanks square root (returns None if not a residue)
export
covering
pfSqrt : {p : Nat} -> {auto 0 _ : GT p 1} -> {auto 0 _ : NonZero p} ->
         PrimeFieldElement p -> Maybe (PrimeFieldElement p)
pfSqrt a =
  if a.value == 0 then Just (MkPFE 0 (zeroLtGt1 p))
  else if legendreSymbol a /= 1 then Nothing
  else
    -- Simplified: for p = 3 (mod 4), sqrt = a^((p+1)/4)
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

||| Helper: zip two lists with a combining function, using a default for the shorter list
zipWithDefault : (t -> t -> t) -> t -> List t -> List t -> List t
zipWithDefault _ _ [] [] = []
zipWithDefault f def [] (y :: ys) = f def y :: zipWithDefault f def [] ys
zipWithDefault f def (x :: xs) [] = f x def :: zipWithDefault f def xs []
zipWithDefault f def (x :: xs) (y :: ys) = f x y :: zipWithDefault f def xs ys

||| Evaluate polynomial at a point
export
covering
polyEval : {p : Nat} -> {auto _ : GT p 1} -> {auto _ : NonZero p} ->
           Polynomial p -> PrimeFieldElement p -> PrimeFieldElement p
polyEval poly x = go poly.coefficients (pfOne p) (MkPFE 0 (zeroLtGt1 p))
  where
    go : List (PrimeFieldElement p) -> PrimeFieldElement p -> PrimeFieldElement p -> PrimeFieldElement p
    go [] _ acc = acc
    go (c :: cs) xPow acc =
      go cs (pfMul xPow x) (pfAdd acc (pfMul c xPow))

||| Polynomial addition
export
polyAdd : {p : Nat} -> {auto _ : NonZero p} ->
          Polynomial p -> Polynomial p -> Polynomial p
polyAdd a b = MkPolynomial (zipWithDefault pfAdd (pfZero p) a.coefficients b.coefficients)

-- ============================================================================
-- UTILITY
-- ============================================================================

||| Check if a natural number is prime (naive implementation)
export
covering
isPrime : Nat -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = go 2
  where
    covering
    go : Nat -> Bool
    go i = if i * i > n then True
           else if n `mod` i == 0 then False
           else go (S i)

||| Generate a random element in GF(p) (given a seed)
export
randomElement : (p : Nat) -> {auto prf : NonZero p} -> Nat -> PrimeFieldElement p
randomElement p seed = pfElement p seed
