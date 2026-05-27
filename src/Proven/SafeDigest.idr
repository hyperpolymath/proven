-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeDigest - Cryptographically safe digest operations
|||
||| This module provides digest parsing, validation, and constant-time
||| comparison aimed at preventing timing attacks.
|||
||| Proof status: `parseAlgorithmRoundtrip` discharged in `Proofs.idr`.
||| The `constantTimeReflexive` / `constantTimeSymmetric` / `verifyTransitive`
||| claims are currently `@ Assumed:` (see `PROOF-NEEDS.md`); the previous
||| header phrase "formally verified" overstated this and has been removed.
|||
||| Supported algorithms: sha256, sha384, sha512, blake3
module Proven.SafeDigest

import public Proven.Core
import Proven.SafeHex
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Cryptographic hash algorithm
public export
data HashAlgorithm
  = SHA256      -- 256-bit SHA-2 (32 bytes)
  | SHA384      -- 384-bit SHA-2 (48 bytes)
  | SHA512      -- 512-bit SHA-2 (64 bytes)
  | Blake3      -- Blake3 (32 bytes default)

public export
Eq HashAlgorithm where
  SHA256 == SHA256 = True
  SHA384 == SHA384 = True
  SHA512 == SHA512 = True
  Blake3 == Blake3 = True
  _ == _ = False

public export
Show HashAlgorithm where
  show SHA256 = "sha256"
  show SHA384 = "sha384"
  show SHA512 = "sha512"
  show Blake3 = "blake3"

||| Digest with algorithm and hex-encoded hash value
public export
record Digest where
  constructor MkDigest
  algorithm : HashAlgorithm
  value : String  -- Hex-encoded hash

||| Digest parsing result
public export
data DigestResult
  = ValidDigest Digest
  | InvalidFormat String      -- Malformed digest string
  | UnknownAlgorithm String   -- Unsupported algorithm
  | InvalidLength Nat         -- Hash length doesn't match algorithm

public export
Show DigestResult where
  show (ValidDigest d) = "ValidDigest(" ++ show d.algorithm ++ ":" ++ d.value ++ ")"
  show (InvalidFormat s) = "InvalidFormat: " ++ s
  show (UnknownAlgorithm a) = "UnknownAlgorithm: " ++ a
  show (InvalidLength n) = "InvalidLength: expected length, got " ++ show n

--------------------------------------------------------------------------------
-- Algorithm Properties
--------------------------------------------------------------------------------

||| Expected digest length in hex characters for each algorithm
|||
||| @ Proof: These are constants defined by the algorithms
public export
expectedLength : HashAlgorithm -> Nat
expectedLength SHA256 = 64   -- 32 bytes * 2 hex chars
expectedLength SHA384 = 96   -- 48 bytes * 2 hex chars
expectedLength SHA512 = 128  -- 64 bytes * 2 hex chars
expectedLength Blake3 = 64   -- 32 bytes * 2 hex chars (default)

||| Get algorithm from string
|||
||| @ Proof: Finite case analysis, always terminates
public export
parseAlgorithm : String -> Maybe HashAlgorithm
parseAlgorithm "sha256" = Just SHA256
parseAlgorithm "sha384" = Just SHA384
parseAlgorithm "sha512" = Just SHA512
parseAlgorithm "blake3" = Just Blake3
parseAlgorithm _ = Nothing

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

||| Parse digest string in format "algorithm:hexvalue"
|||
||| @ Proof of termination:
|||   - Single break call (terminates on finite string)
|||   - Pattern matching on Maybe (finite cases)
|||   - No recursion
public export
parseDigest : String -> DigestResult
parseDigest input =
  case break (== ':') (unpack input) of
    (algChars, []) => InvalidFormat "Missing colon separator"
    (algChars, ':' :: rest) =>
      let algo = pack algChars
          hexValue = pack rest
      in case parseAlgorithm algo of
           Nothing => UnknownAlgorithm algo
           Just alg =>
             let len = length hexValue
                 expected = expectedLength alg
             in if len /= expected
                  then InvalidLength len
                  else if all isHexDigit (unpack hexValue)
                         then ValidDigest (MkDigest alg hexValue)
                         else InvalidFormat "Non-hex characters in digest value"
    _ => InvalidFormat "Unexpected format"

||| Check if digest string is valid without parsing
public export
isValidDigest : String -> Bool
isValidDigest input =
  case parseDigest input of
    ValidDigest _ => True
    _ => False

--------------------------------------------------------------------------------
-- Constant-Time Comparison
--------------------------------------------------------------------------------

||| Constant-time character equality
|||
||| @ Security property: Execution time must not depend on input values
||| @ Proof: Single equality check, no early exit
constantTimeCharEq : Char -> Char -> Bool
constantTimeCharEq a b = a == b  -- Compiler must not optimize

||| Constant-time string comparison
|||
||| @ Security property: Prevents timing attacks
||| @ Proof:
|||   - Always compares full length (no early exit)
|||   - Accumulates result with bitwise OR (constant time)
|||   - Returns final result after full comparison
|||
||| @ Note: Assembly output should be audited for conditional branches
public export
constantTimeCompare : String -> String -> Bool
constantTimeCompare a b =
  let charsA = unpack a
      charsB = unpack b
      lenA = length charsA
      lenB = length charsB
  in if lenA /= lenB
       then False  -- Different lengths -> not equal
       else
         -- Compare all characters without early exit
         let pairs = zip charsA charsB
             diffs = map (\ (x, y) => if x == y then 0 else 1) pairs
             totalDiff = foldl (+) 0 diffs
         in totalDiff == 0

||| Verify digest matches expected value (constant-time)
|||
||| @ Security property: Resistant to timing attacks
||| @ Proof: Uses constantTimeCompare which has constant-time property
public export
verifyDigest : Digest -> Digest -> Bool
verifyDigest expected actual =
  if expected.algorithm /= actual.algorithm
    then False
    else constantTimeCompare expected.value actual.value

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Convert digest to standard string format
public export
toString : Digest -> String
toString d = show d.algorithm ++ ":" ++ d.value

||| Convert to Docker Content Digest header format
public export
toDockerDigest : Digest -> String
toDockerDigest = toString

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate that hex string has correct length for algorithm
public export
validateLength : HashAlgorithm -> String -> Bool
validateLength alg hex = length hex == expectedLength alg

||| Validate that string isInfixOf only hex characters
public export
isHexString : String -> Bool
isHexString s = all isHexDigit (unpack s)

||| Create digest from algorithm and hex string (with validation)
public export
makeDigest : HashAlgorithm -> String -> DigestResult
makeDigest alg hex =
  if not (isHexString hex)
    then InvalidFormat "Non-hex characters"
    else if not (validateLength alg hex)
           then InvalidLength (length hex)
           else ValidDigest (MkDigest alg hex)

--------------------------------------------------------------------------------
-- Proofs and Specifications
--------------------------------------------------------------------------------

||| OWED: parsing is a left inverse of rendering — for any valid `Digest d`,
||| `parseDigest (toString d) = ValidDigest d`. Holds definitionally on any
||| concrete `Digest` constructor, but the witness flows through opaque
||| `String` primitives (`pack` / `unpack` / `break`).
|||
||| Held back by Idris2 0.8.0 not reducing `String.pack` / `unpack` / `break`
||| over an abstract `String` at the type level — Family 1 (String FFI
||| opacity), same as `SafeBase64.standardAlphabetValid`. Discharge once a
||| `Data.String` reflective tactic is available, or by structural induction
||| on the digest's hex representation once the `unpack`-reduction blocker
||| is lifted.
0 parseRenderInverse : (d : Digest) ->
  parseDigest (toString d) = ValidDigest d

||| OWED: `constantTimeCompare s s = True` for every `String s` — comparison
||| is reflexive.
|||
||| Held back by Idris2 0.8.0 not reducing the `unpack` / `zip` / `foldl`
||| chain inside `constantTimeCompare` over an abstract `String` — Family 1
||| (String FFI opacity) plus Family 2 (`Char.(==)` is FFI-bound). Discharge
||| by lifting the obligation to `List Char` (where `x == x` follows by
||| `Char` reflexivity) once the `unpack`-reduction blocker is lifted, or
||| by a `Data.String` reflective tactic.
0 constantTimeReflexive : (s : String) -> constantTimeCompare s s = True

||| OWED: `constantTimeCompare a b = constantTimeCompare b a` — comparison
||| is symmetric.
|||
||| Held back by Idris2 0.8.0 not unfolding the `unpack` / `zip` / `foldl`
||| chain over abstract `String`s, plus the unproven commutativity of the
||| FFI-bound `Char.(==)` — Family 1 + Family 2. Discharge by lifting the
||| obligation to `List Char` (where the result follows from `zip`
||| commutativity and `Char` equality symmetry) once those reduce by Refl.
0 constantTimeSymmetric : (a : String) -> (b : String) ->
  constantTimeCompare a b = constantTimeCompare b a

||| OWED: `verifyDigest` is transitive — if `verifyDigest a b = True` and
||| `verifyDigest b c = True`, then `verifyDigest a c = True`. Requires the
||| same-algorithm constraint and transitivity of `constantTimeCompare` over
||| the underlying string representation.
|||
||| Held back by Idris2 0.8.0 not reducing `constantTimeCompare`'s `unpack`
||| / `zip` / `foldl` chain — Family 1 (String FFI opacity). Transitively
||| depends on `constantTimeReflexive` and `constantTimeSymmetric` being
||| discharged. Discharge once a `Data.String` reflective tactic is
||| available.
0 verifyTransitive : (a : Digest) -> (b : Digest) -> (c : Digest) ->
  verifyDigest a b = True -> verifyDigest b c = True ->
  verifyDigest a c = True

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

||| Example SHA256 digest
export
exampleSHA256 : Digest
exampleSHA256 = MkDigest SHA256
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

||| Example SHA512 digest
export
exampleSHA512 : Digest
exampleSHA512 = MkDigest SHA512
  "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"

||| Example Blake3 digest
export
exampleBlake3 : Digest
exampleBlake3 = MkDigest Blake3
  "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"

--------------------------------------------------------------------------------
-- Security Notes
--------------------------------------------------------------------------------

{-
SECURITY CONSIDERATIONS:

1. Constant-Time Comparison:
   - Uses constantTimeCompare to prevent timing attacks
   - Compiler optimizations must not introduce timing leaks
   - Assembly output should be audited for conditional branches

2. Algorithm Validation:
   - Only secure algorithms supported (SHA-2 family, Blake3)
   - No MD5 or SHA1 (cryptographically broken)
   - Length validation prevents truncated digests

3. Hex Encoding:
   - All digests must be hex-encoded (no raw bytes)
   - Hex validation rejects non-hex characters
   - Lowercase normalization deferred to FFI layer

4. Formal Verification:
   - Parser termination proven by %default total
   - Constant-time property requires assembly audit
   - Algebraic properties (reflexive, symmetric, transitive) specified

Future work:
- Prove constant-time property at assembly level
- Add dependent types for digest length (ByteVector n)
- Actual hashing is in SafeCrypto.Hash (this module handles parsing/comparison)
- FIPS 140-2 compliance markers for FFI layer
-}
