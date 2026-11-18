-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCryptoProps

import Proven.Core
import Proven.SafeCrypto
import Proven.SafeCrypto.Hash
import Proven.SafeCrypto.Random
import Data.List
import Data.Vect
import System

%default total

-- Real API: digests are fixed-size `ByteVector n` values
-- (`sha256 : List Bits8 -> SHA256Digest = ByteVector 32`, `sha512` ->
-- `ByteVector 64`, `hmacSha256` -> `ByteVector 32`) and
-- `randomBytes : (n : Nat) -> RandomResult (ByteVec n)`, all in the
-- sibling modules Proven.SafeCrypto.Hash/Random. Output lengths are carried
-- by the type index, so they are proven below for EVERY input.
--
-- The hash/HMAC/random primitives (`prim__sha256`, `prim__randomBytes`,
-- ...) are bodyless declarations awaiting FFI, so they cannot be executed
-- here; only the pure helpers (constantTimeEq, bytesToHex) are run.
--
-- Retired as tautologies (not weakened): the former prop_hashDeterministic
-- (`sha256 s = sha256 s`) and prop_hmacDeterministic were `x = x` by
-- reflexivity and said nothing about the library; determinism follows from
-- purity. They are replaced by the digest-length proofs below.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| Number of bytes in a digest.
private
digestLength : ByteVector n -> Nat
digestLength (MkByteVector v) = length v

||| Number of bytes in a random byte vector.
private
byteVecLength : ByteVec n -> Nat
byteVecLength (MkByteVec v) = length v

private
natEqRefl : (n : Nat) -> (n == n) = True
natEqRefl Z = Refl
natEqRefl (S k) = natEqRefl k

||| SHA-256 always yields 32 bytes (64 hex characters), for every input.
prop_sha256Length : (input : List Bits8) -> digestLength (sha256 input) = 32
prop_sha256Length input with (sha256 input)
  _ | MkByteVector v = lengthCorrect v

||| SHA-512 always yields 64 bytes, for every input.
prop_sha512Length : (input : List Bits8) -> digestLength (sha512 input) = 64
prop_sha512Length input with (sha512 input)
  _ | MkByteVector v = lengthCorrect v

||| SHA-512 output is strictly longer than SHA-256 output, for all inputs.
prop_sha512LongerThanSha256 : (a, b : List Bits8) ->
                              LT (digestLength (sha256 a)) (digestLength (sha512 b))
prop_sha512LongerThanSha256 a b =
  rewrite prop_sha256Length a in rewrite prop_sha512Length b in
    LTESucc (lteAddRight 32)

||| HMAC-SHA256 always yields 32 bytes, for every key and message.
prop_hmacSha256Length : (key, msg : List Bits8) -> digestLength (hmacSha256 key msg) = 32
prop_hmacSha256Length key msg with (hmacSha256 key msg)
  _ | MkByteVector v = lengthCorrect v

||| Random bytes: every successful result has exactly the requested length.
prop_randomBytesLength : (n : Nat) ->
  either (const True) (\r => byteVecLength r == n) (randomBytes n) = True
prop_randomBytesLength n with (randomBytes n)
  _ | Left _ = Refl
  _ | Right (MkByteVec v) = rewrite lengthCorrect v in natEqRefl n

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: constant-time comparison is reflexive. Blocked by: needs
||| `xor x x = 0` and `0 .|. 0 = 0` on Bits8, which are primitives with no
||| definitional reduction in Idris2 0.8.0 (same blocker as the library's
||| own SafeCrypto.Proofs.constantTimeRefl). This names the proposition; it
||| is not a witness.
public export
ConstantTimeEqReflexiveObligation : Type
ConstantTimeEqReflexiveObligation = (s : List Bits8) -> constantTimeEq s s = True

||| OWED: constant-time comparison is symmetric. Blocked by: needs `xor`
||| commutativity on Bits8 (primitive, no lemma in base/contrib). This names
||| the proposition; it is not a witness.
public export
ConstantTimeEqSymmetricObligation : Type
ConstantTimeEqSymmetricObligation =
  (a, b : List Bits8) -> constantTimeEq a b = constantTimeEq b a

||| OWED: hex rendering emits two characters per byte (so a SHA-256 digest
||| renders as 64 hex characters). Blocked by: String `concat`/`pack`/
||| `unpack` primitives do not reduce on a variable list. This names the
||| proposition; it is not a witness.
public export
BytesToHexLengthObligation : Type
BytesToHexLengthObligation =
  (bs : List Bits8) -> length (unpack (bytesToHex bs)) = 2 * length bs

--------------------------------------------------------------------------------
-- Execution checks (pure helpers only; hash primitives await FFI)
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
corpus : List (List Bits8)
corpus = [[], [0], [255], [1, 2, 3], [0, 0, 0, 1], replicate 32 171,
          map cast [the Integer 0 .. 255]]

||| Test runner for crypto properties
export
runCryptoProperties : IO ()
runCryptoProperties = do
  putStrLn "SafeCrypto Property Tests"
  putStrLn "========================="
  putStrLn "prop_sha256Length, prop_sha512Length, prop_sha512LongerThanSha256: PASS (proven by type)"
  putStrLn "prop_hmacSha256Length, prop_randomBytesLength: PASS (proven by type)"
  check "constant-time comparison reflexive on corpus"
    (all (\s => constantTimeEq s s) corpus)
  check "constant-time comparison symmetric on corpus"
    (all (\a => all (\b => constantTimeEq a b == constantTimeEq b a) corpus) corpus)
  check "constant-time comparison distinguishes unequal inputs"
    (all (\a => all (\b => constantTimeEq a b == (a == b)) corpus) corpus)
  check "32-byte digest renders as 64 hex characters"
    (length (unpack (bytesToHex (replicate 32 171))) == 64)
  check "hex rendering is two characters per byte on corpus"
    (all (\bs => length (unpack (bytesToHex bs)) == 2 * length bs) corpus)
  putStrLn ""
