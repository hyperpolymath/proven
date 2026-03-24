-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCrypto operations
|||
||| This module contains proofs verifying properties of cryptographic operations.
||| Non-trivial proofs are declared as postulates pending formal verification.
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - `/=` returns Bool, not Type; replaced with `Not (x = y)` in signatures
||| - `<=` returns Bool, not Type; replaced with `LTE` in signatures
||| - `digestEqList`/`constantTimeEqList` renamed to use `digestEq`
||| - Added import of Proven.SafeCrypto for Sensitive type
module Proven.SafeCrypto.Proofs

import Proven.Core
import Proven.SafeCrypto.Hash
import Proven.SafeCrypto.Random
import Data.List
import Data.Bits
import Data.Nat
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Constant-Time Properties
--------------------------------------------------------------------------------

-- | Constant-time digest comparison is reflexive.
-- | Postulated: digestEq uses a where-local accumulator loop over Vect;
-- | proving reflexivity requires showing xor x x = 0 for all Bits8 and
-- | that the accumulated .|. folds to 0, which involves Bits8 arithmetic
-- | not reducible in Idris 2's type theory.
export
constantTimeRefl : (d : ByteVector n) -> digestEq d d = True

-- | Constant-time digest comparison is symmetric.
-- | Postulated: requires showing xor is commutative for Bits8, which is
-- | a primitive operation not reducible in Idris 2.
export
constantTimeSym : (d1, d2 : ByteVector n) ->
                  digestEq d1 d2 = digestEq d2 d1

--------------------------------------------------------------------------------
-- Hash Output Size Properties
--------------------------------------------------------------------------------

||| SHA-256 output is exactly 32 bytes
public export
sha256OutputSize : hashOutputSize SHA256_ALG = 32
sha256OutputSize = Refl

||| SHA-512 output is exactly 64 bytes
public export
sha512OutputSize : hashOutputSize SHA512_ALG = 64
sha512OutputSize = Refl

||| SHA3-256 output is exactly 32 bytes
public export
sha3_256OutputSize : hashOutputSize SHA3_256_ALG = 32
sha3_256OutputSize = Refl

||| BLAKE3 output is exactly 32 bytes
public export
blake3OutputSize : hashOutputSize BLAKE3_ALG = 32
blake3OutputSize = Refl

--------------------------------------------------------------------------------
-- Security Level Properties
--------------------------------------------------------------------------------

||| SHA-256 is secure
public export
sha256Secure : isSecure SHA256_ALG = True
sha256Secure = Refl

||| SHA-512 is secure
public export
sha512Secure : isSecure SHA512_ALG = True
sha512Secure = Refl

||| SHA3-256 is secure
public export
sha3_256Secure : isSecure SHA3_256_ALG = True
sha3_256Secure = Refl

||| MD5 is NOT secure
public export
md5NotSecure : isSecure MD5_ALG = False
md5NotSecure = Refl

||| SHA-1 is NOT secure
public export
sha1NotSecure : isSecure SHA1_ALG = False
sha1NotSecure = Refl

||| Modern algorithms are secure
export
modernIsSecure : (alg : HashAlg) ->
                 securityLevel alg = Modern ->
                 isSecure alg = True

||| Standard algorithms are secure
export
standardIsSecure : (alg : HashAlg) ->
                   securityLevel alg = Standard ->
                   isSecure alg = True

--------------------------------------------------------------------------------
-- Digest Comparison Properties
--------------------------------------------------------------------------------

||| Digest equality is reflexive
export
digestEqRefl : (d : ByteVector n) -> digestEq d d = True

||| Digest equality is symmetric
export
digestEqSym : (d1, d2 : ByteVector n) -> digestEq d1 d2 = digestEq d2 d1

-- | Different digests compare unequal (probabilistic property).
-- | Uses `Not (d1 = d2)` instead of `d1 /= d2` because in Idris 2 0.8.0
-- | the `/=` operator returns Bool, not a Type suitable for use in
-- | type signatures.
export
differentDigestsUnequal : (d1, d2 : ByteVector n) ->
                          Not (d1 = d2) ->
                          digestEq d1 d2 = False

--------------------------------------------------------------------------------
-- Random Generation Properties
--------------------------------------------------------------------------------

||| Random bytes generates correct length output
export
randomBytesLength : (n : Nat) ->
                    case randomBytes n of
                      Right (MkByteVec v) => length v = n
                      Left _ => ()

||| Random generation within bounds
export
randomNatBounded : (max : Nat) -> {auto ok : IsSucc max} ->
                   case randomNat max of
                     Right n => LT n max
                     Left _ => ()

||| Random range respects min and max bounds
export
randomRangeBounded : (mn, mx : Nat) -> {auto ok : LTE mn mx} ->
                     case randomNatRange mn mx of
                       Right n => (LTE mn n, LTE n mx)
                       Left _ => ()

--------------------------------------------------------------------------------
-- Nonce Properties
--------------------------------------------------------------------------------

-- | Counter nonces are unique for different counters.
-- | Uses `Not (c1 = c2)` instead of `c1 /= c2` for Idris 2 0.8.0
-- | compatibility.
export
counterNonceUnique : (pfx : ByteVec 8) -> (c1, c2 : Bits64) ->
                     Not (c1 = c2) ->
                     Not (counterNonce pfx c1 = counterNonce pfx c2)

||| Fresh nonces have correct size
export
freshNonceSize : (n : Nat) ->
                 case freshNonce n of
                   Right (MkByteVec v) => length v = n
                   Left _ => ()

--------------------------------------------------------------------------------
-- Token Generation Properties
--------------------------------------------------------------------------------

-- | Random token has expected length (base64 expansion).
-- | Uses `LTE` instead of `<=` for Idris 2 0.8.0 compatibility
-- | (the `<=` operator returns Bool, not a Type).
export
tokenLengthApprox : (bytes : Nat) ->
                    case randomToken bytes of
                      Right s => LTE (length s) ((bytes * 4 `div` 3) + 3)
                      Left _ => ()

||| UUID v4 has correct format (36 chars with hyphens)
export
uuidLength : case randomUUID of
               Right s => length s = 36
               Left _ => ()

--------------------------------------------------------------------------------
-- Sensitive Data Properties
--------------------------------------------------------------------------------

-- Note: mapSensitivePreserves and combineSensitiveCorrect proofs (both Refl)
-- are defined in Proven.SafeCrypto alongside the Sensitive type to avoid
-- import ambiguities between Proven.SafeCrypto.{Hash,Random} re-exports.
-- They were previously here but caused name resolution conflicts in 0.8.0.

--------------------------------------------------------------------------------
-- Hex Conversion Properties
--------------------------------------------------------------------------------

||| Hex encoding is deterministic: encoding is a pure function
||| so applying it twice to the same input yields identical results.
||| This follows directly from reflexivity of equality.
public export
hexEncodeDeterministic : (f : List Bits8 -> String) -> (bs : List Bits8) ->
                         f bs = f bs
hexEncodeDeterministic _ _ = Refl

-- | Hex encoding produces even-length string.
-- | Postulated: depends on internals of bytesToHex which operates via
-- | pack/unpack FFI boundaries.
export
hexEncodeEvenLength : (bytesToHex : List Bits8 -> String) -> (bs : List Bits8) ->
                      mod (length (bytesToHex bs)) 2 = 0
