-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafeCrypto operations
|||
||| This module contains proofs verifying properties of cryptographic operations.
||| Non-trivial proofs are declared as postulates pending formal verification.
module Proven.SafeCrypto.Proofs

import Proven.Core
import Proven.SafeCrypto.Hash
import Proven.SafeCrypto.Random
import Data.List
import Data.Bits
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Constant-Time Properties
--------------------------------------------------------------------------------

||| Constant-time comparison is reflexive
export postulate
constantTimeRefl : (xs : List Bits8) -> digestEqList xs xs = True

||| Constant-time comparison is symmetric
export postulate
constantTimeSym : (xs, ys : List Bits8) ->
                  constantTimeEqList xs ys = constantTimeEqList ys xs

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
export postulate
modernIsSecure : (alg : HashAlg) ->
                 securityLevel alg = Modern ->
                 isSecure alg = True

||| Standard algorithms are secure
export postulate
standardIsSecure : (alg : HashAlg) ->
                   securityLevel alg = Standard ->
                   isSecure alg = True

--------------------------------------------------------------------------------
-- Digest Comparison Properties
--------------------------------------------------------------------------------

||| Digest equality is reflexive
export postulate
digestEqRefl : (d : ByteVector n) -> digestEq d d = True

||| Digest equality is symmetric
export postulate
digestEqSym : (d1, d2 : ByteVector n) -> digestEq d1 d2 = digestEq d2 d1

||| Different digests compare unequal (probabilistic property)
export postulate
differentDigestsUnequal : (d1, d2 : ByteVector n) ->
                          d1 /= d2 ->
                          digestEq d1 d2 = False

--------------------------------------------------------------------------------
-- Random Generation Properties
--------------------------------------------------------------------------------

||| Random bytes generates correct length output
export postulate
randomBytesLength : (n : Nat) ->
                    case randomBytes n of
                      Right (MkByteVec v) => length v = n
                      Left _ => ()

||| Random generation within bounds
export postulate
randomNatBounded : (max : Nat) -> {auto ok : IsSucc max} ->
                   case randomNat max of
                     Right n => LT n max
                     Left _ => ()

||| Random range respects min and max bounds
export postulate
randomRangeBounded : (min, max : Nat) -> {auto ok : LTE min max} ->
                     case randomNatRange min max of
                       Right n => (LTE min n, LTE n max)
                       Left _ => ()

--------------------------------------------------------------------------------
-- Nonce Properties
--------------------------------------------------------------------------------

||| Counter nonces are unique for different counters
export postulate
counterNonceUnique : (prefix : ByteVec 8) -> (c1, c2 : Bits64) ->
                     c1 /= c2 ->
                     counterNonce prefix c1 /= counterNonce prefix c2

||| Fresh nonces have correct size
export postulate
freshNonceSize : (n : Nat) ->
                 case freshNonce n of
                   Right (MkByteVec v) => length v = n
                   Left _ => ()

--------------------------------------------------------------------------------
-- Token Generation Properties
--------------------------------------------------------------------------------

||| Random token has expected length (base64 expansion)
export postulate
tokenLengthApprox : (bytes : Nat) ->
                    case randomToken bytes of
                      Right s => length s <= (bytes * 4 `div` 3) + 3
                      Left _ => ()

||| UUID v4 has correct format (36 chars with hyphens)
export postulate
uuidLength : case randomUUID of
               Right s => length s = 36
               Left _ => ()

--------------------------------------------------------------------------------
-- Sensitive Data Properties
--------------------------------------------------------------------------------

||| Map over sensitive preserves structure
public export
mapSensitivePreserves : (f : a -> b) -> (s : Sensitive a) ->
                        unsafeReveal (mapSensitive f s) = f (unsafeReveal s)
mapSensitivePreserves f (MkSensitive v) = Refl

||| Combine sensitive applies function correctly
public export
combineSensitiveCorrect : (f : a -> b -> c) -> (sa : Sensitive a) -> (sb : Sensitive b) ->
                          unsafeReveal (combineSensitive f sa sb) =
                          f (unsafeReveal sa) (unsafeReveal sb)
combineSensitiveCorrect f (MkSensitive a) (MkSensitive b) = Refl

--------------------------------------------------------------------------------
-- Hex Conversion Properties
--------------------------------------------------------------------------------

||| Hex encoding is deterministic
public export
hexEncodeDeterministic : (bs : List Bits8) ->
                         bytesToHex bs = bytesToHex bs
  where
    bytesToHex : List Bits8 -> String
    bytesToHex bytes = concat (map byteToHex bytes)
      where
        hexDigit : Nat -> Char
        hexDigit n = if n < 10 then chr (ord '0' + cast n) else chr (ord 'a' + cast n - 10)

        byteToHex : Bits8 -> String
        byteToHex b =
          let n = cast {to=Nat} b
          in pack [hexDigit (n `div` 16), hexDigit (n `mod` 16)]
hexEncodeDeterministic bs = Refl

||| Hex encoding produces even-length string
export postulate
hexEncodeEvenLength : (bs : List Bits8) ->
                      mod (length (bytesToHex bs)) 2 = 0
