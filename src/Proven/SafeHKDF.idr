-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeHKDF - HMAC-based Key Derivation Function parameter validation
|||
||| Validates HKDF parameters per RFC 5869. Actual HMAC computation
||| requires FFI — this module ensures inputs are well-formed.
||| Prevents: zero-length IKM, output exceeding 255*HashLen,
||| weak salt, algorithm confusion.
module Proven.SafeHKDF

import Data.String
import Data.List
import Data.Nat
import Data.So

%default total

||| Supported hash algorithms and their output lengths (bytes)
public export
data HKDFHash = HKDFSHA256 | HKDFSHA384 | HKDFSHA512

public export
Show HKDFHash where
  show HKDFSHA256 = "SHA-256"
  show HKDFSHA384 = "SHA-384"
  show HKDFSHA512 = "SHA-512"

public export
Eq HKDFHash where
  HKDFSHA256 == HKDFSHA256 = True
  HKDFSHA384 == HKDFSHA384 = True
  HKDFSHA512 == HKDFSHA512 = True
  _ == _ = False

||| Hash output length in bytes
public export
hashLen : HKDFHash -> Nat
hashLen HKDFSHA256 = 32
hashLen HKDFSHA384 = 48
hashLen HKDFSHA512 = 64

||| Maximum output length: 255 * HashLen (RFC 5869 Section 2.3)
public export
maxOutputLen : HKDFHash -> Nat
maxOutputLen h = 255 * hashLen h

||| Validated HKDF parameters
public export
record HKDFParams where
  constructor MkHKDFParams
  algorithm  : HKDFHash
  ikmLength  : Nat        -- Input keying material length (bytes)
  saltLength : Nat        -- Salt length (bytes, 0 = use default)
  infoLength : Nat        -- Context info length (bytes)
  outputLen  : Nat        -- Desired output length (bytes)

||| HKDF validation errors
public export
data HKDFError =
    EmptyIKM                   -- IKM must be non-empty
  | OutputTooLong Nat Nat      -- Requested vs max
  | SaltTooShort Nat Nat       -- Actual vs recommended minimum

public export
Show HKDFError where
  show EmptyIKM = "Input keying material must be non-empty"
  show (OutputTooLong req maxL) = "Output length " ++ show req ++ " exceeds maximum " ++ show maxL
  show (SaltTooShort actual minL) = "Salt length " ++ show actual ++ " below recommended " ++ show minL

||| Validate HKDF parameters
public export
validateHKDF : HKDFParams -> List HKDFError
validateHKDF p =
  let ikmErr = if p.ikmLength == 0 then [EmptyIKM] else []
      outErr = if p.outputLen > maxOutputLen p.algorithm
                 then [OutputTooLong p.outputLen (maxOutputLen p.algorithm)] else []
      saltWarn = if p.saltLength > 0 && p.saltLength < hashLen p.algorithm
                   then [SaltTooShort p.saltLength (hashLen p.algorithm)] else []
  in ikmErr ++ outErr ++ saltWarn

||| Check if parameters are valid (no errors)
public export
isValid : HKDFParams -> Bool
isValid = isNil . validateHKDF

||| Create validated parameters (returns Nothing if invalid)
public export
mkHKDFParams : HKDFHash -> (ikm : Nat) -> (salt : Nat) -> (info : Nat) -> (outLen : Nat) -> Maybe HKDFParams
mkHKDFParams h ikm salt info outLen =
  let params = MkHKDFParams h ikm salt info outLen
  in if isValid params then Just params else Nothing

||| Number of HMAC iterations needed (N = ceil(L/HashLen))
public export
iterations : HKDFParams -> Nat
iterations p =
  let hl = hashLen p.algorithm
  in if hl == 0 then 0
     else (p.outputLen + hl `minus` 1) `div` hl
