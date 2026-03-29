-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafePBKDF2 - Password-Based Key Derivation Function 2 parameter validation
|||
||| Validates PBKDF2 parameters per RFC 8018 / NIST SP 800-132.
||| Prevents: low iteration counts, short salts, weak derived key lengths.
||| Actual PBKDF2 computation requires FFI.
module Proven.SafePBKDF2

import Data.String
import Data.Nat
import Data.So

%default total

||| Supported PRF algorithms
public export
data PBKDF2PRF = PRFSHA1 | PRFSHA256 | PRFSHA384 | PRFSHA512

public export
Show PBKDF2PRF where
  show PRFSHA1   = "HMAC-SHA1"
  show PRFSHA256 = "HMAC-SHA256"
  show PRFSHA384 = "HMAC-SHA384"
  show PRFSHA512 = "HMAC-SHA512"

public export
Eq PBKDF2PRF where
  PRFSHA1 == PRFSHA1 = True; PRFSHA256 == PRFSHA256 = True
  PRFSHA384 == PRFSHA384 = True; PRFSHA512 == PRFSHA512 = True
  _ == _ = False

||| PRF output length in bytes
public export
prfLen : PBKDF2PRF -> Nat
prfLen PRFSHA1   = 20
prfLen PRFSHA256 = 32
prfLen PRFSHA384 = 48
prfLen PRFSHA512 = 64

||| NIST minimum iteration count (2023 recommendation)
public export
MinIterations : Nat
MinIterations = 600000

||| Minimum salt length (bytes, NIST SP 800-132)
public export
MinSaltLength : Nat
MinSaltLength = 16

||| PBKDF2 parameters
public export
record PBKDF2Params where
  constructor MkPBKDF2Params
  prf          : PBKDF2PRF
  iterations   : Nat
  saltLength   : Nat
  dkLength     : Nat    -- Derived key length in bytes

||| Validation errors
public export
data PBKDF2Error =
    IterationsTooLow Nat      -- Below minimum
  | SaltTooShort Nat          -- Below minimum
  | DKLengthZero              -- Zero-length derived key
  | DKLengthTooLong Nat Nat   -- Exceeds (2^32-1) * hLen
  | WeakPRF                   -- SHA1 is deprecated

public export
Show PBKDF2Error where
  show (IterationsTooLow n) = "Iterations " ++ show n ++ " below minimum " ++ show MinIterations
  show (SaltTooShort n) = "Salt " ++ show n ++ " bytes below minimum " ++ show MinSaltLength
  show DKLengthZero = "Derived key length cannot be zero"
  show (DKLengthTooLong req maxL) = "DK length " ++ show req ++ " exceeds max " ++ show maxL
  show WeakPRF = "HMAC-SHA1 is deprecated; use SHA256 or higher"

||| Validate PBKDF2 parameters
public export
validatePBKDF2 : PBKDF2Params -> List PBKDF2Error
validatePBKDF2 p =
  let iterErr = if p.iterations < MinIterations then [IterationsTooLow p.iterations] else []
      saltErr = if p.saltLength < MinSaltLength then [SaltTooShort p.saltLength] else []
      dkZero  = if p.dkLength == 0 then [DKLengthZero] else []
      prfWarn = if p.prf == PRFSHA1 then [WeakPRF] else []
  in iterErr ++ saltErr ++ dkZero ++ prfWarn

||| Check if parameters meet security requirements
public export
isSecure : PBKDF2Params -> Bool
isSecure = isNil . validatePBKDF2

||| Recommended parameters for password hashing
public export
recommendedParams : PBKDF2Params
recommendedParams = MkPBKDF2Params PRFSHA256 600000 32 32

||| High-security parameters
public export
highSecurityParams : PBKDF2Params
highSecurityParams = MkPBKDF2Params PRFSHA512 1000000 64 64

||| Estimated computation time category
public export
data CostCategory = Fast | Moderate | Slow | VerySlow

public export
Show CostCategory where
  show Fast = "fast (<100ms)"
  show Moderate = "moderate (100ms-1s)"
  show Slow = "slow (1s-5s)"
  show VerySlow = "very slow (>5s)"

||| Estimate cost category from iteration count
public export
estimateCost : Nat -> CostCategory
estimateCost iters =
  if iters < 100000 then Fast
  else if iters < 600000 then Moderate
  else if iters < 2000000 then Slow
  else VerySlow
