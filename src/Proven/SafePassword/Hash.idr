-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Password Hashing Algorithms and Parameters
|||
||| Defines secure password hashing algorithm interfaces.
||| Actual implementations require FFI to native crypto libraries.
module Proven.SafePassword.Hash

import Proven.Core
import Data.List
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Hash Algorithm Types
--------------------------------------------------------------------------------

||| Supported password hashing algorithms
public export
data HashAlgorithm
  = Argon2id      -- Winner of PHC, recommended
  | Argon2i       -- Data-independent (side-channel resistant)
  | Argon2d       -- Data-dependent (GPU resistant)
  | Bcrypt        -- Widely supported, good
  | Scrypt        -- Memory-hard, older
  | PBKDF2_SHA256 -- Legacy, avoid for new systems
  | PBKDF2_SHA512 -- Legacy, avoid for new systems

public export
Show HashAlgorithm where
  show Argon2id = "argon2id"
  show Argon2i = "argon2i"
  show Argon2d = "argon2d"
  show Bcrypt = "bcrypt"
  show Scrypt = "scrypt"
  show PBKDF2_SHA256 = "pbkdf2-sha256"
  show PBKDF2_SHA512 = "pbkdf2-sha512"

public export
Eq HashAlgorithm where
  Argon2id == Argon2id = True
  Argon2i == Argon2i = True
  Argon2d == Argon2d = True
  Bcrypt == Bcrypt = True
  Scrypt == Scrypt = True
  PBKDF2_SHA256 == PBKDF2_SHA256 = True
  PBKDF2_SHA512 == PBKDF2_SHA512 = True
  _ == _ = False

||| Check if algorithm is recommended for new systems
public export
isRecommended : HashAlgorithm -> Bool
isRecommended Argon2id = True
isRecommended Argon2i = True
isRecommended Bcrypt = True
isRecommended Scrypt = True
isRecommended _ = False

--------------------------------------------------------------------------------
-- Argon2 Parameters
--------------------------------------------------------------------------------

||| Argon2 parameters
public export
record Argon2Params where
  constructor MkArgon2Params
  timeCost : Nat       -- Iterations (t)
  memoryCost : Nat     -- Memory in KiB (m)
  parallelism : Nat    -- Threads (p)
  hashLength : Nat     -- Output length in bytes
  saltLength : Nat     -- Salt length in bytes

public export
Show Argon2Params where
  show p = "Argon2(t=" ++ show p.timeCost ++
           ",m=" ++ show p.memoryCost ++
           ",p=" ++ show p.parallelism ++ ")"

||| Default Argon2id parameters (OWASP recommended)
public export
defaultArgon2Params : Argon2Params
defaultArgon2Params = MkArgon2Params
  { timeCost = 3
  , memoryCost = 65536    -- 64 MiB
  , parallelism = 4
  , hashLength = 32
  , saltLength = 16
  }

||| High-security Argon2id parameters
public export
highSecurityArgon2Params : Argon2Params
highSecurityArgon2Params = MkArgon2Params
  { timeCost = 4
  , memoryCost = 131072   -- 128 MiB
  , parallelism = 4
  , hashLength = 32
  , saltLength = 32
  }

||| Interactive (fast) Argon2id parameters
public export
interactiveArgon2Params : Argon2Params
interactiveArgon2Params = MkArgon2Params
  { timeCost = 2
  , memoryCost = 19456    -- ~19 MiB
  , parallelism = 1
  , hashLength = 32
  , saltLength = 16
  }

||| Validate Argon2 parameters
public export
validateArgon2Params : Argon2Params -> Either String Argon2Params
validateArgon2Params p =
  if p.timeCost < 1 then Left "Time cost must be >= 1"
  else if p.memoryCost < 8192 then Left "Memory cost must be >= 8 MiB"
  else if p.parallelism < 1 then Left "Parallelism must be >= 1"
  else if p.hashLength < 16 then Left "Hash length must be >= 16"
  else if p.saltLength < 16 then Left "Salt length must be >= 16"
  else Right p

--------------------------------------------------------------------------------
-- Bcrypt Parameters
--------------------------------------------------------------------------------

||| Bcrypt parameters
public export
record BcryptParams where
  constructor MkBcryptParams
  cost : Nat  -- Work factor (2^cost iterations)

public export
Show BcryptParams where
  show p = "Bcrypt(cost=" ++ show p.cost ++ ")"

||| Default bcrypt parameters
public export
defaultBcryptParams : BcryptParams
defaultBcryptParams = MkBcryptParams { cost = 12 }

||| High-security bcrypt parameters
public export
highSecurityBcryptParams : BcryptParams
highSecurityBcryptParams = MkBcryptParams { cost = 14 }

||| Validate bcrypt parameters
public export
validateBcryptParams : BcryptParams -> Either String BcryptParams
validateBcryptParams p =
  if p.cost < 10 then Left "Bcrypt cost must be >= 10"
  else if p.cost > 31 then Left "Bcrypt cost must be <= 31"
  else Right p

--------------------------------------------------------------------------------
-- Scrypt Parameters
--------------------------------------------------------------------------------

||| Scrypt parameters
public export
record ScryptParams where
  constructor MkScryptParams
  n : Nat     -- CPU/memory cost (must be power of 2)
  r : Nat     -- Block size
  p : Nat     -- Parallelism
  keyLength : Nat

public export
Show ScryptParams where
  show params = "Scrypt(N=" ++ show params.n ++
                ",r=" ++ show params.r ++
                ",p=" ++ show params.p ++ ")"

||| Default scrypt parameters
public export
defaultScryptParams : ScryptParams
defaultScryptParams = MkScryptParams
  { n = 16384   -- 2^14
  , r = 8
  , p = 1
  , keyLength = 32
  }

||| High-security scrypt parameters
public export
highSecurityScryptParams : ScryptParams
highSecurityScryptParams = MkScryptParams
  { n = 1048576  -- 2^20
  , r = 8
  , p = 1
  , keyLength = 32
  }

||| Validate scrypt parameters
public export
validateScryptParams : ScryptParams -> Either String ScryptParams
validateScryptParams p =
  if p.n < 16384 then Left "Scrypt N must be >= 16384"
  else if p.r < 8 then Left "Scrypt r must be >= 8"
  else if p.p < 1 then Left "Scrypt p must be >= 1"
  else Right p

--------------------------------------------------------------------------------
-- PBKDF2 Parameters
--------------------------------------------------------------------------------

||| PBKDF2 parameters
public export
record PBKDF2Params where
  constructor MkPBKDF2Params
  iterations : Nat
  keyLength : Nat

public export
Show PBKDF2Params where
  show p = "PBKDF2(iterations=" ++ show p.iterations ++ ")"

||| Default PBKDF2-SHA256 parameters (if you must use PBKDF2)
public export
defaultPBKDF2Params : PBKDF2Params
defaultPBKDF2Params = MkPBKDF2Params
  { iterations = 600000  -- OWASP 2023 recommendation
  , keyLength = 32
  }

--------------------------------------------------------------------------------
-- Unified Hash Parameters
--------------------------------------------------------------------------------

||| Unified parameter type for all algorithms
public export
data HashParams
  = MkArgon2 Argon2Params
  | MkBcrypt BcryptParams
  | MkScrypt ScryptParams
  | MkPBKDF2 PBKDF2Params

public export
Show HashParams where
  show (MkArgon2 p) = show p
  show (MkBcrypt p) = show p
  show (MkScrypt p) = show p
  show (MkPBKDF2 p) = show p

||| Get algorithm from params
public export
paramsAlgorithm : HashParams -> HashAlgorithm
paramsAlgorithm (MkArgon2 _) = Argon2id
paramsAlgorithm (MkBcrypt _) = Bcrypt
paramsAlgorithm (MkScrypt _) = Scrypt
paramsAlgorithm (MkPBKDF2 _) = PBKDF2_SHA256

||| Check if new params are at least as strong as old
public export
paramsAtLeast : HashParams -> HashParams -> Bool
paramsAtLeast (MkArgon2 old) (MkArgon2 new) =
  old.timeCost >= new.timeCost &&
  old.memoryCost >= new.memoryCost &&
  old.parallelism >= new.parallelism
paramsAtLeast (MkBcrypt old) (MkBcrypt new) =
  old.cost >= new.cost
paramsAtLeast (MkScrypt old) (MkScrypt new) =
  old.n >= new.n && old.r >= new.r
paramsAtLeast (MkPBKDF2 old) (MkPBKDF2 new) =
  old.iterations >= new.iterations
paramsAtLeast _ _ = False  -- Different algorithms

--------------------------------------------------------------------------------
-- Hash Format Types
--------------------------------------------------------------------------------

||| PHC string format components
public export
record PHCFormat where
  constructor MkPHCFormat
  algorithm : String
  version : Maybe Nat
  params : List (String, String)
  salt : String
  hash : String

||| Parse PHC format string (e.g., $argon2id$v=19$m=65536,t=3,p=4$salt$hash)
public export
parsePHCFormat : String -> Maybe PHCFormat
parsePHCFormat s =
  -- Stub - actual implementation would parse the PHC string format
  Nothing

||| Encode to PHC format string
public export
encodePHCFormat : PHCFormat -> String
encodePHCFormat phc =
  "$" ++ phc.algorithm ++
  maybe "" (\v => "$v=" ++ show v) phc.version ++
  "$" ++ intercalate "," (map (\(k,v) => k ++ "=" ++ v) phc.params) ++
  "$" ++ phc.salt ++
  "$" ++ phc.hash

--------------------------------------------------------------------------------
-- Hash Result Types
--------------------------------------------------------------------------------

||| Raw hash output
public export
record HashOutput where
  constructor MkHashOutput
  hash : List Bits8
  salt : List Bits8
  params : HashParams

||| Encode hash output to storable string
public export
encodeHash : HashOutput -> String
encodeHash output =
  -- Stub - actual implementation would use proper encoding
  ""

||| Decode hash string back to components
public export
decodeHash : String -> Maybe HashOutput
decodeHash s =
  -- Stub - actual implementation would parse the encoded format
  Nothing

--------------------------------------------------------------------------------
-- Timing Attack Protection
--------------------------------------------------------------------------------

||| Compare two hashes in constant time
public export
constantTimeHashCompare : List Bits8 -> List Bits8 -> Bool
constantTimeHashCompare xs ys =
  if length xs /= length ys
    then False
    else go xs ys 0
  where
    go : List Bits8 -> List Bits8 -> Bits8 -> Bool
    go [] [] acc = acc == 0
    go (x :: xs') (y :: ys') acc = go xs' ys' (acc `or` (x `xor` y))
    go _ _ _ = False

||| Constant-time string hash comparison
public export
constantTimeStringCompare : String -> String -> Bool
constantTimeStringCompare s1 s2 =
  constantTimeHashCompare (map (cast . ord) (unpack s1))
                          (map (cast . ord) (unpack s2))
