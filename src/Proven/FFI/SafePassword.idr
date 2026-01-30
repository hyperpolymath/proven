-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafePassword operations
|||
||| This module exports password validation and hashing to the C ABI via Idris2's RefC backend.
||| All functions are proven total and use secure hashing algorithms.
|||
||| Return conventions:
||| - Validation → (status: Int, error: String)
|||   - status = 0: Valid
|||   - status = 1: Invalid, error contains violation message
||| - Hash algorithm → Int (algorithm ID)
||| - Entropy/metrics → Double
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Always hash passwords with Argon2id or bcrypt. Never store plaintext passwords.
module Proven.FFI.SafePassword

import Proven.SafePassword
import Proven.SafePassword.Policy
import Proven.SafePassword.Hash
import Proven.SafePassword.Strength
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode HashAlgorithm as Int
encodeHashAlgorithm : HashAlgorithm -> Int
encodeHashAlgorithm Argon2id = 0
encodeHashAlgorithm Argon2i = 1
encodeHashAlgorithm Argon2d = 2
encodeHashAlgorithm Bcrypt = 3
encodeHashAlgorithm Scrypt = 4
encodeHashAlgorithm PBKDF2_SHA256 = 5
encodeHashAlgorithm PBKDF2_SHA512 = 6

||| Decode Int to HashAlgorithm
decodeHashAlgorithm : Int -> Maybe HashAlgorithm
decodeHashAlgorithm 0 = Just Argon2id
decodeHashAlgorithm 1 = Just Argon2i
decodeHashAlgorithm 2 = Just Argon2d
decodeHashAlgorithm 3 = Just Bcrypt
decodeHashAlgorithm 4 = Just Scrypt
decodeHashAlgorithm 5 = Just PBKDF2_SHA256
decodeHashAlgorithm 6 = Just PBKDF2_SHA512
decodeHashAlgorithm _ = Nothing

||| Encode policy violations as (status, error)
encodeValidationResult : List PolicyViolation -> (Int, String)
encodeValidationResult [] = (0, "")
encodeValidationResult violations = (1, unlines (map show violations))

||| Encode VerifyResult as Int
encodeVerifyResult : VerifyResult -> Int
encodeVerifyResult Verified = 0
encodeVerifyResult NotVerified = 1
encodeVerifyResult NeedsRehash = 2
encodeVerifyResult HashCorrupted = 3

--------------------------------------------------------------------------------
-- Hash Algorithm Constants
--------------------------------------------------------------------------------

export
proven_idris_password_hash_argon2id : Int
proven_idris_password_hash_argon2id = encodeHashAlgorithm Argon2id

export
proven_idris_password_hash_argon2i : Int
proven_idris_password_hash_argon2i = encodeHashAlgorithm Argon2i

export
proven_idris_password_hash_bcrypt : Int
proven_idris_password_hash_bcrypt = encodeHashAlgorithm Bcrypt

export
proven_idris_password_hash_scrypt : Int
proven_idris_password_hash_scrypt = encodeHashAlgorithm Scrypt

export
proven_idris_password_hash_name : Int -> String
proven_idris_password_hash_name algInt =
  case decodeHashAlgorithm algInt of
    Nothing => "Unknown"
    Just alg => show alg

export
proven_idris_password_hash_is_recommended : Int -> Int
proven_idris_password_hash_is_recommended algInt =
  case decodeHashAlgorithm algInt of
    Nothing => 0
    Just alg => encodeBool (isRecommended alg)

--------------------------------------------------------------------------------
-- Password Policy Validation
--------------------------------------------------------------------------------

export
proven_idris_password_validate_default : String -> (Int, String)
proven_idris_password_validate_default pwd =
  encodeValidationResult (checkPolicy defaultPolicy pwd)

export
proven_idris_password_validate_strict : String -> (Int, String)
proven_idris_password_validate_strict pwd =
  encodeValidationResult (checkPolicy strictPolicy pwd)

export
proven_idris_password_validate_passphrase : String -> (Int, String)
proven_idris_password_validate_passphrase pwd =
  encodeValidationResult (checkPolicy passphrasePolicy pwd)

export
proven_idris_password_is_valid_default : String -> Int
proven_idris_password_is_valid_default pwd =
  encodeBool (isValidPassword defaultPolicy pwd)

export
proven_idris_password_is_valid_strict : String -> Int
proven_idris_password_is_valid_strict pwd =
  encodeBool (isValidPassword strictPolicy pwd)

--------------------------------------------------------------------------------
-- Pattern Detection
--------------------------------------------------------------------------------

export
proven_idris_password_contains_username : String -> String -> Int
proven_idris_password_contains_username pwd username =
  encodeBool (containsUsername pwd username)

export
proven_idris_password_is_common_pattern : String -> Int
proven_idris_password_is_common_pattern pwd =
  encodeBool (isCommonPattern pwd)

--------------------------------------------------------------------------------
-- Entropy Calculation
--------------------------------------------------------------------------------

export
proven_idris_password_calculate_entropy : String -> Double
proven_idris_password_calculate_entropy = calculateEntropy

export
proven_idris_password_entropy_low : Double
proven_idris_password_entropy_low = entropyBits LowSecurity

export
proven_idris_password_entropy_medium : Double
proven_idris_password_entropy_medium = entropyBits MediumSecurity

export
proven_idris_password_entropy_high : Double
proven_idris_password_entropy_high = entropyBits HighSecurity

export
proven_idris_password_entropy_critical : Double
proven_idris_password_entropy_critical = entropyBits CriticalSecurity

export
proven_idris_password_meets_entropy_low : String -> Int
proven_idris_password_meets_entropy_low pwd =
  encodeBool (meetsEntropyRequirement pwd LowSecurity)

export
proven_idris_password_meets_entropy_medium : String -> Int
proven_idris_password_meets_entropy_medium pwd =
  encodeBool (meetsEntropyRequirement pwd MediumSecurity)

export
proven_idris_password_meets_entropy_high : String -> Int
proven_idris_password_meets_entropy_high pwd =
  encodeBool (meetsEntropyRequirement pwd HighSecurity)

export
proven_idris_password_meets_entropy_critical : String -> Int
proven_idris_password_meets_entropy_critical pwd =
  encodeBool (meetsEntropyRequirement pwd CriticalSecurity)

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

export
proven_idris_password_mask : String -> String
proven_idris_password_mask = maskPassword

--------------------------------------------------------------------------------
-- Default Argon2id Parameters
--------------------------------------------------------------------------------

export
proven_idris_password_argon2_default_time_cost : Int
proven_idris_password_argon2_default_time_cost = cast defaultArgon2Params.timeCost

export
proven_idris_password_argon2_default_memory_cost : Int
proven_idris_password_argon2_default_memory_cost = cast defaultArgon2Params.memoryCost

export
proven_idris_password_argon2_default_parallelism : Int
proven_idris_password_argon2_default_parallelism = cast defaultArgon2Params.parallelism

export
proven_idris_password_argon2_default_hash_length : Int
proven_idris_password_argon2_default_hash_length = cast defaultArgon2Params.hashLength

export
proven_idris_password_argon2_default_salt_length : Int
proven_idris_password_argon2_default_salt_length = cast defaultArgon2Params.saltLength

--------------------------------------------------------------------------------
-- High Security Argon2id Parameters
--------------------------------------------------------------------------------

export
proven_idris_password_argon2_high_time_cost : Int
proven_idris_password_argon2_high_time_cost = cast highSecurityArgon2Params.timeCost

export
proven_idris_password_argon2_high_memory_cost : Int
proven_idris_password_argon2_high_memory_cost = cast highSecurityArgon2Params.memoryCost

export
proven_idris_password_argon2_high_parallelism : Int
proven_idris_password_argon2_high_parallelism = cast highSecurityArgon2Params.parallelism

--------------------------------------------------------------------------------
-- Default Bcrypt Parameters
--------------------------------------------------------------------------------

export
proven_idris_password_bcrypt_default_cost : Int
proven_idris_password_bcrypt_default_cost = cast defaultBcryptParams.cost

export
proven_idris_password_bcrypt_high_cost : Int
proven_idris_password_bcrypt_high_cost = cast highSecurityBcryptParams.cost

--------------------------------------------------------------------------------
-- Policy Constants
--------------------------------------------------------------------------------

export
proven_idris_password_default_min_length : Int
proven_idris_password_default_min_length = cast defaultPolicy.minLength

export
proven_idris_password_default_max_length : Int
proven_idris_password_default_max_length = cast defaultPolicy.maxLength

export
proven_idris_password_strict_min_length : Int
proven_idris_password_strict_min_length = cast strictPolicy.minLength

export
proven_idris_password_passphrase_min_length : Int
proven_idris_password_passphrase_min_length = cast passphrasePolicy.minLength

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_password_is_length_error : String -> Int
proven_idris_password_is_length_error errorMsg =
  if isInfixOf "too short" (toLower errorMsg) || isInfixOf "too long" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_password_is_complexity_error : String -> Int
proven_idris_password_is_complexity_error errorMsg =
  if isInfixOf "uppercase" (toLower errorMsg) ||
     isInfixOf "lowercase" (toLower errorMsg) ||
     isInfixOf "digit" (toLower errorMsg) ||
     isInfixOf "symbol" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_password_is_pattern_error : String -> Int
proven_idris_password_is_pattern_error errorMsg =
  if isInfixOf "pattern" (toLower errorMsg) || isInfixOf "repeated" (toLower errorMsg)
    then 1
    else 0
