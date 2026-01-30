-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCrypto operations
|||
||| This module exports cryptographic operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and use constant-time comparisons where needed.
|||
||| Return conventions:
||| - RandomResult → (status: Int, value/error: String)
|||   - status = 0: Success, value contains result
|||   - status = 1: Error, value contains error message
||| - Hash algorithm info → Int (algorithm ID or size)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Use constant-time comparisons for security-sensitive data.
module Proven.FFI.SafeCrypto

import Proven.SafeCrypto
import Proven.SafeCrypto.Hash
import Proven.SafeCrypto.Random
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode HashAlg as Int
encodeHashAlg : HashAlg -> Int
encodeHashAlg MD5_ALG = 0
encodeHashAlg SHA1_ALG = 1
encodeHashAlg SHA224_ALG = 2
encodeHashAlg SHA256_ALG = 3
encodeHashAlg SHA384_ALG = 4
encodeHashAlg SHA512_ALG = 5
encodeHashAlg SHA3_256_ALG = 6
encodeHashAlg SHA3_512_ALG = 7
encodeHashAlg BLAKE2b_ALG = 8
encodeHashAlg BLAKE2s_ALG = 9
encodeHashAlg BLAKE3_ALG = 10

||| Decode Int to HashAlg
decodeHashAlg : Int -> Maybe HashAlg
decodeHashAlg 0 = Just MD5_ALG
decodeHashAlg 1 = Just SHA1_ALG
decodeHashAlg 2 = Just SHA224_ALG
decodeHashAlg 3 = Just SHA256_ALG
decodeHashAlg 4 = Just SHA384_ALG
decodeHashAlg 5 = Just SHA512_ALG
decodeHashAlg 6 = Just SHA3_256_ALG
decodeHashAlg 7 = Just SHA3_512_ALG
decodeHashAlg 8 = Just BLAKE2b_ALG
decodeHashAlg 9 = Just BLAKE2s_ALG
decodeHashAlg 10 = Just BLAKE3_ALG
decodeHashAlg _ = Nothing

||| Encode SecurityLevel as Int
encodeSecurityLevel : SecurityLevel -> Int
encodeSecurityLevel Broken = 0
encodeSecurityLevel Deprecated = 1
encodeSecurityLevel Standard = 2
encodeSecurityLevel Modern = 3

||| Encode RandomResult String as (status, value/error)
encodeRandomResult : RandomResult String -> (Int, String)
encodeRandomResult (Left err) = (1, show err)
encodeRandomResult (Right val) = (0, val)

--------------------------------------------------------------------------------
-- Hash Algorithm Information
--------------------------------------------------------------------------------

export
proven_idris_crypto_hash_alg_md5 : Int
proven_idris_crypto_hash_alg_md5 = encodeHashAlg MD5_ALG

export
proven_idris_crypto_hash_alg_sha1 : Int
proven_idris_crypto_hash_alg_sha1 = encodeHashAlg SHA1_ALG

export
proven_idris_crypto_hash_alg_sha256 : Int
proven_idris_crypto_hash_alg_sha256 = encodeHashAlg SHA256_ALG

export
proven_idris_crypto_hash_alg_sha512 : Int
proven_idris_crypto_hash_alg_sha512 = encodeHashAlg SHA512_ALG

export
proven_idris_crypto_hash_alg_sha3_256 : Int
proven_idris_crypto_hash_alg_sha3_256 = encodeHashAlg SHA3_256_ALG

export
proven_idris_crypto_hash_alg_blake2b : Int
proven_idris_crypto_hash_alg_blake2b = encodeHashAlg BLAKE2b_ALG

export
proven_idris_crypto_hash_alg_blake3 : Int
proven_idris_crypto_hash_alg_blake3 = encodeHashAlg BLAKE3_ALG

export
proven_idris_crypto_hash_output_size : Int -> Int
proven_idris_crypto_hash_output_size algInt =
  case decodeHashAlg algInt of
    Nothing => 0
    Just alg => cast (hashOutputSize alg)

export
proven_idris_crypto_hash_block_size : Int -> Int
proven_idris_crypto_hash_block_size algInt =
  case decodeHashAlg algInt of
    Nothing => 0
    Just alg => cast (hashBlockSize alg)

export
proven_idris_crypto_hash_name : Int -> String
proven_idris_crypto_hash_name algInt =
  case decodeHashAlg algInt of
    Nothing => "Unknown"
    Just alg => show alg

--------------------------------------------------------------------------------
-- Security Level Checking
--------------------------------------------------------------------------------

export
proven_idris_crypto_security_level : Int -> Int
proven_idris_crypto_security_level algInt =
  case decodeHashAlg algInt of
    Nothing => 0  -- Broken
    Just alg => encodeSecurityLevel (securityLevel alg)

export
proven_idris_crypto_is_secure : Int -> Int
proven_idris_crypto_is_secure algInt =
  case decodeHashAlg algInt of
    Nothing => 0
    Just alg => encodeBool (isSecure alg)

export
proven_idris_crypto_is_broken : Int -> Int
proven_idris_crypto_is_broken algInt =
  case decodeHashAlg algInt of
    Nothing => 1
    Just alg => case securityLevel alg of
      Broken => 1
      _ => 0

export
proven_idris_crypto_is_deprecated : Int -> Int
proven_idris_crypto_is_deprecated algInt =
  case decodeHashAlg algInt of
    Nothing => 0
    Just alg => case securityLevel alg of
      Deprecated => 1
      _ => 0

--------------------------------------------------------------------------------
-- Byte Conversion Utilities
--------------------------------------------------------------------------------

export
proven_idris_crypto_hex_to_bytes : String -> (Int, String)
proven_idris_crypto_hex_to_bytes hex =
  case hexToBytes hex of
    Nothing => (1, "Invalid hex string")
    Just bytes => (0, bytesToHex bytes)  -- Return normalized hex

export
proven_idris_crypto_bytes_to_hex : String -> String
proven_idris_crypto_bytes_to_hex input =
  let bytes = map (cast . ord) (unpack input)
  in bytesToHex bytes

--------------------------------------------------------------------------------
-- Random Number Generation
--------------------------------------------------------------------------------

export
proven_idris_crypto_random_token : Int -> (Int, String)
proven_idris_crypto_random_token bytes =
  encodeRandomResult (randomToken (cast bytes))

export
proven_idris_crypto_random_hex : Int -> (Int, String)
proven_idris_crypto_random_hex bytes =
  encodeRandomResult (randomHex (cast bytes))

export
proven_idris_crypto_random_uuid : (Int, String)
proven_idris_crypto_random_uuid = encodeRandomResult randomUUID

export
proven_idris_crypto_random_int : Int -> Int -> (Int, String)
proven_idris_crypto_random_int min max =
  case randomInt (cast min) (cast max) of
    Left err => (1, show err)
    Right val => (0, show val)

export
proven_idris_crypto_random_bool : (Int, String)
proven_idris_crypto_random_bool =
  case randomBool of
    Left err => (1, show err)
    Right True => (0, "1")
    Right False => (0, "0")

--------------------------------------------------------------------------------
-- Constant-Time Operations
--------------------------------------------------------------------------------

export
proven_idris_crypto_constant_time_eq : String -> String -> Int
proven_idris_crypto_constant_time_eq s1 s2 =
  let bytes1 = map (cast . ord) (unpack s1)
      bytes2 = map (cast . ord) (unpack s2)
  in encodeBool (constantTimeEq bytes1 bytes2)

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_crypto_is_entropy_error : String -> Int
proven_idris_crypto_is_entropy_error errorMsg =
  if isInfixOf "entropy" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_crypto_is_key_error : String -> Int
proven_idris_crypto_is_key_error errorMsg =
  if isInfixOf "key" (toLower errorMsg) || isInfixOf "length" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_crypto_is_decryption_error : String -> Int
proven_idris_crypto_is_decryption_error errorMsg =
  if isInfixOf "decrypt" (toLower errorMsg) || isInfixOf "authentication" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Algorithm Recommendations
--------------------------------------------------------------------------------

export
proven_idris_crypto_recommended_hash : Int
proven_idris_crypto_recommended_hash = encodeHashAlg SHA256_ALG

export
proven_idris_crypto_modern_hash : Int
proven_idris_crypto_modern_hash = encodeHashAlg BLAKE3_ALG

export
proven_idris_crypto_min_token_bytes : Int
proven_idris_crypto_min_token_bytes = 32  -- 256 bits

export
proven_idris_crypto_default_nonce_size : Int
proven_idris_crypto_default_nonce_size = 12  -- Standard for AES-GCM

export
proven_idris_crypto_default_tag_size : Int
proven_idris_crypto_default_tag_size = 16  -- 128 bits
