-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeSSH operations
|||
||| This module exports SSH key validation and algorithm checks to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Algorithm info -> Int (encoded algorithm/strength IDs)
||| - Key parsing -> (status: Int, value: String)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeSSH

import Proven.SafeSSH

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode AlgorithmStrength as Int
encodeStrength : AlgorithmStrength -> Int
encodeStrength Broken = 0
encodeStrength Weak = 1
encodeStrength Acceptable = 2
encodeStrength Strong = 3
encodeStrength Excellent = 4

||| Encode SSHAlgorithm as Int
encodeAlgorithm : SSHAlgorithm -> Int
encodeAlgorithm RSA = 0
encodeAlgorithm DSA = 1
encodeAlgorithm ECDSA_256 = 2
encodeAlgorithm ECDSA_384 = 3
encodeAlgorithm ECDSA_521 = 4
encodeAlgorithm Ed25519 = 5
encodeAlgorithm Ed448 = 6
encodeAlgorithm SK_ECDSA_256 = 7
encodeAlgorithm SK_Ed25519 = 8

||| Encode KeyValidity as Int
encodeValidity : KeyValidity -> Int
encodeValidity Valid = 0
encodeValidity WeakAlgorithm = 1
encodeValidity InvalidFormat = 2
encodeValidity TooShort = 3

--------------------------------------------------------------------------------
-- Algorithm Operations
--------------------------------------------------------------------------------

export
proven_idris_ssh_parse_algorithm : String -> (Int, String)
proven_idris_ssh_parse_algorithm s = case parseAlgorithm s of
  Nothing => (1, "Unknown algorithm")
  Just alg => (0, show alg)

export
proven_idris_ssh_algorithm_strength : String -> Int
proven_idris_ssh_algorithm_strength s = case parseAlgorithm s of
  Nothing => 0  -- Broken/unknown
  Just alg => encodeStrength (algorithmStrength alg)

export
proven_idris_ssh_is_weak_algorithm : String -> Int
proven_idris_ssh_is_weak_algorithm s = case parseAlgorithm s of
  Nothing => 1  -- Unknown is weak
  Just alg => encodeBool (isWeakAlgorithm alg)

--------------------------------------------------------------------------------
-- Key Parsing and Validation
--------------------------------------------------------------------------------

export
proven_idris_ssh_parse_public_key : String -> (Int, String)
proven_idris_ssh_parse_public_key s = case parsePublicKey s of
  Nothing => (1, "Invalid SSH public key format")
  Just key => (0, show (keyAlgorithm key))

export
proven_idris_ssh_validate_key : String -> Int
proven_idris_ssh_validate_key s = case parsePublicKey s of
  Nothing => 2  -- InvalidFormat
  Just key => encodeValidity (validateKey key)

export
proven_idris_ssh_key_algorithm : String -> (Int, String)
proven_idris_ssh_key_algorithm s = case parsePublicKey s of
  Nothing => (1, "Invalid key")
  Just key => (0, show (keyAlgorithm key))

export
proven_idris_ssh_key_comment : String -> (Int, String)
proven_idris_ssh_key_comment s = case parsePublicKey s of
  Nothing => (1, "Invalid key")
  Just key => (0, keyComment key)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

export
proven_idris_ssh_min_rsa_key_bits : Int
proven_idris_ssh_min_rsa_key_bits = cast minRSAKeyBits

export
proven_idris_ssh_recommended_rsa_key_bits : Int
proven_idris_ssh_recommended_rsa_key_bits = cast recommendedRSAKeyBits

export
proven_idris_ssh_recommended_algorithms : String
proven_idris_ssh_recommended_algorithms =
  allowedAlgorithmsConfig recommendedAlgorithms
