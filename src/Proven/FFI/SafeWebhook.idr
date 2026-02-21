-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeWebhook operations
|||
||| This module exports webhook verification operations to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Verification -> (status: Int, result: String)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeWebhook

import Proven.SafeWebhook

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode VerifyResult as Int
encodeVerifyResult : VerifyResult -> Int
encodeVerifyResult Verified = 0
encodeVerifyResult InvalidSignature = 1
encodeVerifyResult ExpiredTimestamp = 2
encodeVerifyResult DuplicateDelivery = 3
encodeVerifyResult MissingSignature = 4
encodeVerifyResult UnsupportedAlgorithm = 5

||| Encode SignatureAlgorithm as Int
encodeSigAlg : SignatureAlgorithm -> Int
encodeSigAlg HMAC_SHA256 = 0
encodeSigAlg HMAC_SHA512 = 1
encodeSigAlg Ed25519Sig = 2

--------------------------------------------------------------------------------
-- Timestamp Validation
--------------------------------------------------------------------------------

export
proven_idris_webhook_is_timestamp_valid : Int -> Int -> Int
proven_idris_webhook_is_timestamp_valid eventTime currentTime =
  encodeBool (isTimestampValid (cast eventTime) (cast currentTime))

export
proven_idris_webhook_max_age : Int
proven_idris_webhook_max_age = cast maxWebhookAge

--------------------------------------------------------------------------------
-- Constant-Time Comparison
--------------------------------------------------------------------------------

export
proven_idris_webhook_constant_time_eq : String -> String -> Int
proven_idris_webhook_constant_time_eq a b =
  encodeBool (constantTimeEq a b)

--------------------------------------------------------------------------------
-- Signature Parsing
--------------------------------------------------------------------------------

export
proven_idris_webhook_parse_signature_header : String -> (Int, String)
proven_idris_webhook_parse_signature_header s =
  case parseSignatureHeader s of
    Nothing => (1, "Unsupported signature format")
    Just sig => (0, show (sigAlgorithm sig) ++ ":" ++ sigValue sig)

export
proven_idris_webhook_signature_algorithm : String -> Int
proven_idris_webhook_signature_algorithm s =
  case parseSignatureHeader s of
    Nothing => -1
    Just sig => encodeSigAlg (sigAlgorithm sig)

--------------------------------------------------------------------------------
-- Signature Verification (Structure)
--------------------------------------------------------------------------------

export
proven_idris_webhook_verify_signature : String -> String -> String -> Int
proven_idris_webhook_verify_signature secret payload sigHeader =
  case parseSignatureHeader sigHeader of
    Nothing => 0
    Just sig => encodeBool (verifySignature secret payload sig)

--------------------------------------------------------------------------------
-- Algorithm Info
--------------------------------------------------------------------------------

export
proven_idris_webhook_algorithm_name : Int -> String
proven_idris_webhook_algorithm_name 0 = show HMAC_SHA256
proven_idris_webhook_algorithm_name 1 = show HMAC_SHA512
proven_idris_webhook_algorithm_name 2 = show Ed25519Sig
proven_idris_webhook_algorithm_name _ = "unknown"
