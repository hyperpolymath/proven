-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeJWT operations
|||
||| This module exports safe JWT handling to the C ABI via Idris2's RefC backend.
||| All validation functions are proven total and prevent timing attacks.
|||
||| Return conventions:
||| - Result ValidatedJWT → (status: Int, claims/error: String)
|||   - status = 0: Success, claims is JSON string
|||   - status = 1: Error, claims contains error message
||| - Bool → Int (0 = false, 1 = true)
|||
||| NOTE: Token creation (signing) requires crypto library integration.
||| Current implementation uses stubs for demonstration.
module Proven.FFI.SafeJWT

import Proven.SafeJWT
import Proven.SafeJWT.Types
import Proven.SafeJWT.Decode
import Proven.SafeJWT.Validate
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Result ValidatedJWT as (status, claims or error)
encodeJWTResult : Result JWTError ValidatedJWT -> (Int, String)
encodeJWTResult (Err err) = (1, friendlyError err)
encodeJWTResult (Ok validated) =
  -- Return pretty-printed JWT info (without signature)
  (0, prettyPrintJWT validated.decoded)

||| Encode Result DecodedJWT as (status, info or error)
encodeDecodedResult : Result JWTError DecodedJWT -> (Int, String)
encodeDecodedResult (Err err) = (1, show err)
encodeDecodedResult (Ok decoded) = (0, prettyPrintJWT decoded)

||| Decode Integer from Int
decodeInteger : Int -> Integer
decodeInteger n = cast n

--------------------------------------------------------------------------------
-- JWT Validation
--------------------------------------------------------------------------------

export
proven_idris_jwt_verify : String -> Integer -> String -> (Int, String)
proven_idris_jwt_verify secret currentTime token =
  encodeJWTResult (verifyJWT secret currentTime token)

export
proven_idris_jwt_is_valid : String -> Integer -> String -> Int
proven_idris_jwt_is_valid secret currentTime token =
  encodeBool (isTokenValid secret currentTime token)

--------------------------------------------------------------------------------
-- JWT Decoding (Inspection)
--------------------------------------------------------------------------------

export
proven_idris_jwt_decode : String -> (Int, String)
proven_idris_jwt_decode token =
  encodeDecodedResult (inspectJWT token)

export
proven_idris_jwt_token_info : String -> (Int, String)
proven_idris_jwt_token_info token =
  case tokenInfo token of
    Err err => (1, show err)
    Ok info => (0, info)

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_jwt_is_expired_error : String -> Int
proven_idris_jwt_is_expired_error errorMsg =
  if isInfixOf "expired" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_jwt_is_signature_error : String -> Int
proven_idris_jwt_is_signature_error errorMsg =
  if isInfixOf "signature" (toLower errorMsg) || isInfixOf "invalid token" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_jwt_is_algorithm_error : String -> Int
proven_idris_jwt_is_algorithm_error errorMsg =
  if isInfixOf "algorithm" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Validation Presets (Constants)
--------------------------------------------------------------------------------

export
proven_idris_jwt_access_token_max_age : Integer
proven_idris_jwt_access_token_max_age = 3600  -- 1 hour

export
proven_idris_jwt_refresh_token_max_age : Integer
proven_idris_jwt_refresh_token_max_age = 604800  -- 1 week

export
proven_idris_jwt_id_token_max_age : Integer
proven_idris_jwt_id_token_max_age = 3600  -- 1 hour

--------------------------------------------------------------------------------
-- Token Lifecycle Helpers
--------------------------------------------------------------------------------

||| Helper to parse current time and check validity
parseAndCheckValidity : Integer -> String -> Maybe Integer
parseAndCheckValidity currentTime tokenStr =
  case inspectJWT tokenStr of
    Err _ => Nothing
    Ok decoded => timeUntilExpiration currentTime (MkValidatedJWT decoded (SecretKey []))

export
proven_idris_jwt_remaining_validity : Integer -> String -> (Int, Integer)
proven_idris_jwt_remaining_validity currentTime token =
  case parseAndCheckValidity currentTime token of
    Nothing => (1, 0)  -- Error
    Just remaining => (0, remaining)

export
proven_idris_jwt_needs_refresh : Integer -> Int -> String -> Int
proven_idris_jwt_needs_refresh currentTime thresholdSeconds token =
  case parseAndCheckValidity currentTime token of
    Nothing => 0  -- Invalid token doesn't need refresh
    Just remaining =>
      if remaining <= decodeInteger thresholdSeconds
        then 1  -- Needs refresh
        else 0  -- Still valid

--------------------------------------------------------------------------------
-- Security Helpers
--------------------------------------------------------------------------------

export
proven_idris_jwt_friendly_error : String -> String
proven_idris_jwt_friendly_error errorMsg =
  -- Simple mapping - in production would parse error type first
  if isInfixOf "expired" (toLower errorMsg)
    then "Your session has expired. Please log in again."
  else if isInfixOf "signature" (toLower errorMsg)
    then "Invalid token. Please log in again."
  else if isInfixOf "issuer" (toLower errorMsg)
    then "Token was not issued by a trusted source."
  else if isInfixOf "audience" (toLower errorMsg)
    then "Token was not intended for this application."
  else
    "Authentication failed. Please log in again."
