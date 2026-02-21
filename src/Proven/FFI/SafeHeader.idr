-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeHeader operations
|||
||| This module exports safe HTTP header handling to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent header injection attacks (CRLF).
|||
||| Return conventions:
||| - HeaderResult Header → (status: Int, headerLine/error: String)
|||   - status = 0: Success, headerLine is "Name: Value"
|||   - status = 1: Error, headerLine contains error message
||| - HeaderResult Headers → (status: Int, count: Int, error: String)
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeHeader

import Proven.SafeHeader
import Proven.SafeHeader.Types
import Proven.SafeHeader.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode HeaderResult Header as (status, headerLine or error)
encodeHeaderResult : HeaderResult Header -> (Int, String)
encodeHeaderResult (Err err) = (1, friendlyError err)
encodeHeaderResult (Ok header) = (0, renderHeader header)

||| Encode HeaderResult Headers as (status, count, renderedHeaders or error)
encodeHeadersResult : HeaderResult Headers -> (Int, Int, String)
encodeHeadersResult (Err err) = (1, 0, friendlyError err)
encodeHeadersResult (Ok headers) = (0, cast (length headers), renderHeaders headers)

||| Decode Int to Nat (clamp negative)
decodeNat : Int -> Nat
decodeNat n = if n < 0 then Z else fromInteger (cast n)

--------------------------------------------------------------------------------
-- Header Creation
--------------------------------------------------------------------------------

export
proven_idris_header_make : String -> String -> (Int, String)
proven_idris_header_make name value =
  encodeHeaderResult (mkHeaderDefault name value)

export
proven_idris_header_parse_line : String -> (Int, String)
proven_idris_header_parse_line line =
  encodeHeaderResult (parseHeaderLine defaultOptions line)

--------------------------------------------------------------------------------
-- Header Parsing
--------------------------------------------------------------------------------

export
proven_idris_header_parse_raw : String -> (Int, Int, String)
proven_idris_header_parse_raw raw =
  encodeHeadersResult (parseHeaderString defaultOptions raw)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

export
proven_idris_header_render_http : String -> String
proven_idris_header_render_http headersStr =
  case parseHeaderString defaultOptions headersStr of
    Err _ => ""
    Ok headers => renderHeadersHTTP headers

--------------------------------------------------------------------------------
-- Security Header Builders
--------------------------------------------------------------------------------

export
proven_idris_header_strict_csp : String
proven_idris_header_strict_csp = strictCSP

export
proven_idris_header_standard_hsts : String
proven_idris_header_standard_hsts = standardHSTS

export
proven_idris_header_hsts : Int -> Int -> Int -> String
proven_idris_header_hsts maxAge includeSubdomains preload =
  buildHSTS (decodeNat maxAge) (includeSubdomains /= 0) (preload /= 0)

export
proven_idris_header_no_frame : String
proven_idris_header_no_frame = noFrame

export
proven_idris_header_no_sniff : String
proven_idris_header_no_sniff = noSniff

export
proven_idris_header_strict_referrer : String
proven_idris_header_strict_referrer = strictReferrer

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

||| Encode error type as Int for checking
encodeErrorType : HeaderError -> Int
encodeErrorType (HeaderInjection _ _) = 1  -- Injection
encodeErrorType (NameTooLong _ _) = 2       -- Size error
encodeErrorType (ValueTooLong _ _) = 2      -- Size error
encodeErrorType (TotalSizeTooLarge _) = 2   -- Size error
encodeErrorType _ = 0                        -- Other

export
proven_idris_header_is_injection_error : String -> Int
proven_idris_header_is_injection_error errorMsg =
  -- Simple heuristic: check if error message contains "injection"
  if isInfixOf "injection" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_header_is_size_error : String -> Int
proven_idris_header_is_size_error errorMsg =
  -- Simple heuristic: check if error message contains size-related keywords
  let lowerMsg = toLower errorMsg
  in if isInfixOf "too long" lowerMsg || isInfixOf "too large" lowerMsg
       then 1
       else 0

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_header_max_name_length : Int
proven_idris_header_max_name_length = cast maxHeaderNameLength

export
proven_idris_header_max_value_length : Int
proven_idris_header_max_value_length = cast maxHeaderValueLength

export
proven_idris_header_max_total_size : Int
proven_idris_header_max_total_size = cast maxTotalHeaderSize
