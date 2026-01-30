-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeHtml operations
|||
||| This module exports HTML escaping and XSS prevention to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent Cross-Site Scripting attacks.
|||
||| Return conventions:
||| - Escaped strings → String (always safe for HTML)
||| - Maybe String → (status: Int, value: String)
|||   - status = 0: Success, URL is safe
|||   - status = 1: Error (dangerous scheme detected)
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeHtml

import Proven.SafeHtml
import Proven.SafeHtml.Escape
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Maybe String as (status, value)
encodeStringResult : Maybe String -> (Int, String)
encodeStringResult Nothing = (1, "")
encodeStringResult (Just s) = (0, s)

||| Decode Int to char code
decodeChar : Int -> Char
decodeChar code = chr code

--------------------------------------------------------------------------------
-- Core HTML Escaping (XSS Prevention)
--------------------------------------------------------------------------------

export
proven_idris_html_escape_content : String -> String
proven_idris_html_escape_content s =
  escapeHtmlContent s

export
proven_idris_html_escape_attr : String -> String
proven_idris_html_escape_attr s =
  escapeHtmlAttr s

export
proven_idris_html_escape_inline_script : String -> String
proven_idris_html_escape_inline_script s =
  escapeInlineScript s

export
proven_idris_html_escape_inline_css : String -> String
proven_idris_html_escape_inline_css s =
  escapeInlineCss s

export
proven_idris_html_escape_comment : String -> String
proven_idris_html_escape_comment s =
  escapeHtmlComment s

export
proven_idris_html_escape_event_handler : String -> String
proven_idris_html_escape_event_handler s =
  escapeEventHandler s

export
proven_idris_html_escape_non_ascii : String -> String
proven_idris_html_escape_non_ascii s =
  escapeNonAscii s

--------------------------------------------------------------------------------
-- URL Sanitization
--------------------------------------------------------------------------------

export
proven_idris_html_has_dangerous_scheme : String -> Int
proven_idris_html_has_dangerous_scheme url =
  encodeBool (hasDangerousScheme url)

export
proven_idris_html_sanitize_url : String -> (Int, String)
proven_idris_html_sanitize_url url =
  encodeStringResult (sanitizeUrl url)

export
proven_idris_html_escape_url_attr : String -> (Int, String)
proven_idris_html_escape_url_attr url =
  encodeStringResult (escapeUrlAttr url)

--------------------------------------------------------------------------------
-- Attribute Validation
--------------------------------------------------------------------------------

export
proven_idris_html_is_valid_attr_name : String -> Int
proven_idris_html_is_valid_attr_name name =
  encodeBool (isValidAttrName name)

export
proven_idris_html_is_event_handler : String -> Int
proven_idris_html_is_event_handler name =
  encodeBool (isEventHandler name)

--------------------------------------------------------------------------------
-- Tag Validation
--------------------------------------------------------------------------------

export
proven_idris_html_is_void_tag : String -> Int
proven_idris_html_is_void_tag tag =
  encodeBool (isVoidTag tag)

export
proven_idris_html_is_dangerous_tag : String -> Int
proven_idris_html_is_dangerous_tag tag =
  encodeBool (isDangerousTag tag)

--------------------------------------------------------------------------------
-- Character Entity Conversion
--------------------------------------------------------------------------------

export
proven_idris_html_char_to_numeric_entity : Int -> String
proven_idris_html_char_to_numeric_entity charCode =
  toNumericEntity (decodeChar charCode)

export
proven_idris_html_char_to_hex_entity : Int -> String
proven_idris_html_char_to_hex_entity charCode =
  toHexEntity (decodeChar charCode)
