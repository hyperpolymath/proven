-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeContentType operations
|||
||| This module exports safe Content-Type/MIME handling to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent MIME sniffing attacks.
|||
||| Return conventions:
||| - ContentTypeResult → (status: Int, rendered/error: String)
|||   - status = 0: Success, rendered is "type/subtype; params"
|||   - status = 1: Error, rendered contains error message
||| - Bool → Int (0 = false, 1 = true)
module Proven.FFI.SafeContentType

import Proven.SafeContentType
import Proven.SafeContentType.Types
import Proven.SafeContentType.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode ContentTypeResult as (status, rendered or error)
encodeContentTypeResult : ContentTypeResult ContentType -> (Int, String)
encodeContentTypeResult (Err err) = (1, friendlyError err)
encodeContentTypeResult (Ok ct) = (0, renderContentType ct)

||| Encode Maybe String as (status, value)
encodeStringResult : Maybe String -> (Int, String)
encodeStringResult Nothing = (1, "")
encodeStringResult (Just s) = (0, s)

--------------------------------------------------------------------------------
-- Content-Type Parsing
--------------------------------------------------------------------------------

%export
proven_idris_content_type_parse : String -> (Int, String)
proven_idris_content_type_parse header =
  encodeContentTypeResult (parseContentTypeDefault header)

%export
proven_idris_content_type_create : String -> String -> (Int, String)
proven_idris_content_type_create mediaType subtype =
  encodeContentTypeResult (mkContentTypeDefault mediaType subtype)

--------------------------------------------------------------------------------
-- Common Content Types (Presets)
--------------------------------------------------------------------------------

%export
proven_idris_content_type_json : String
proven_idris_content_type_json = renderContentType json

%export
proven_idris_content_type_html : String
proven_idris_content_type_html = renderContentType html

%export
proven_idris_content_type_xml : String
proven_idris_content_type_xml = renderContentType xml

%export
proven_idris_content_type_plain : String
proven_idris_content_type_plain = renderContentType plain

%export
proven_idris_content_type_css : String
proven_idris_content_type_css = renderContentType css

%export
proven_idris_content_type_javascript : String
proven_idris_content_type_javascript = renderContentType javascript

%export
proven_idris_content_type_binary : String
proven_idris_content_type_binary = renderContentType binary

%export
proven_idris_content_type_form : String
proven_idris_content_type_form = renderContentType form

%export
proven_idris_content_type_pdf : String
proven_idris_content_type_pdf = renderContentType pdf

%export
proven_idris_content_type_png : String
proven_idris_content_type_png = renderContentType png

%export
proven_idris_content_type_jpeg : String
proven_idris_content_type_jpeg = renderContentType jpeg

%export
proven_idris_content_type_svg : String
proven_idris_content_type_svg = renderContentType svg

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

%export
proven_idris_content_type_is_text : String -> Int
proven_idris_content_type_is_text header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isText ct)

%export
proven_idris_content_type_is_binary : String -> Int
proven_idris_content_type_is_binary header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isBinary ct)

%export
proven_idris_content_type_is_json : String -> Int
proven_idris_content_type_is_json header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isJson ct)

%export
proven_idris_content_type_is_xml : String -> Int
proven_idris_content_type_is_xml header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isXml ct)

%export
proven_idris_content_type_is_html : String -> Int
proven_idris_content_type_is_html header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isHtml ct)

%export
proven_idris_content_type_is_multipart : String -> Int
proven_idris_content_type_is_multipart header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (isMultipart ct)

%export
proven_idris_content_type_matches : String -> String -> Int
proven_idris_content_type_matches header pattern =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (matches ct pattern)

--------------------------------------------------------------------------------
-- Extension Mapping
--------------------------------------------------------------------------------

%export
proven_idris_content_type_from_extension : String -> (Int, String)
proven_idris_content_type_from_extension ext =
  case extensionToContentType ext of
    Nothing => (1, "")
    Just ct => (0, renderContentType ct)

%export
proven_idris_content_type_to_extension : String -> (Int, String)
proven_idris_content_type_to_extension header =
  case parseContentTypeDefault header of
    Err _ => (1, "")
    Ok ct => encodeStringResult (contentTypeToExtension ct)

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

%export
proven_idris_content_type_safe_from_sniffing : String -> Int
proven_idris_content_type_safe_from_sniffing header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (safeFromSniffing ct)

%export
proven_idris_content_type_secure_charset : String -> Int
proven_idris_content_type_secure_charset header =
  case parseContentTypeDefault header of
    Err _ => 0
    Ok ct => encodeBool (secureCharset ct)

%export
proven_idris_content_type_nosniff : String
proven_idris_content_type_nosniff = nosniff

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

%export
proven_idris_content_type_is_format_error : String -> Int
proven_idris_content_type_is_format_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg) || isInfixOf "empty" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_content_type_is_size_error : String -> Int
proven_idris_content_type_is_size_error errorMsg =
  if isInfixOf "too long" (toLower errorMsg)
    then 1
    else 0

%export
proven_idris_content_type_is_sniffing_error : String -> Int
proven_idris_content_type_is_sniffing_error errorMsg =
  if isInfixOf "sniffing" (toLower errorMsg)
    then 1
    else 0
