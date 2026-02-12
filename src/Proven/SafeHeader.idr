-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeHeader - Safe HTTP header handling
|||
||| This module provides safe HTTP header operations including:
||| - Header name/value validation
||| - Injection prevention (CRLF)
||| - Size limits
||| - Security header builders
|||
||| Example usage:
||| ```idris
||| -- Create headers safely
||| case mkHeader defaultOptions "Content-Type" "application/json" of
|||   Ok header => addToResponse header
|||   Err (HeaderInjection _ _) => rejectRequest
|||   Err e => handleError e
|||
||| -- Build security headers
||| let csp = buildCSP [("default-src", ["'self'"]), ("script-src", ["'self'", "'unsafe-inline'"])]
||| let hsts = buildHSTS 31536000 True True
||| ```
module Proven.SafeHeader

import public Proven.Core
import public Proven.SafeHeader.Types
import public Proven.SafeHeader.Parser
import public Proven.SafeHeader.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Create a validated header
public export
header : String -> String -> HeaderResult Header
header = mkHeaderDefault

||| Create header with options
public export
headerWith : HeaderOptions -> String -> String -> HeaderResult Header
headerWith = mkHeader

||| Create header for well-known type
public export
wellKnown : WellKnownHeader -> String -> HeaderResult Header
wellKnown = mkWellKnownHeader defaultOptions

--------------------------------------------------------------------------------
-- Header Collection API
--------------------------------------------------------------------------------

||| Empty header collection
public export
empty : Headers
empty = []

||| Add header to collection
public export
add : Headers -> String -> String -> HeaderResult Headers
add = addHeaderDefault

||| Add header with options
public export
addWith : HeaderOptions -> Headers -> String -> String -> HeaderResult Headers
addWith = addHeader

||| Set header (replace existing)
public export
set : Headers -> String -> String -> HeaderResult Headers
set = setHeaderDefault

||| Get header value
public export
get : Headers -> String -> Maybe String
get = getHeader

||| Get all values for header
public export
getAll : Headers -> String -> List String
getAll = getHeaders

||| Check if header exists
public export
has : Headers -> String -> Bool
has = hasHeader

||| Remove header
public export
remove : Headers -> String -> Headers
remove = removeHeader

||| Merge header collections
public export
merge : Headers -> Headers -> Headers
merge = mergeHeaders

--------------------------------------------------------------------------------
-- Parsing API
--------------------------------------------------------------------------------

||| Parse header line (Name: Value)
public export
parseLine : String -> HeaderResult Header
parseLine = parseHeaderLine defaultOptions

||| Parse multiple header lines
public export
parseLines : List String -> HeaderResult Headers
parseLines = parseHeaders defaultOptions

||| Parse raw header string
public export
parseRaw : String -> HeaderResult Headers
parseRaw = parseHeaderString defaultOptions

--------------------------------------------------------------------------------
-- Rendering API
--------------------------------------------------------------------------------

||| Render header to string
public export
render : Header -> String
render = renderHeader

||| Render all headers
public export
renderAll : Headers -> String
renderAll = renderHeaders

||| Render headers for HTTP wire format
public export
renderHTTP : Headers -> String
renderHTTP = renderHeadersHTTP

--------------------------------------------------------------------------------
-- Size Checking
--------------------------------------------------------------------------------

||| Get total headers size
public export
size : Headers -> Nat
size = totalSize

||| Check if headers are within size limits
public export
checkSize : Headers -> HeaderResult ()
checkSize = checkTotalSize defaultOptions

||| Check with custom options
public export
checkSizeWith : HeaderOptions -> Headers -> HeaderResult ()
checkSizeWith = checkTotalSize

--------------------------------------------------------------------------------
-- Common Header Helpers
--------------------------------------------------------------------------------

||| Get Content-Length
public export
contentLength : Headers -> HeaderResult (Maybe Nat)
contentLength = parseContentLength

||| Get Content-Type
public export
contentType : Headers -> Maybe String
contentType h = get h "Content-Type"

||| Get Accept values with quality
public export
accept : Headers -> List (String, Double)
accept h = case get h "Accept" of
  Just v => parseAcceptHeader v
  Nothing => []

||| Get Cache-Control directives
public export
cacheControl : Headers -> List (String, Maybe String)
cacheControl h = case get h "Cache-Control" of
  Just v => parseCacheControl v
  Nothing => []

--------------------------------------------------------------------------------
-- Security Header Builders
--------------------------------------------------------------------------------

||| Build Content-Security-Policy
public export
csp : List (String, List String) -> String
csp = buildCSP

||| Build Strict-Transport-Security
public export
hsts : Nat -> Bool -> Bool -> String
hsts = buildHSTS

||| Build Permissions-Policy
public export
permissionsPolicy : List (String, List String) -> String
permissionsPolicy = buildPermissionsPolicy

--------------------------------------------------------------------------------
-- Security Header Presets
--------------------------------------------------------------------------------

||| Strict CSP (self only)
public export
strictCSP : String
strictCSP = buildCSP
  [ ("default-src", ["'self'"])
  , ("script-src", ["'self'"])
  , ("style-src", ["'self'"])
  , ("img-src", ["'self'"])
  , ("font-src", ["'self'"])
  , ("connect-src", ["'self'"])
  , ("frame-ancestors", ["'none'"])
  , ("base-uri", ["'self'"])
  , ("form-action", ["'self'"])
  ]

||| Standard HSTS (1 year, subdomains, preload)
public export
standardHSTS : String
standardHSTS = buildHSTS 31536000 True True

||| Strict X-Frame-Options
public export
noFrame : String
noFrame = "DENY"

||| Strict X-Content-Type-Options
public export
noSniff : String
noSniff = "nosniff"

||| Strict Referrer-Policy
public export
strictReferrer : String
strictReferrer = "strict-origin-when-cross-origin"

||| Add all security headers to collection
public export
addSecurityHeaders : Headers -> HeaderResult Headers
addSecurityHeaders headers = do
  h1 <- add headers "Content-Security-Policy" strictCSP
  h2 <- add h1 "Strict-Transport-Security" standardHSTS
  h3 <- add h2 "X-Frame-Options" noFrame
  h4 <- add h3 "X-Content-Type-Options" noSniff
  h5 <- add h4 "Referrer-Policy" strictReferrer
  h6 <- add h5 "Permissions-Policy" (buildPermissionsPolicy [("geolocation", []), ("microphone", []), ("camera", [])])
  Ok h6

--------------------------------------------------------------------------------
-- CORS Header Helpers
--------------------------------------------------------------------------------

||| Build CORS headers for specific origin
public export
corsHeaders : String -> List String -> Headers -> HeaderResult Headers
corsHeaders origin methods headers = do
  h1 <- add headers "Access-Control-Allow-Origin" origin
  h2 <- add h1 "Access-Control-Allow-Methods" (joinBy ", " methods)
  h3 <- add h2 "Access-Control-Allow-Headers" "Content-Type, Authorization"
  h4 <- add h3 "Access-Control-Max-Age" "86400"
  Ok h4

||| Build permissive CORS headers
public export
corsPermissive : Headers -> HeaderResult Headers
corsPermissive headers = do
  h1 <- add headers "Access-Control-Allow-Origin" "*"
  h2 <- add h1 "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
  h3 <- add h2 "Access-Control-Allow-Headers" "*"
  Ok h3

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

||| Check for required headers
public export
require : List String -> Headers -> HeaderResult ()
require = checkRequired

||| Validate Content-Type matches expected
public export
expectContentType : String -> Headers -> HeaderResult ()
expectContentType expected headers =
  case contentType headers of
    Nothing => Err (MissingHeader "Content-Type")
    Just ct => if isPrefixOf expected ct
                 then Ok ()
                 else Err (InvalidValueFormat "Content-Type" ct ("expected " ++ expected))

--------------------------------------------------------------------------------
-- Preset Options
--------------------------------------------------------------------------------

||| Default options
public export
defaults : HeaderOptions
defaults = defaultOptions

||| Strict options
public export
strict : HeaderOptions
strict = strictOptions

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is injection
public export
isInjection : HeaderError -> Bool
isInjection (HeaderInjection _ _) = True
isInjection _ = False

||| Check if error is size related
public export
isSizeError : HeaderError -> Bool
isSizeError (NameTooLong _ _) = True
isSizeError (ValueTooLong _ _) = True
isSizeError (TotalSizeTooLarge _) = True
isSizeError _ = False

||| Get user-friendly error message
public export
friendlyError : HeaderError -> String
friendlyError (NameTooLong name len) =
  "Header name '" ++ name ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (ValueTooLong name len) =
  "Header value for '" ++ name ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (InvalidNameChar name char) =
  "Invalid character '" ++ singleton char ++ "' in header name '" ++ name ++ "'"
friendlyError (InvalidValueChar name char) =
  "Invalid character in value for header '" ++ name ++ "'"
friendlyError EmptyName =
  "Header name cannot be empty"
friendlyError (HeaderInjection name value) =
  "Possible header injection attack detected in '" ++ name ++ "'"
friendlyError (TotalSizeTooLarge size) =
  "Total headers size (" ++ show size ++ " bytes) exceeds limit"
friendlyError (DuplicateHeader name) =
  "Duplicate header '" ++ name ++ "' not allowed"
friendlyError (MissingHeader name) =
  "Required header '" ++ name ++ "' is missing"
friendlyError (InvalidValueFormat name value reason) =
  "Invalid value for '" ++ name ++ "': " ++ reason

