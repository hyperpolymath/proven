-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| HTTP header parsing and validation
|||
||| This module provides safe header parsing including:
||| - Name and value validation
||| - Injection prevention
||| - Header collection management
module Proven.SafeHeader.Parser

import Proven.Core
import Proven.SafeHeader.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Header Name Validation
--------------------------------------------------------------------------------

||| Validate and create header name
export
validateName : HeaderOptions -> String -> HeaderResult HeaderName
validateName opts name =
  let len = length (unpack name)
  in if null (unpack name)
       then Err EmptyName
       else if len > opts.maxNameLen
         then Err (NameTooLong name len)
         else if opts.strictTokens && not (isValidToken name)
           then case find (not . isTokenChar) (unpack name) of
                  Just c => Err (InvalidNameChar name c)
                  Nothing => Err (InvalidNameChar name ' ')
           else if opts.blockDangerous && isDangerousHeader name
             then Err (InvalidNameChar name ':')  -- Block dangerous
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
             else Ok (MkHeaderName (toLower name) name (believe_me Refl))

||| Validate header name with default options
export
validateNameDefault : String -> HeaderResult HeaderName
validateNameDefault = validateName defaultOptions

--------------------------------------------------------------------------------
-- Header Value Validation
--------------------------------------------------------------------------------

||| Validate and create header value
export
validateValue : HeaderOptions -> String -> String -> HeaderResult HeaderValue
validateValue opts name value =
  let trimmed = trim value
      len = length (unpack trimmed)
  in if len > opts.maxValueLen
       then Err (ValueTooLong name len)
       else if hasCRLF trimmed
         then Err (HeaderInjection name trimmed)
         else if not opts.allowEmptyValues && null (unpack trimmed)
           then Err (InvalidValueFormat name value "empty value not allowed")
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
           else Ok (MkHeaderValue trimmed (believe_me Refl))

||| Validate header value with default options
export
validateValueDefault : String -> String -> HeaderResult HeaderValue
validateValueDefault = validateValue defaultOptions

--------------------------------------------------------------------------------
-- Header Creation
--------------------------------------------------------------------------------

||| Create a validated header
export
mkHeader : HeaderOptions -> String -> String -> HeaderResult Header
mkHeader opts name value = do
  validName <- validateName opts name
  validValue <- validateValue opts name value
  Ok (MkHeader validName validValue)

||| Create header with default options
export
mkHeaderDefault : String -> String -> HeaderResult Header
mkHeaderDefault = mkHeader defaultOptions

||| Create header from well-known header
export
mkWellKnownHeader : HeaderOptions -> WellKnownHeader -> String -> HeaderResult Header
mkWellKnownHeader opts h value = do
  let name = show h
  validName <- validateName opts name
  validValue <- validateValue opts name value
  Ok (MkHeader validName validValue)

--------------------------------------------------------------------------------
-- Header Collection Operations
--------------------------------------------------------------------------------

||| Add header to collection
export
addHeader : HeaderOptions -> Headers -> String -> String -> HeaderResult Headers
addHeader opts headers name value = do
  header <- mkHeader opts name value
  let existing = find (\h => h.name.name == toLower name) headers
  case existing of
    Just _ => if opts.allowDuplicates
                then Ok (header :: headers)
                else Err (DuplicateHeader name)
    Nothing => Ok (header :: headers)

||| Add header with default options
export
addHeaderDefault : Headers -> String -> String -> HeaderResult Headers
addHeaderDefault = addHeader defaultOptions

||| Get header value by name
export
getHeader : Headers -> String -> Maybe String
getHeader headers name =
  case find (\h => h.name.name == toLower name) headers of
    Just h => Just h.value.value
    Nothing => Nothing

||| Get all values for header name (for multi-value headers)
export
getHeaders : Headers -> String -> List String
getHeaders headers name =
  map (\h => h.value.value) $
    filter (\h => h.name.name == toLower name) headers

||| Check if header exists
export
hasHeader : Headers -> String -> Bool
hasHeader headers name = isJust (getHeader headers name)

||| Remove header by name
export
removeHeader : Headers -> String -> Headers
removeHeader headers name =
  filter (\h => h.name.name /= toLower name) headers

||| Set header (replace if exists)
export
setHeader : HeaderOptions -> Headers -> String -> String -> HeaderResult Headers
setHeader opts headers name value = do
  header <- mkHeader opts name value
  let filtered = removeHeader headers name
  Ok (header :: filtered)

||| Set header with default options
export
setHeaderDefault : Headers -> String -> String -> HeaderResult Headers
setHeaderDefault = setHeader defaultOptions

--------------------------------------------------------------------------------
-- Header Parsing
--------------------------------------------------------------------------------

||| Parse single header line (Name: Value)
export
parseHeaderLine : HeaderOptions -> String -> HeaderResult Header
parseHeaderLine opts line =
  case break (== ':') line of
    (name, rest) =>
      if null (unpack rest)
        then Err (InvalidValueFormat name line "missing colon")
        else let value = drop 1 rest  -- Skip the colon
             in mkHeader opts (trim name) (trim value)

||| Parse multiple header lines
export
parseHeaders : HeaderOptions -> List String -> HeaderResult Headers
parseHeaders opts lines = foldlM addLine [] lines
  where
    addLine : Headers -> String -> HeaderResult Headers
    addLine acc line =
      if null (unpack (trim line))
        then Ok acc  -- Skip empty lines
        else do
          header <- parseHeaderLine opts line
          Ok (header :: acc)

||| Parse headers from raw string (CRLF separated)
export
parseHeaderString : HeaderOptions -> String -> HeaderResult Headers
parseHeaderString opts raw =
  let normalized = pack (map (\c => if c == '\r' then '\n' else c) (unpack raw))
      headerLines = filter (not . null . unpack) (lines normalized)
  in parseHeaders opts headerLines

--------------------------------------------------------------------------------
-- Total Size Checking
--------------------------------------------------------------------------------

||| Calculate total size of headers
export
totalSize : Headers -> Nat
totalSize headers = sum (map headerSize headers)
  where
    headerSize : Header -> Nat
    headerSize h = length (unpack h.name.originalCase) + 2 + length (unpack h.value.value)

||| Check total size is within limits
export
checkTotalSize : HeaderOptions -> Headers -> HeaderResult ()
checkTotalSize opts headers =
  let size = totalSize headers
  in if size > opts.maxTotalLen
       then Err (TotalSizeTooLarge size)
       else Ok ()

--------------------------------------------------------------------------------
-- Header Rendering
--------------------------------------------------------------------------------

||| Render single header
export
renderHeader : Header -> String
renderHeader h = h.name.originalCase ++ ": " ++ h.value.value

||| Render headers to string
export
renderHeaders : Headers -> String
renderHeaders headers =
  unlines (map renderHeader (reverse headers))

||| Render headers with CRLF (HTTP wire format)
export
renderHeadersHTTP : Headers -> String
renderHeadersHTTP headers =
  concatMap (\h => renderHeader h ++ "\r\n") (reverse headers)

--------------------------------------------------------------------------------
-- Common Header Helpers
--------------------------------------------------------------------------------

||| Parse Content-Length
export
parseContentLength : Headers -> HeaderResult (Maybe Nat)
parseContentLength headers =
  case getHeader headers "content-length" of
    Nothing => Ok Nothing
    Just v => case parsePositive v of
                Just n => Ok (Just n)
                Nothing => Err (InvalidValueFormat "Content-Length" v "not a valid number")

||| Parse Accept header quality values
export
parseAcceptHeader : String -> List (String, Double)
parseAcceptHeader value =
  map parseQValue (split (== ',') value)
  where
    parseQValue : String -> (String, Double)
    parseQValue s =
      let trimmed = trim s
      in case break (== ';') trimmed of
           (media, rest) =>
             if null (unpack rest)
               then (trim media, 1.0)
               else case parseDouble (drop 3 (trim rest)) of  -- Skip ";q="
                      Just q => (trim media, q)
                      Nothing => (trim media, 1.0)

||| Parse Cache-Control directives
export
parseCacheControl : String -> List (String, Maybe String)
parseCacheControl value =
  map parseDirective (split (== ',') value)
  where
    parseDirective : String -> (String, Maybe String)
    parseDirective s =
      let trimmed = trim s
      in case break (== '=') trimmed of
           (name, rest) =>
             if null (unpack rest)
               then (toLower (trim name), Nothing)
               else (toLower (trim name), Just (trim (drop 1 rest)))

--------------------------------------------------------------------------------
-- Security Header Builders
--------------------------------------------------------------------------------

||| Build Content-Security-Policy header value
export
buildCSP : List (String, List String) -> String
buildCSP directives =
  joinBy "; " (map renderDirective directives)
  where
    renderDirective : (String, List String) -> String
    renderDirective (name, values) = name ++ " " ++ unwords values

||| Build Strict-Transport-Security header value
export
buildHSTS : Nat -> Bool -> Bool -> String
buildHSTS maxAge includeSubDomains preload =
  let base = "max-age=" ++ show maxAge
      withSub = if includeSubDomains then base ++ "; includeSubDomains" else base
      withPreload = if preload then withSub ++ "; preload" else withSub
  in withPreload

||| Build Permissions-Policy header value
export
buildPermissionsPolicy : List (String, List String) -> String
buildPermissionsPolicy policies =
  joinBy ", " (map renderPolicy policies)
  where
    renderPolicy : (String, List String) -> String
    renderPolicy (feature, origins) =
      if null origins
        then feature ++ "=()"
        else feature ++ "=(" ++ unwords (map (\o => "\"" ++ o ++ "\"") origins) ++ ")"

--------------------------------------------------------------------------------
-- Header Utilities
--------------------------------------------------------------------------------

||| Merge two header collections
export
mergeHeaders : Headers -> Headers -> Headers
mergeHeaders h1 h2 = h1 ++ h2

||| Filter headers by category
export
filterByCategory : HeaderCategory -> Headers -> Headers
filterByCategory cat headers =
  filter (matchesCategory cat) headers
  where
    matchesCategory : HeaderCategory -> Header -> Bool
    matchesCategory c h =
      case lookupWellKnown h.name.name of
        Just wk => headerCategory wk == c
        Nothing => c == Custom

    lookupWellKnown : String -> Maybe WellKnownHeader
    lookupWellKnown name =
      -- Simplified lookup - in practice would be more complete
      if name == "content-type" then Just HdrContentType
      else if name == "content-length" then Just HdrContentLength
      else if name == "cache-control" then Just HdrCacheControl
      else if name == "content-security-policy" then Just HdrContentSecurityPolicy
      else Nothing

||| Check for required headers
export
checkRequired : List String -> Headers -> HeaderResult ()
checkRequired required headers =
  case find (\r => not (hasHeader headers r)) required of
    Just missing => Err (MissingHeader missing)
    Nothing => Ok ()

