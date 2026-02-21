-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeContentType - Safe Content-Type handling
|||
||| This module provides safe Content-Type operations including:
||| - Media type parsing and validation
||| - Charset handling
||| - MIME sniffing prevention
||| - Content negotiation
|||
||| Example usage:
||| ```idris
||| -- Parse Content-Type header
||| case parse "application/json; charset=utf-8" of
|||   Ok ct => processRequest ct
|||   Err e => badRequest (show e)
|||
||| -- Create Content-Type
||| let ct = json  -- application/json; charset=utf-8
||| let header = render ct
||| ```
module Proven.SafeContentType

import public Proven.Core
import public Proven.SafeContentType.Types
import public Proven.SafeContentType.Parser
import public Proven.SafeContentType.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Parse Content-Type header
public export
parse : String -> ContentTypeResult ContentType
parse = parseContentTypeDefault

||| Parse with options
public export
parseWith : ContentTypeOptions -> String -> ContentTypeResult ContentType
parseWith = parseContentType

||| Create content type
public export
create : String -> String -> ContentTypeResult ContentType
create = mkContentTypeDefault

||| Create with options
public export
createWith : ContentTypeOptions -> String -> String -> ContentTypeResult ContentType
createWith = mkContentType

||| Create from well-known type
public export
fromWellKnown : WellKnownMediaType -> ContentType
fromWellKnown = mkWellKnown

--------------------------------------------------------------------------------
-- Rendering API
--------------------------------------------------------------------------------

||| Render content type to string
public export
render : ContentType -> String
render = renderContentType

--------------------------------------------------------------------------------
-- Common Content Types (Short Aliases)
--------------------------------------------------------------------------------

||| text/plain; charset=utf-8
public export
plain : ContentType
plain = textPlain

||| text/html; charset=utf-8
public export
html : ContentType
html = textHtml

||| text/css; charset=utf-8
public export
css : ContentType
css = textCss

||| text/javascript; charset=utf-8
public export
javascript : ContentType
javascript = textJavascript

||| application/json; charset=utf-8
public export
json : ContentType
json = applicationJson

||| application/xml; charset=utf-8
public export
xml : ContentType
xml = applicationXml

||| application/octet-stream
public export
binary : ContentType
binary = applicationOctetStream

||| application/x-www-form-urlencoded
public export
form : ContentType
form = formUrlencoded

||| multipart/form-data with boundary
public export
multipart : String -> ContentType
multipart = multipartFormData

||| application/pdf
public export
pdf : ContentType
pdf = applicationPdf

||| image/png
public export
png : ContentType
png = imagePng

||| image/jpeg
public export
jpeg : ContentType
jpeg = imageJpeg

||| image/svg+xml; charset=utf-8
public export
svg : ContentType
svg = imageSvg

--------------------------------------------------------------------------------
-- Builder API
--------------------------------------------------------------------------------

||| Add charset to content type
public export
charset : Charset -> ContentType -> ContentType
charset = withCharset

||| Add boundary to content type
public export
boundary : String -> ContentType -> ContentType
boundary = withBoundary

||| Add parameter to content type
public export
param : String -> String -> ContentType -> ContentType
param = withParam

--------------------------------------------------------------------------------
-- Charset Helpers
--------------------------------------------------------------------------------

||| UTF-8 charset
public export
utf8 : Charset
utf8 = UTF8

||| ASCII charset
public export
ascii : Charset
ascii = ASCII

||| ISO-8859-1 (Latin-1) charset
public export
latin1 : Charset
latin1 = ISO8859_1

--------------------------------------------------------------------------------
-- Matching API
--------------------------------------------------------------------------------

||| Check if content type matches pattern
public export
match : ContentType -> String -> Bool
match = matches

||| Check if content type is text
public export
textType : ContentType -> Bool
textType = isText

||| Check if content type is binary
public export
binaryType : ContentType -> Bool
binaryType = isBinary

||| Check if content type is multipart
public export
multipartType : ContentType -> Bool
multipartType = isMultipart

||| Check if content type is JSON
public export
jsonType : ContentType -> Bool
jsonType = isJson

||| Check if content type is XML
public export
xmlType : ContentType -> Bool
xmlType = isXml

||| Check if content type is HTML
public export
htmlType : ContentType -> Bool
htmlType = isHtml

--------------------------------------------------------------------------------
-- Extension Mapping
--------------------------------------------------------------------------------

||| Get content type from file extension
public export
fromExtension : String -> Maybe ContentType
fromExtension = extensionToContentType

||| Get file extension from content type
public export
toExtension : ContentType -> Maybe String
toExtension = contentTypeToExtension

--------------------------------------------------------------------------------
-- Content Negotiation
--------------------------------------------------------------------------------

||| Parse Accept header
public export
parseAccept : String -> List (ContentType, Double)
parseAccept = parseAcceptHeader

||| Negotiate best content type from Accept header
public export
best : List ContentType -> String -> Maybe ContentType
best = negotiate

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

||| Default options
public export
defaults : ContentTypeOptions
defaults = defaultOptions

||| Strict options
public export
strict : ContentTypeOptions
strict = strictOptions

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is format related
public export
isFormatError : ContentTypeError -> Bool
isFormatError EmptyContentType = True
isFormatError (InvalidFormat _ _) = True
isFormatError _ = False

||| Check if error is size related
public export
isSizeError : ContentTypeError -> Bool
isSizeError (TypeTooLong _ _) = True
isSizeError (SubtypeTooLong _ _) = True
isSizeError (TooLong _) = True
isSizeError _ = False

||| Check if error is sniffing related
public export
isSniffingError : ContentTypeError -> Bool
isSniffingError (SniffingDanger _ _) = True
isSniffingError _ = False

||| Get user-friendly error message
public export
friendlyError : ContentTypeError -> String
friendlyError EmptyContentType =
  "Content-Type header cannot be empty"
friendlyError (InvalidFormat raw reason) =
  "Invalid Content-Type '" ++ raw ++ "': " ++ reason
friendlyError (TypeTooLong t len) =
  "Media type '" ++ t ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (SubtypeTooLong s len) =
  "Subtype '" ++ s ++ "' is too long (" ++ show len ++ " chars)"
friendlyError (InvalidTypeChar t c) =
  "Invalid character '" ++ singleton c ++ "' in type '" ++ t ++ "'"
friendlyError (InvalidSubtypeChar s c) =
  "Invalid character '" ++ singleton c ++ "' in subtype '" ++ s ++ "'"
friendlyError (InvalidParameter name reason) =
  "Invalid parameter '" ++ name ++ "': " ++ reason
friendlyError (MissingCharset t) =
  "Missing charset for text type '" ++ t ++ "'"
friendlyError (InvalidCharset c) =
  "Invalid charset: " ++ c
friendlyError MissingBoundary =
  "Multipart Content-Type requires a boundary parameter"
friendlyError (TooLong len) =
  "Content-Type is too long (" ++ show len ++ " chars)"
friendlyError (SniffingDanger t reason) =
  "MIME sniffing risk with '" ++ t ++ "': " ++ reason

--------------------------------------------------------------------------------
-- Security Helpers
--------------------------------------------------------------------------------

||| Check if content type is safe from sniffing
public export
safeFromSniffing : ContentType -> Bool
safeFromSniffing ct =
  let full = ct.media.mediaType ++ "/" ++ ct.media.subtype
  in not (canSniffToDangerous full)

||| Check if charset is secure (Unicode)
public export
secureCharset : ContentType -> Bool
secureCharset ct = case ct.charset of
  Nothing => False
  Just c => isUnicode c

||| Get recommended X-Content-Type-Options value
public export
nosniff : String
nosniff = "nosniff"

--------------------------------------------------------------------------------
-- API Documentation
--------------------------------------------------------------------------------

||| Common usage patterns
public export
usageExamples : String
usageExamples = """
SafeContentType Usage Examples:

1. Parse Content-Type header:
   case parse "application/json; charset=utf-8" of
     Ok ct => handleJson ct
     Err e => badRequest (friendlyError e)

2. Create common content types:
   let jsonCt = json              -- application/json; charset=utf-8
   let htmlCt = html              -- text/html; charset=utf-8
   let formCt = multipart "----" -- multipart/form-data; boundary=----

3. Content negotiation:
   let available = [json, xml, html]
   case best available acceptHeader of
     Just ct => respondWith ct
     Nothing => notAcceptable

4. Check content type:
   if jsonType ct then parseJson body
   else if xmlType ct then parseXml body
   else if htmlType ct then parseHtml body
   else unsupportedMediaType

5. Build custom content type:
   let ct = charset UTF8 $ param "version" "1" $ json

6. Get content type from file extension:
   case fromExtension "json" of
     Just ct => setContentType ct
     Nothing => setContentType binary
"""

