-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe Content-Type types and constraints
|||
||| This module defines types for safe Content-Type handling including:
||| - Media type validation
||| - Charset handling
||| - MIME sniffing prevention
||| - Content negotiation types
module Proven.SafeContentType.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Content-Type Constraints
--------------------------------------------------------------------------------

||| Maximum media type length
public export
maxTypeLength : Nat
maxTypeLength = 127

||| Maximum subtype length
public export
maxSubtypeLength : Nat
maxSubtypeLength = 127

||| Maximum total Content-Type length
public export
maxTotalLength : Nat
maxTotalLength = 1024

--------------------------------------------------------------------------------
-- Token Characters (RFC 2045)
--------------------------------------------------------------------------------

||| Valid token characters per RFC 2045
||| token = 1*<any CHAR except SPACE, CTLs, or tspecials>
public export
isValidTokenChar : Char -> Bool
isValidTokenChar c =
  isAlphaNum c ||
  c == '!' || c == '#' || c == '$' || c == '%' || c == '&' ||
  c == '\'' || c == '*' || c == '+' || c == '-' || c == '.' ||
  c == '^' || c == '_' || c == '`' || c == '|' || c == '~'

||| Check if string is valid token
public export
isValidToken : String -> Bool
isValidToken s = not (null (unpack s)) && all isValidTokenChar (unpack s)

--------------------------------------------------------------------------------
-- Media Category
--------------------------------------------------------------------------------

||| Media type category
public export
data MediaCategory : Type where
  ||| Text media types (text/*)
  TextMedia : MediaCategory
  ||| Application media types (application/*)
  ApplicationMedia : MediaCategory
  ||| Image media types (image/*)
  ImageMedia : MediaCategory
  ||| Audio media types (audio/*)
  AudioMedia : MediaCategory
  ||| Video media types (video/*)
  VideoMedia : MediaCategory
  ||| Multipart media types (multipart/*)
  MultipartMedia : MediaCategory
  ||| Font media types (font/*)
  FontMedia : MediaCategory
  ||| Custom/unknown category
  CustomMedia : MediaCategory

public export
Show MediaCategory where
  show TextMedia = "text"
  show ApplicationMedia = "application"
  show ImageMedia = "image"
  show AudioMedia = "audio"
  show VideoMedia = "video"
  show MultipartMedia = "multipart"
  show FontMedia = "font"
  show CustomMedia = "custom"

public export
Eq MediaCategory where
  TextMedia == TextMedia = True
  ApplicationMedia == ApplicationMedia = True
  ImageMedia == ImageMedia = True
  AudioMedia == AudioMedia = True
  VideoMedia == VideoMedia = True
  MultipartMedia == MultipartMedia = True
  FontMedia == FontMedia = True
  CustomMedia == CustomMedia = True
  _ == _ = False

||| Parse media category from type string
public export
parseCategory : String -> MediaCategory
parseCategory "text" = TextMedia
parseCategory "application" = ApplicationMedia
parseCategory "image" = ImageMedia
parseCategory "audio" = AudioMedia
parseCategory "video" = VideoMedia
parseCategory "multipart" = MultipartMedia
parseCategory "font" = FontMedia
parseCategory _ = CustomMedia

--------------------------------------------------------------------------------
-- Charset
--------------------------------------------------------------------------------

||| Character encoding
public export
data Charset : Type where
  ||| UTF-8
  UTF8 : Charset
  ||| UTF-16 Big Endian
  UTF16BE : Charset
  ||| UTF-16 Little Endian
  UTF16LE : Charset
  ||| ASCII
  ASCII : Charset
  ||| ISO-8859-1 (Latin-1)
  ISO8859_1 : Charset
  ||| Other charset
  OtherCharset : String -> Charset

public export
Show Charset where
  show UTF8 = "utf-8"
  show UTF16BE = "utf-16be"
  show UTF16LE = "utf-16le"
  show ASCII = "us-ascii"
  show ISO8859_1 = "iso-8859-1"
  show (OtherCharset s) = s

public export
Eq Charset where
  UTF8 == UTF8 = True
  UTF16BE == UTF16BE = True
  UTF16LE == UTF16LE = True
  ASCII == ASCII = True
  ISO8859_1 == ISO8859_1 = True
  (OtherCharset a) == (OtherCharset b) = a == b
  _ == _ = False

||| Check if charset is Unicode
public export
isUnicode : Charset -> Bool
isUnicode UTF8 = True
isUnicode UTF16BE = True
isUnicode UTF16LE = True
isUnicode _ = False

||| Parse charset name to Charset
public export
parseCharset : String -> Charset
parseCharset s = case toLower (trim s) of
  "utf-8" => UTF8
  "utf8" => UTF8
  "utf-16be" => UTF16BE
  "utf-16le" => UTF16LE
  "us-ascii" => ASCII
  "ascii" => ASCII
  "iso-8859-1" => ISO8859_1
  "latin1" => ISO8859_1
  other => OtherCharset other

--------------------------------------------------------------------------------
-- Media Type
--------------------------------------------------------------------------------

||| Validated media type (type/subtype)
public export
record MediaType where
  constructor MkMediaType
  ||| The media type (e.g., "text", "application")
  mediaType : String
  ||| The subtype (e.g., "html", "json")
  subtype : String
  ||| Structured syntax suffix (e.g., "json" from "vnd.api+json")
  suffix : Maybe String
  ||| Media category
  category : MediaCategory
  ||| Proof type is bounded
  0 typeBounded : length (unpack mediaType) <= maxTypeLength = True
  ||| Proof subtype is bounded
  0 subtypeBounded : length (unpack subtype) <= maxSubtypeLength = True

public export
Eq MediaType where
  m1 == m2 = m1.mediaType == m2.mediaType && m1.subtype == m2.subtype

public export
Show MediaType where
  show m = m.mediaType ++ "/" ++ m.subtype ++
           maybe "" (\s => "+" ++ s) m.suffix

--------------------------------------------------------------------------------
-- Parameter
--------------------------------------------------------------------------------

||| Content-Type parameter (name=value)
public export
record Parameter where
  constructor MkParameter
  ||| Parameter name (lowercase)
  name : String
  ||| Parameter value
  value : String

public export
Eq Parameter where
  p1 == p2 = p1.name == p2.name && p1.value == p2.value

public export
Show Parameter where
  show p = p.name ++ "=" ++ p.value

||| List of parameters
public export
Parameters : Type
Parameters = List Parameter

--------------------------------------------------------------------------------
-- Content Type
--------------------------------------------------------------------------------

||| Complete validated Content-Type
public export
record ContentType where
  constructor MkContentType
  ||| Media type
  media : MediaType
  ||| Character encoding
  charset : Maybe Charset
  ||| Multipart boundary
  boundary : Maybe String
  ||| Additional parameters
  params : Parameters

public export
Eq ContentType where
  c1 == c2 = c1.media == c2.media &&
             c1.charset == c2.charset &&
             c1.boundary == c2.boundary

public export
Show ContentType where
  show ct =
    let base = show ct.media
        cs = maybe "" (\c => "; charset=" ++ show c) ct.charset
        bd = maybe "" (\b => "; boundary=" ++ b) ct.boundary
        ps = concatMap (\p => "; " ++ show p) ct.params
    in base ++ cs ++ bd ++ ps

--------------------------------------------------------------------------------
-- Well-Known Media Types
--------------------------------------------------------------------------------

||| Well-known media types
public export
data WellKnownMediaType : Type where
  TextPlain : WellKnownMediaType
  TextHtml : WellKnownMediaType
  TextCss : WellKnownMediaType
  TextJavascript : WellKnownMediaType
  TextXml : WellKnownMediaType
  TextCsv : WellKnownMediaType
  ApplicationJson : WellKnownMediaType
  ApplicationXml : WellKnownMediaType
  ApplicationOctetStream : WellKnownMediaType
  ApplicationFormUrlencoded : WellKnownMediaType
  ApplicationPdf : WellKnownMediaType
  ApplicationZip : WellKnownMediaType
  ApplicationGzip : WellKnownMediaType
  MultipartFormData : WellKnownMediaType
  MultipartMixed : WellKnownMediaType
  ImagePng : WellKnownMediaType
  ImageJpeg : WellKnownMediaType
  ImageGif : WellKnownMediaType
  ImageWebp : WellKnownMediaType
  ImageSvg : WellKnownMediaType
  ImageIco : WellKnownMediaType
  AudioMpeg : WellKnownMediaType
  AudioOgg : WellKnownMediaType
  VideoMp4 : WellKnownMediaType
  VideoWebm : WellKnownMediaType
  FontWoff : WellKnownMediaType
  FontWoff2 : WellKnownMediaType
  FontTtf : WellKnownMediaType
  FontOtf : WellKnownMediaType

public export
Show WellKnownMediaType where
  show TextPlain = "text/plain"
  show TextHtml = "text/html"
  show TextCss = "text/css"
  show TextJavascript = "text/javascript"
  show TextXml = "text/xml"
  show TextCsv = "text/csv"
  show ApplicationJson = "application/json"
  show ApplicationXml = "application/xml"
  show ApplicationOctetStream = "application/octet-stream"
  show ApplicationFormUrlencoded = "application/x-www-form-urlencoded"
  show ApplicationPdf = "application/pdf"
  show ApplicationZip = "application/zip"
  show ApplicationGzip = "application/gzip"
  show MultipartFormData = "multipart/form-data"
  show MultipartMixed = "multipart/mixed"
  show ImagePng = "image/png"
  show ImageJpeg = "image/jpeg"
  show ImageGif = "image/gif"
  show ImageWebp = "image/webp"
  show ImageSvg = "image/svg+xml"
  show ImageIco = "image/x-icon"
  show AudioMpeg = "audio/mpeg"
  show AudioOgg = "audio/ogg"
  show VideoMp4 = "video/mp4"
  show VideoWebm = "video/webm"
  show FontWoff = "font/woff"
  show FontWoff2 = "font/woff2"
  show FontTtf = "font/ttf"
  show FontOtf = "font/otf"

||| Get category for well-known media type
public export
wellKnownCategory : WellKnownMediaType -> MediaCategory
wellKnownCategory TextPlain = TextMedia
wellKnownCategory TextHtml = TextMedia
wellKnownCategory TextCss = TextMedia
wellKnownCategory TextJavascript = TextMedia
wellKnownCategory TextXml = TextMedia
wellKnownCategory TextCsv = TextMedia
wellKnownCategory ApplicationJson = ApplicationMedia
wellKnownCategory ApplicationXml = ApplicationMedia
wellKnownCategory ApplicationOctetStream = ApplicationMedia
wellKnownCategory ApplicationFormUrlencoded = ApplicationMedia
wellKnownCategory ApplicationPdf = ApplicationMedia
wellKnownCategory ApplicationZip = ApplicationMedia
wellKnownCategory ApplicationGzip = ApplicationMedia
wellKnownCategory MultipartFormData = MultipartMedia
wellKnownCategory MultipartMixed = MultipartMedia
wellKnownCategory ImagePng = ImageMedia
wellKnownCategory ImageJpeg = ImageMedia
wellKnownCategory ImageGif = ImageMedia
wellKnownCategory ImageWebp = ImageMedia
wellKnownCategory ImageSvg = ImageMedia
wellKnownCategory ImageIco = ImageMedia
wellKnownCategory AudioMpeg = AudioMedia
wellKnownCategory AudioOgg = AudioMedia
wellKnownCategory VideoMp4 = VideoMedia
wellKnownCategory VideoWebm = VideoMedia
wellKnownCategory FontWoff = FontMedia
wellKnownCategory FontWoff2 = FontMedia
wellKnownCategory FontTtf = FontMedia
wellKnownCategory FontOtf = FontMedia

--------------------------------------------------------------------------------
-- MIME Sniffing
--------------------------------------------------------------------------------

||| Check if content type can be MIME-sniffed to a dangerous type
public export
canSniffToDangerous : String -> Bool
canSniffToDangerous s = case toLower s of
  "text/plain" => True
  "application/octet-stream" => True
  "text/html" => True
  _ => False

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Content-Type parsing/validation errors
public export
data ContentTypeError : Type where
  ||| Empty Content-Type
  EmptyContentType : ContentTypeError
  ||| Invalid format
  InvalidFormat : (raw : String) -> (reason : String) -> ContentTypeError
  ||| Media type too long
  TypeTooLong : (t : String) -> (len : Nat) -> ContentTypeError
  ||| Subtype too long
  SubtypeTooLong : (s : String) -> (len : Nat) -> ContentTypeError
  ||| Invalid character in type
  InvalidTypeChar : (t : String) -> (char : Char) -> ContentTypeError
  ||| Invalid character in subtype
  InvalidSubtypeChar : (s : String) -> (char : Char) -> ContentTypeError
  ||| Invalid parameter
  InvalidParameter : (name : String) -> (reason : String) -> ContentTypeError
  ||| Missing charset for text type
  MissingCharset : (t : String) -> ContentTypeError
  ||| Invalid charset
  InvalidCharset : (c : String) -> ContentTypeError
  ||| Missing boundary for multipart
  MissingBoundary : ContentTypeError
  ||| Content-Type too long
  TooLong : (len : Nat) -> ContentTypeError
  ||| MIME sniffing danger
  SniffingDanger : (t : String) -> (reason : String) -> ContentTypeError

public export
Show ContentTypeError where
  show EmptyContentType =
    "Empty Content-Type"
  show (InvalidFormat raw reason) =
    "Invalid Content-Type '" ++ raw ++ "': " ++ reason
  show (TypeTooLong t len) =
    "Type too long: " ++ show len ++ " chars (max " ++ show maxTypeLength ++ ")"
  show (SubtypeTooLong s len) =
    "Subtype too long: " ++ show len ++ " chars (max " ++ show maxSubtypeLength ++ ")"
  show (InvalidTypeChar t char) =
    "Invalid character '" ++ singleton char ++ "' in type: " ++ t
  show (InvalidSubtypeChar s char) =
    "Invalid character '" ++ singleton char ++ "' in subtype: " ++ s
  show (InvalidParameter name reason) =
    "Invalid parameter '" ++ name ++ "': " ++ reason
  show (MissingCharset t) =
    "Missing charset for text type: " ++ t
  show (InvalidCharset c) =
    "Invalid charset: " ++ c
  show MissingBoundary =
    "Multipart Content-Type requires boundary parameter"
  show (TooLong len) =
    "Content-Type too long: " ++ show len ++ " chars (max " ++ show maxTotalLength ++ ")"
  show (SniffingDanger t reason) =
    "MIME sniffing danger for '" ++ t ++ "': " ++ reason

public export
Eq ContentTypeError where
  EmptyContentType == EmptyContentType = True
  InvalidFormat r1 s1 == InvalidFormat r2 s2 = r1 == r2 && s1 == s2
  TypeTooLong t1 l1 == TypeTooLong t2 l2 = t1 == t2 && l1 == l2
  SubtypeTooLong s1 l1 == SubtypeTooLong s2 l2 = s1 == s2 && l1 == l2
  InvalidTypeChar t1 c1 == InvalidTypeChar t2 c2 = t1 == t2 && c1 == c2
  InvalidSubtypeChar s1 c1 == InvalidSubtypeChar s2 c2 = s1 == s2 && c1 == c2
  InvalidParameter n1 r1 == InvalidParameter n2 r2 = n1 == n2 && r1 == r2
  MissingCharset t1 == MissingCharset t2 = t1 == t2
  InvalidCharset c1 == InvalidCharset c2 = c1 == c2
  MissingBoundary == MissingBoundary = True
  TooLong l1 == TooLong l2 = l1 == l2
  SniffingDanger t1 r1 == SniffingDanger t2 r2 = t1 == t2 && r1 == r2
  _ == _ = False

||| Content-Type result type
public export
ContentTypeResult : Type -> Type
ContentTypeResult = Result ContentTypeError

--------------------------------------------------------------------------------
-- Validation Options
--------------------------------------------------------------------------------

||| Content-Type validation options
public export
record ContentTypeOptions where
  constructor MkContentTypeOptions
  ||| Maximum type length
  maxTypeLen : Nat
  ||| Maximum subtype length
  maxSubtypeLen : Nat
  ||| Maximum total length
  maxTotalLen : Nat
  ||| Require charset for text types
  requireCharset : Bool
  ||| Require boundary for multipart
  requireBoundary : Bool
  ||| Prevent MIME sniffing
  preventSniffing : Bool

||| Default options
public export
defaultOptions : ContentTypeOptions
defaultOptions = MkContentTypeOptions
  { maxTypeLen = maxTypeLength
  , maxSubtypeLen = maxSubtypeLength
  , maxTotalLen = maxTotalLength
  , requireCharset = False
  , requireBoundary = True
  , preventSniffing = True
  }

||| Strict options
public export
strictOptions : ContentTypeOptions
strictOptions = MkContentTypeOptions
  { maxTypeLen = 64
  , maxSubtypeLen = 64
  , maxTotalLen = 512
  , requireCharset = True
  , requireBoundary = True
  , preventSniffing = True
  }

--------------------------------------------------------------------------------
-- Decidable Length Bounds
--------------------------------------------------------------------------------

||| Decide whether a string's length is within a bound.
||| Returns a proof when True, enabling believe_me-free construction.
public export
decLengthLTE : (s : String) -> (maxLen : Nat) ->
               Dec (length (unpack s) <= maxLen = True)
decLengthLTE s maxLen =
  case decEq (length (unpack s) <= maxLen) True of
    Yes prf => Yes prf
    No contra => No contra

||| Helper: construct a MediaType with decidable bounds checking.
||| Only succeeds when both type and subtype are within their length limits.
public export
mkMediaTypeSafe : (t : String) -> (s : String) ->
                  (suffix : Maybe String) -> (cat : MediaCategory) ->
                  Maybe MediaType
mkMediaTypeSafe t s suffix cat =
  case decLengthLTE t maxTypeLength of
    Yes tPrf => case decLengthLTE s maxSubtypeLength of
                  Yes sPrf => Just (MkMediaType t s suffix cat tPrf sPrf)
                  No _ => Nothing
    No _ => Nothing
