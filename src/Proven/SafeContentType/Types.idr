-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe content type types and constraints
|||
||| This module defines types for safe Content-Type handling including:
||| - Media types (type/subtype)
||| - Charset handling
||| - Parameters validation
||| - MIME sniffing prevention
module Proven.SafeContentType.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Content Type Constraints
--------------------------------------------------------------------------------

||| Maximum type length
public export
maxTypeLength : Nat
maxTypeLength = 127

||| Maximum subtype length
public export
maxSubtypeLength : Nat
maxSubtypeLength = 127

||| Maximum parameter name length
public export
maxParamNameLength : Nat
maxParamNameLength = 64

||| Maximum parameter value length
public export
maxParamValueLength : Nat
maxParamValueLength = 256

||| Maximum total content-type length
public export
maxContentTypeLength : Nat
maxContentTypeLength = 1024

--------------------------------------------------------------------------------
-- Media Type Category
--------------------------------------------------------------------------------

||| Top-level media type categories
public export
data MediaCategory : Type where
  ||| Text content (text/*)
  TextMedia : MediaCategory
  ||| Image content (image/*)
  ImageMedia : MediaCategory
  ||| Audio content (audio/*)
  AudioMedia : MediaCategory
  ||| Video content (video/*)
  VideoMedia : MediaCategory
  ||| Application data (application/*)
  ApplicationMedia : MediaCategory
  ||| Multipart content (multipart/*)
  MultipartMedia : MediaCategory
  ||| Message content (message/*)
  MessageMedia : MediaCategory
  ||| Font content (font/*)
  FontMedia : MediaCategory
  ||| Model content (model/*)
  ModelMedia : MediaCategory
  ||| Unknown/custom type
  CustomMedia : MediaCategory

public export
Show MediaCategory where
  show TextMedia = "text"
  show ImageMedia = "image"
  show AudioMedia = "audio"
  show VideoMedia = "video"
  show ApplicationMedia = "application"
  show MultipartMedia = "multipart"
  show MessageMedia = "message"
  show FontMedia = "font"
  show ModelMedia = "model"
  show CustomMedia = "custom"

public export
Eq MediaCategory where
  TextMedia == TextMedia = True
  ImageMedia == ImageMedia = True
  AudioMedia == AudioMedia = True
  VideoMedia == VideoMedia = True
  ApplicationMedia == ApplicationMedia = True
  MultipartMedia == MultipartMedia = True
  MessageMedia == MessageMedia = True
  FontMedia == FontMedia = True
  ModelMedia == ModelMedia = True
  CustomMedia == CustomMedia = True
  _ == _ = False

||| Parse category from type string
public export
parseCategory : String -> MediaCategory
parseCategory s = case toLower s of
  "text" => TextMedia
  "image" => ImageMedia
  "audio" => AudioMedia
  "video" => VideoMedia
  "application" => ApplicationMedia
  "multipart" => MultipartMedia
  "message" => MessageMedia
  "font" => FontMedia
  "model" => ModelMedia
  _ => CustomMedia

--------------------------------------------------------------------------------
-- Charset
--------------------------------------------------------------------------------

||| Character set encoding
public export
data Charset : Type where
  ||| UTF-8 (recommended)
  UTF8 : Charset
  ||| UTF-16 (little endian)
  UTF16LE : Charset
  ||| UTF-16 (big endian)
  UTF16BE : Charset
  ||| ISO-8859-1 (Latin-1)
  ISO8859_1 : Charset
  ||| US-ASCII
  ASCII : Charset
  ||| Windows-1252
  Windows1252 : Charset
  ||| Other charset
  OtherCharset : String -> Charset

public export
Show Charset where
  show UTF8 = "utf-8"
  show UTF16LE = "utf-16le"
  show UTF16BE = "utf-16be"
  show ISO8859_1 = "iso-8859-1"
  show ASCII = "us-ascii"
  show Windows1252 = "windows-1252"
  show (OtherCharset s) = toLower s

public export
Eq Charset where
  UTF8 == UTF8 = True
  UTF16LE == UTF16LE = True
  UTF16BE == UTF16BE = True
  ISO8859_1 == ISO8859_1 = True
  ASCII == ASCII = True
  Windows1252 == Windows1252 = True
  OtherCharset a == OtherCharset b = toLower a == toLower b
  _ == _ = False

||| Parse charset from string
public export
parseCharset : String -> Charset
parseCharset s = case toLower (trim s) of
  "utf-8" => UTF8
  "utf8" => UTF8
  "utf-16le" => UTF16LE
  "utf-16be" => UTF16BE
  "iso-8859-1" => ISO8859_1
  "latin1" => ISO8859_1
  "us-ascii" => ASCII
  "ascii" => ASCII
  "windows-1252" => Windows1252
  "cp1252" => Windows1252
  other => OtherCharset other

||| Check if charset is Unicode
public export
isUnicode : Charset -> Bool
isUnicode UTF8 = True
isUnicode UTF16LE = True
isUnicode UTF16BE = True
isUnicode _ = False

--------------------------------------------------------------------------------
-- Media Type Parameter
--------------------------------------------------------------------------------

||| Content-Type parameter
public export
record Parameter where
  constructor MkParameter
  ||| Parameter name (lowercase)
  name : String
  ||| Parameter value
  value : String

public export
Eq Parameter where
  p1 == p2 = toLower p1.name == toLower p2.name && p1.value == p2.value

public export
Show Parameter where
  show p = p.name ++ "=" ++ p.value

||| Parameter list
public export
Parameters : Type
Parameters = List Parameter

--------------------------------------------------------------------------------
-- Media Type
--------------------------------------------------------------------------------

||| Validated media type
public export
record MediaType where
  constructor MkMediaType
  ||| Type (e.g., "text", "application")
  mediaType : String
  ||| Subtype (e.g., "html", "json")
  subtype : String
  ||| Suffix (e.g., "xml" from "application/rss+xml")
  suffix : Maybe String
  ||| Type category
  category : MediaCategory
  ||| Proof type is bounded
  0 typeBounded : length (unpack mediaType) <= maxTypeLength = True
  ||| Proof subtype is bounded
  0 subtypeBounded : length (unpack subtype) <= maxSubtypeLength = True

public export
Eq MediaType where
  m1 == m2 = toLower m1.mediaType == toLower m2.mediaType &&
             toLower m1.subtype == toLower m2.subtype

public export
Show MediaType where
  show m = m.mediaType ++ "/" ++ m.subtype ++
           maybe "" (\s => "+" ++ s) m.suffix

--------------------------------------------------------------------------------
-- Content Type
--------------------------------------------------------------------------------

||| Complete validated content type
public export
record ContentType where
  constructor MkContentType
  ||| Media type
  media : MediaType
  ||| Charset parameter
  charset : Maybe Charset
  ||| Boundary parameter (for multipart)
  boundary : Maybe String
  ||| Other parameters
  params : Parameters

public export
Eq ContentType where
  c1 == c2 = c1.media == c2.media && c1.charset == c2.charset

public export
Show ContentType where
  show ct = show ct.media ++
            maybe "" (\c => "; charset=" ++ show c) ct.charset ++
            maybe "" (\b => "; boundary=" ++ b) ct.boundary ++
            concatMap (\p => "; " ++ show p) ct.params

--------------------------------------------------------------------------------
-- Well-Known Media Types
--------------------------------------------------------------------------------

||| Common media types
public export
data WellKnownMediaType : Type where
  -- Text types
  TextPlain : WellKnownMediaType
  TextHtml : WellKnownMediaType
  TextCss : WellKnownMediaType
  TextJavascript : WellKnownMediaType
  TextXml : WellKnownMediaType
  TextCsv : WellKnownMediaType
  TextMarkdown : WellKnownMediaType
  -- Application types
  ApplicationJson : WellKnownMediaType
  ApplicationXml : WellKnownMediaType
  ApplicationJavascript : WellKnownMediaType
  ApplicationOctetStream : WellKnownMediaType
  ApplicationPdf : WellKnownMediaType
  ApplicationZip : WellKnownMediaType
  ApplicationGzip : WellKnownMediaType
  ApplicationFormUrlencoded : WellKnownMediaType
  ApplicationLdJson : WellKnownMediaType
  -- Multipart types
  MultipartFormData : WellKnownMediaType
  MultipartMixed : WellKnownMediaType
  MultipartAlternative : WellKnownMediaType
  -- Image types
  ImagePng : WellKnownMediaType
  ImageJpeg : WellKnownMediaType
  ImageGif : WellKnownMediaType
  ImageWebp : WellKnownMediaType
  ImageSvg : WellKnownMediaType
  ImageIco : WellKnownMediaType
  -- Audio types
  AudioMpeg : WellKnownMediaType
  AudioOgg : WellKnownMediaType
  AudioWav : WellKnownMediaType
  AudioWebm : WellKnownMediaType
  -- Video types
  VideoMp4 : WellKnownMediaType
  VideoWebm : WellKnownMediaType
  VideoOgg : WellKnownMediaType
  -- Font types
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
  show TextMarkdown = "text/markdown"
  show ApplicationJson = "application/json"
  show ApplicationXml = "application/xml"
  show ApplicationJavascript = "application/javascript"
  show ApplicationOctetStream = "application/octet-stream"
  show ApplicationPdf = "application/pdf"
  show ApplicationZip = "application/zip"
  show ApplicationGzip = "application/gzip"
  show ApplicationFormUrlencoded = "application/x-www-form-urlencoded"
  show ApplicationLdJson = "application/ld+json"
  show MultipartFormData = "multipart/form-data"
  show MultipartMixed = "multipart/mixed"
  show MultipartAlternative = "multipart/alternative"
  show ImagePng = "image/png"
  show ImageJpeg = "image/jpeg"
  show ImageGif = "image/gif"
  show ImageWebp = "image/webp"
  show ImageSvg = "image/svg+xml"
  show ImageIco = "image/x-icon"
  show AudioMpeg = "audio/mpeg"
  show AudioOgg = "audio/ogg"
  show AudioWav = "audio/wav"
  show AudioWebm = "audio/webm"
  show VideoMp4 = "video/mp4"
  show VideoWebm = "video/webm"
  show VideoOgg = "video/ogg"
  show FontWoff = "font/woff"
  show FontWoff2 = "font/woff2"
  show FontTtf = "font/ttf"
  show FontOtf = "font/otf"

public export
Eq WellKnownMediaType where
  TextPlain == TextPlain = True
  TextHtml == TextHtml = True
  TextCss == TextCss = True
  TextJavascript == TextJavascript = True
  TextXml == TextXml = True
  TextCsv == TextCsv = True
  TextMarkdown == TextMarkdown = True
  ApplicationJson == ApplicationJson = True
  ApplicationXml == ApplicationXml = True
  ApplicationJavascript == ApplicationJavascript = True
  ApplicationOctetStream == ApplicationOctetStream = True
  ApplicationPdf == ApplicationPdf = True
  ApplicationZip == ApplicationZip = True
  ApplicationGzip == ApplicationGzip = True
  ApplicationFormUrlencoded == ApplicationFormUrlencoded = True
  ApplicationLdJson == ApplicationLdJson = True
  MultipartFormData == MultipartFormData = True
  MultipartMixed == MultipartMixed = True
  MultipartAlternative == MultipartAlternative = True
  ImagePng == ImagePng = True
  ImageJpeg == ImageJpeg = True
  ImageGif == ImageGif = True
  ImageWebp == ImageWebp = True
  ImageSvg == ImageSvg = True
  ImageIco == ImageIco = True
  AudioMpeg == AudioMpeg = True
  AudioOgg == AudioOgg = True
  AudioWav == AudioWav = True
  AudioWebm == AudioWebm = True
  VideoMp4 == VideoMp4 = True
  VideoWebm == VideoWebm = True
  VideoOgg == VideoOgg = True
  FontWoff == FontWoff = True
  FontWoff2 == FontWoff2 = True
  FontTtf == FontTtf = True
  FontOtf == FontOtf = True
  _ == _ = False

||| Get category from well-known type
public export
wellKnownCategory : WellKnownMediaType -> MediaCategory
wellKnownCategory TextPlain = TextMedia
wellKnownCategory TextHtml = TextMedia
wellKnownCategory TextCss = TextMedia
wellKnownCategory TextJavascript = TextMedia
wellKnownCategory TextXml = TextMedia
wellKnownCategory TextCsv = TextMedia
wellKnownCategory TextMarkdown = TextMedia
wellKnownCategory ApplicationJson = ApplicationMedia
wellKnownCategory ApplicationXml = ApplicationMedia
wellKnownCategory ApplicationJavascript = ApplicationMedia
wellKnownCategory ApplicationOctetStream = ApplicationMedia
wellKnownCategory ApplicationPdf = ApplicationMedia
wellKnownCategory ApplicationZip = ApplicationMedia
wellKnownCategory ApplicationGzip = ApplicationMedia
wellKnownCategory ApplicationFormUrlencoded = ApplicationMedia
wellKnownCategory ApplicationLdJson = ApplicationMedia
wellKnownCategory MultipartFormData = MultipartMedia
wellKnownCategory MultipartMixed = MultipartMedia
wellKnownCategory MultipartAlternative = MultipartMedia
wellKnownCategory ImagePng = ImageMedia
wellKnownCategory ImageJpeg = ImageMedia
wellKnownCategory ImageGif = ImageMedia
wellKnownCategory ImageWebp = ImageMedia
wellKnownCategory ImageSvg = ImageMedia
wellKnownCategory ImageIco = ImageMedia
wellKnownCategory AudioMpeg = AudioMedia
wellKnownCategory AudioOgg = AudioMedia
wellKnownCategory AudioWav = AudioMedia
wellKnownCategory AudioWebm = AudioMedia
wellKnownCategory VideoMp4 = VideoMedia
wellKnownCategory VideoWebm = VideoMedia
wellKnownCategory VideoOgg = VideoMedia
wellKnownCategory FontWoff = FontMedia
wellKnownCategory FontWoff2 = FontMedia
wellKnownCategory FontTtf = FontMedia
wellKnownCategory FontOtf = FontMedia

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Content-Type parsing/validation errors
public export
data ContentTypeError : Type where
  ||| Empty content type
  EmptyContentType : ContentTypeError
  ||| Invalid media type format
  InvalidFormat : (raw : String) -> (reason : String) -> ContentTypeError
  ||| Type too long
  TypeTooLong : (mediaType : String) -> (len : Nat) -> ContentTypeError
  ||| Subtype too long
  SubtypeTooLong : (subtype : String) -> (len : Nat) -> ContentTypeError
  ||| Invalid character in type
  InvalidTypeChar : (mediaType : String) -> (char : Char) -> ContentTypeError
  ||| Invalid character in subtype
  InvalidSubtypeChar : (subtype : String) -> (char : Char) -> ContentTypeError
  ||| Invalid parameter
  InvalidParameter : (name : String) -> (reason : String) -> ContentTypeError
  ||| Missing required charset
  MissingCharset : (mediaType : String) -> ContentTypeError
  ||| Invalid charset
  InvalidCharset : (charset : String) -> ContentTypeError
  ||| Missing boundary for multipart
  MissingBoundary : ContentTypeError
  ||| Total length too long
  TooLong : (len : Nat) -> ContentTypeError
  ||| MIME sniffing danger
  SniffingDanger : (mediaType : String) -> (reason : String) -> ContentTypeError

public export
Show ContentTypeError where
  show EmptyContentType =
    "Content-Type cannot be empty"
  show (InvalidFormat raw reason) =
    "Invalid Content-Type format '" ++ raw ++ "': " ++ reason
  show (TypeTooLong mediaType len) =
    "Media type too long: " ++ show len ++ " chars (max " ++ show maxTypeLength ++ ")"
  show (SubtypeTooLong subtype len) =
    "Subtype too long: " ++ show len ++ " chars (max " ++ show maxSubtypeLength ++ ")"
  show (InvalidTypeChar mediaType char) =
    "Invalid character '" ++ singleton char ++ "' in type: " ++ mediaType
  show (InvalidSubtypeChar subtype char) =
    "Invalid character '" ++ singleton char ++ "' in subtype: " ++ subtype
  show (InvalidParameter name reason) =
    "Invalid parameter '" ++ name ++ "': " ++ reason
  show (MissingCharset mediaType) =
    "Missing charset for text type: " ++ mediaType
  show (InvalidCharset charset) =
    "Invalid charset: " ++ charset
  show MissingBoundary =
    "Missing boundary parameter for multipart type"
  show (TooLong len) =
    "Content-Type too long: " ++ show len ++ " chars"
  show (SniffingDanger mediaType reason) =
    "MIME sniffing danger for '" ++ mediaType ++ "': " ++ reason

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
  ||| Prevent MIME sniffing attacks
  preventSniffing : Bool
  ||| Default charset for text
  defaultCharset : Charset

||| Default options
public export
defaultOptions : ContentTypeOptions
defaultOptions = MkContentTypeOptions
  { maxTypeLen = maxTypeLength
  , maxSubtypeLen = maxSubtypeLength
  , maxTotalLen = maxContentTypeLength
  , requireCharset = False
  , requireBoundary = True
  , preventSniffing = True
  , defaultCharset = UTF8
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
  , defaultCharset = UTF8
  }

--------------------------------------------------------------------------------
-- Token Character Validation
--------------------------------------------------------------------------------

||| Valid token characters per RFC 2045
public export
isValidTokenChar : Char -> Bool
isValidTokenChar c =
  let code = ord c
  in (code >= 33 && code <= 126) &&  -- Printable non-space
     not (c `elem` unpack "()<>@,;:\\\"/[]?=")

||| Check if string is valid token
public export
isValidToken : String -> Bool
isValidToken s =
  not (null (unpack s)) && all isValidTokenChar (unpack s)

--------------------------------------------------------------------------------
-- Dangerous Types (MIME Sniffing)
--------------------------------------------------------------------------------

||| Types that can be sniffed to something dangerous
public export
sniffableToDangerous : List String
sniffableToDangerous =
  [ "text/plain"              -- Can be sniffed to HTML/JS
  , "application/octet-stream" -- Generic binary
  , "application/x-unknown"    -- Unknown type
  , "unknown/unknown"          -- Completely unknown
  ]

||| Check if type can be sniffed to dangerous
public export
canSniffToDangerous : String -> Bool
canSniffToDangerous s = toLower s `elem` sniffableToDangerous

