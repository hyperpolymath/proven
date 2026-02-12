-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Content-Type parsing and building
|||
||| This module provides safe Content-Type operations including:
||| - Header parsing
||| - Media type validation
||| - Parameter extraction
||| - Content-Type building
module Proven.SafeContentType.Parser

import Proven.Core
import Proven.SafeContentType.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Type/Subtype Validation
--------------------------------------------------------------------------------

||| Validate media type
export
validateType : ContentTypeOptions -> String -> ContentTypeResult String
validateType opts t =
  let lower = toLower (trim t)
      len = length (unpack lower)
  in if null (unpack lower)
       then Err (InvalidFormat t "empty type")
       else if len > opts.maxTypeLen
         then Err (TypeTooLong t len)
         else case find (not . isValidTokenChar) (unpack lower) of
                Just c => Err (InvalidTypeChar t c)
                Nothing => Ok lower

||| Validate subtype
export
validateSubtype : ContentTypeOptions -> String -> ContentTypeResult String
validateSubtype opts s =
  let lower = toLower (trim s)
      len = length (unpack lower)
  in if null (unpack lower)
       then Err (InvalidFormat s "empty subtype")
       else if len > opts.maxSubtypeLen
         then Err (SubtypeTooLong s len)
         else case find (not . isValidTokenChar) (unpack lower) of
                Just c => Err (InvalidSubtypeChar s c)
                Nothing => Ok lower

--------------------------------------------------------------------------------
-- Parameter Parsing
--------------------------------------------------------------------------------

||| Parse parameter value (handles quoted strings)
parseParamValue : String -> String
parseParamValue v =
  let trimmed = trim v
  in if isPrefixOf "\"" trimmed && isSuffixOf "\"" trimmed
       then pack $ drop 1 $ take (minus (length (unpack trimmed)) 1) (unpack trimmed)
       else trimmed

||| Parse single parameter (name=value)
export
parseParameter : String -> ContentTypeResult Parameter
parseParameter param =
  case break (== '=') (trim param) of
    (name, rest) =>
      if null (unpack rest)
        then Err (InvalidParameter param "missing value")
        else let n = toLower (trim name)
                 v = parseParamValue (drop 1 rest)
             in if not (isValidToken n)
                  then Err (InvalidParameter n "invalid name")
                  else Ok (MkParameter n v)

||| Parse all parameters
export
parseParameters : List String -> ContentTypeResult Parameters
parseParameters = traverse parseParameter

--------------------------------------------------------------------------------
-- Content-Type Parsing
--------------------------------------------------------------------------------

||| Extract suffix from subtype (e.g., "json" from "vnd.api+json")
extractSuffix : String -> (String, Maybe String)
extractSuffix subtype =
  case break (== '+') subtype of
    (base, rest) =>
      if null (unpack rest)
        then (subtype, Nothing)
        else (base, Just (drop 1 rest))

||| Parse Content-Type header
export
parseContentType : ContentTypeOptions -> String -> ContentTypeResult ContentType
parseContentType opts raw =
  let trimmed = trim raw
  in if null (unpack trimmed)
       then Err EmptyContentType
       else if length (unpack trimmed) > opts.maxTotalLen
         then Err (TooLong (length (unpack trimmed)))
         else do
           -- Split on semicolons
           let parts = split (== ';') trimmed
           case parts of
             [] => Err EmptyContentType
             (mediaStr :: paramStrs) => do
               -- Parse type/subtype
               (t, s) <- parseMediaPart mediaStr
               validType <- validateType opts t
               validSubtype <- validateSubtype opts s
               let (baseSubtype, suffix) = extractSuffix validSubtype
               let category = parseCategory validType
               let media = MkMediaType validType baseSubtype suffix category
                             (believe_me Refl) (believe_me Refl)
               -- Parse parameters
               params <- parseParameters (filter (not . null . unpack . trim) paramStrs)
               -- Extract charset
               let charset = map (parseCharset . value) $
                             find (\p => p.name == "charset") params
               -- Extract boundary
               let boundary = map value $ find (\p => p.name == "boundary") params
               -- Filter out charset and boundary from params
               let otherParams = filter (\p => p.name /= "charset" && p.name /= "boundary") params
               -- Validate multipart has boundary
               if category == MultipartMedia && isNothing boundary && opts.requireBoundary
                 then Err MissingBoundary
                 else Ok (MkContentType media charset boundary otherParams)
  where
    parseMediaPart : String -> ContentTypeResult (String, String)
    parseMediaPart str =
      case break (== '/') (trim str) of
        (t, rest) =>
          if null (unpack rest)
            then Err (InvalidFormat str "missing slash")
            else Ok (t, drop 1 rest)

||| Parse with default options
export
parseContentTypeDefault : String -> ContentTypeResult ContentType
parseContentTypeDefault = parseContentType defaultOptions

--------------------------------------------------------------------------------
-- Content-Type Creation
--------------------------------------------------------------------------------

||| Create content type from type and subtype
export
mkContentType : ContentTypeOptions -> String -> String -> ContentTypeResult ContentType
mkContentType opts t s = do
  validType <- validateType opts t
  validSubtype <- validateSubtype opts s
  let (baseSubtype, suffix) = extractSuffix validSubtype
  let category = parseCategory validType
  let media = MkMediaType validType baseSubtype suffix category
                (believe_me Refl) (believe_me Refl)
  -- Check sniffing danger
  let fullType = validType ++ "/" ++ validSubtype
  if opts.preventSniffing && canSniffToDangerous fullType
    then Err (SniffingDanger fullType "can be MIME sniffed")
    else Ok (MkContentType media Nothing Nothing [])

||| Create with default options
export
mkContentTypeDefault : String -> String -> ContentTypeResult ContentType
mkContentTypeDefault = mkContentType defaultOptions

||| Create from well-known type
export
mkWellKnown : WellKnownMediaType -> ContentType
mkWellKnown wk =
  let full = show wk
  in case break (== '/') full of
       (t, rest) =>
         let s = drop 1 rest
             (baseSubtype, suffix) = extractSuffix s
             category = wellKnownCategory wk
             media = MkMediaType t baseSubtype suffix category
                       (believe_me Refl) (believe_me Refl)
         in MkContentType media Nothing Nothing []

--------------------------------------------------------------------------------
-- Content-Type Building
--------------------------------------------------------------------------------

||| Render content type to string
export
renderContentType : ContentType -> String
renderContentType ct = show ct

||| Build content type with charset
export
withCharset : Charset -> ContentType -> ContentType
withCharset c ct = { charset := Just c } ct

||| Build content type with boundary
export
withBoundary : String -> ContentType -> ContentType
withBoundary b ct = { boundary := Just b } ct

||| Add parameter
export
withParam : String -> String -> ContentType -> ContentType
withParam name value ct =
  { params := MkParameter (toLower name) value :: ct.params } ct

--------------------------------------------------------------------------------
-- Common Content Types
--------------------------------------------------------------------------------

||| text/plain; charset=utf-8
export
textPlain : ContentType
textPlain = withCharset UTF8 (mkWellKnown TextPlain)

||| text/html; charset=utf-8
export
textHtml : ContentType
textHtml = withCharset UTF8 (mkWellKnown TextHtml)

||| text/css; charset=utf-8
export
textCss : ContentType
textCss = withCharset UTF8 (mkWellKnown TextCss)

||| text/javascript; charset=utf-8
export
textJavascript : ContentType
textJavascript = withCharset UTF8 (mkWellKnown TextJavascript)

||| application/json; charset=utf-8
export
applicationJson : ContentType
applicationJson = withCharset UTF8 (mkWellKnown ApplicationJson)

||| application/xml; charset=utf-8
export
applicationXml : ContentType
applicationXml = withCharset UTF8 (mkWellKnown ApplicationXml)

||| application/octet-stream
export
applicationOctetStream : ContentType
applicationOctetStream = mkWellKnown ApplicationOctetStream

||| application/x-www-form-urlencoded
export
formUrlencoded : ContentType
formUrlencoded = mkWellKnown ApplicationFormUrlencoded

||| multipart/form-data with boundary
export
multipartFormData : String -> ContentType
multipartFormData boundary = withBoundary boundary (mkWellKnown MultipartFormData)

||| application/pdf
export
applicationPdf : ContentType
applicationPdf = mkWellKnown ApplicationPdf

||| image/png
export
imagePng : ContentType
imagePng = mkWellKnown ImagePng

||| image/jpeg
export
imageJpeg : ContentType
imageJpeg = mkWellKnown ImageJpeg

||| image/svg+xml; charset=utf-8
export
imageSvg : ContentType
imageSvg = withCharset UTF8 (mkWellKnown ImageSvg)

--------------------------------------------------------------------------------
-- Content-Type Matching
--------------------------------------------------------------------------------

||| Check if content type matches pattern
export
matches : ContentType -> String -> Bool
matches ct pattern =
  let (patType, patSubtype) = case break (== '/') pattern of
                                (t, rest) => (toLower t, toLower (drop 1 rest))
  in (patType == "*" || patType == ct.media.mediaType) &&
     (patSubtype == "*" || patSubtype == ct.media.subtype)

||| Check if content type is text
export
isText : ContentType -> Bool
isText ct = ct.media.category == TextMedia

||| Check if content type is binary
export
isBinary : ContentType -> Bool
isBinary ct = ct.media.category `elem` [ImageMedia, AudioMedia, VideoMedia, ApplicationMedia]

||| Check if content type is multipart
export
isMultipart : ContentType -> Bool
isMultipart ct = ct.media.category == MultipartMedia

||| Check if content type is JSON
export
isJson : ContentType -> Bool
isJson ct = ct.media.subtype == "json" ||
            ct.media.suffix == Just "json"

||| Check if content type is XML
export
isXml : ContentType -> Bool
isXml ct = ct.media.subtype == "xml" ||
           ct.media.suffix == Just "xml"

||| Check if content type is HTML
export
isHtml : ContentType -> Bool
isHtml ct = ct.media.mediaType == "text" && ct.media.subtype == "html"

--------------------------------------------------------------------------------
-- MIME Type Extensions
--------------------------------------------------------------------------------

||| Common file extension to content type mapping
export
extensionToContentType : String -> Maybe ContentType
extensionToContentType ext = case toLower ext of
  "txt" => Just textPlain
  "html" => Just textHtml
  "htm" => Just textHtml
  "css" => Just textCss
  "js" => Just textJavascript
  "mjs" => Just textJavascript
  "json" => Just applicationJson
  "xml" => Just applicationXml
  "pdf" => Just applicationPdf
  "png" => Just imagePng
  "jpg" => Just imageJpeg
  "jpeg" => Just imageJpeg
  "gif" => Just (mkWellKnown ImageGif)
  "webp" => Just (mkWellKnown ImageWebp)
  "svg" => Just imageSvg
  "ico" => Just (mkWellKnown ImageIco)
  "mp3" => Just (mkWellKnown AudioMpeg)
  "mp4" => Just (mkWellKnown VideoMp4)
  "webm" => Just (mkWellKnown VideoWebm)
  "woff" => Just (mkWellKnown FontWoff)
  "woff2" => Just (mkWellKnown FontWoff2)
  "ttf" => Just (mkWellKnown FontTtf)
  "otf" => Just (mkWellKnown FontOtf)
  "zip" => Just (mkWellKnown ApplicationZip)
  "gz" => Just (mkWellKnown ApplicationGzip)
  "gzip" => Just (mkWellKnown ApplicationGzip)
  _ => Nothing

||| Content type to common file extension
export
contentTypeToExtension : ContentType -> Maybe String
contentTypeToExtension ct =
  let full = ct.media.mediaType ++ "/" ++ ct.media.subtype
  in case toLower full of
       "text/plain" => Just "txt"
       "text/html" => Just "html"
       "text/css" => Just "css"
       "text/javascript" => Just "js"
       "application/javascript" => Just "js"
       "application/json" => Just "json"
       "application/xml" => Just "xml"
       "text/xml" => Just "xml"
       "application/pdf" => Just "pdf"
       "image/png" => Just "png"
       "image/jpeg" => Just "jpg"
       "image/gif" => Just "gif"
       "image/webp" => Just "webp"
       "image/svg+xml" => Just "svg"
       "audio/mpeg" => Just "mp3"
       "video/mp4" => Just "mp4"
       "video/webm" => Just "webm"
       "font/woff" => Just "woff"
       "font/woff2" => Just "woff2"
       "font/ttf" => Just "ttf"
       "font/otf" => Just "otf"
       "application/zip" => Just "zip"
       "application/gzip" => Just "gz"
       _ => Nothing

--------------------------------------------------------------------------------
-- Accept Header Parsing
--------------------------------------------------------------------------------

||| Parse Accept header media range with quality
export
parseAcceptMediaRange : String -> ContentTypeResult (ContentType, Double)
parseAcceptMediaRange range =
  let parts = split (== ';') (trim range)
  in case parts of
       [] => Err EmptyContentType
       (mediaStr :: params) => do
         ct <- parseContentTypeDefault mediaStr
         let qParam = find (\p => isPrefixOf "q=" (trim p)) params
         let quality = case qParam of
                         Nothing => 1.0
                         Just qStr =>
                           let qVal = trim (drop 2 (trim qStr))
                           in fromMaybe 1.0 (parseDouble qVal)
         Ok (ct, quality)

||| Parse complete Accept header
export
parseAcceptHeader : String -> List (ContentType, Double)
parseAcceptHeader header =
  let ranges = split (== ',') header
  in sortBy (\(_, q1), (_, q2) => compare q2 q1) $
     mapMaybe (\r => case parseAcceptMediaRange r of
                       Ok pair => Just pair
                       Err _ => Nothing) ranges

--------------------------------------------------------------------------------
-- Content Negotiation
--------------------------------------------------------------------------------

||| Find best matching content type from Accept header
export
negotiate : List ContentType -> String -> Maybe ContentType
negotiate available acceptHeader =
  let accepted = parseAcceptHeader acceptHeader
  in findFirst available accepted
  where
    findFirst : List ContentType -> List (ContentType, Double) -> Maybe ContentType
    findFirst _ [] = Nothing
    findFirst avail ((ct, _) :: rest) =
      case find (\a => matches a (show ct.media)) avail of
        Just found => Just found
        Nothing => findFirst avail rest

