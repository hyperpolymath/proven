-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeMediaType - RFC 6838 Media Type parsing and validation
|||
||| Provides strict media type parsing with parameter handling.
||| Prevents: content type confusion, MIME sniffing attacks.
module Proven.SafeMediaType

import Data.String
import Data.List
import Data.List1
import Data.Nat

%default total

||| Top-level media type categories
public export
data TypeCategory =
    Application | Audio | Font | Example | Image
  | Message | Model | Multipart | Text | Video

public export
Show TypeCategory where
  show Application = "application"; show Audio = "audio"
  show Font = "font"; show Example = "example"; show Image = "image"
  show Message = "message"; show Model = "model"
  show Multipart = "multipart"; show Text = "text"; show Video = "video"

public export
Eq TypeCategory where
  Application == Application = True; Audio == Audio = True
  Font == Font = True; Example == Example = True; Image == Image = True
  Message == Message = True; Model == Model = True
  Multipart == Multipart = True; Text == Text = True; Video == Video = True
  _ == _ = False

||| Parse category string
public export
parseCategory : String -> Maybe TypeCategory
parseCategory "application" = Just Application
parseCategory "audio"       = Just Audio
parseCategory "font"        = Just Font
parseCategory "example"     = Just Example
parseCategory "image"       = Just Image
parseCategory "message"     = Just Message
parseCategory "model"       = Just Model
parseCategory "multipart"   = Just Multipart
parseCategory "text"        = Just Text
parseCategory "video"       = Just Video
parseCategory _             = Nothing

||| A media type parameter (key=value)
public export
record MediaParam where
  constructor MkMediaParam
  paramName  : String
  paramValue : String

public export
Eq MediaParam where
  a == b = a.paramName == b.paramName && a.paramValue == b.paramValue

||| A parsed media type
public export
record MediaType where
  constructor MkMediaType
  category : TypeCategory
  subtype  : String
  params   : List MediaParam

public export
Eq MediaType where
  a == b = a.category == b.category && a.subtype == b.subtype

public export
Show MediaType where
  show mt = show mt.category ++ "/" ++ mt.subtype ++
    fastConcat (map (\p => "; " ++ p.paramName ++ "=" ++ p.paramValue) mt.params)

||| Parse a media type string (e.g., "text/html; charset=utf-8")
public export
parseMediaType : String -> Maybe MediaType
parseMediaType s =
  case forget (split (== ';') s) of
    [] => Nothing
    (typePart :: paramParts) =>
      case break (== '/') (unpack (trim typePart)) of
        (catChars, '/' :: subChars) => do
          cat <- parseCategory (pack catChars)
          let sub = pack subChars
          if length sub > 0
            then Just (MkMediaType cat sub (mapMaybe parseParam paramParts))
            else Nothing
        _ => Nothing
  where
    trim : String -> String
    trim = pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack

    parseParam : String -> Maybe MediaParam
    parseParam str =
      case break (== '=') (unpack (trim str)) of
        (key, '=' :: val) =>
          let k = pack key
              v = pack val
          in if length k > 0 then Just (MkMediaParam k v) else Nothing
        _ => Nothing

||| Check if media type is a known safe type for display
public export
isSafeForDisplay : MediaType -> Bool
isSafeForDisplay mt = case mt.category of
  Text  => mt.subtype == "plain" || mt.subtype == "html" || mt.subtype == "css"
  Image => mt.subtype == "png" || mt.subtype == "jpeg" || mt.subtype == "gif" ||
           mt.subtype == "webp" || mt.subtype == "svg+xml"
  Audio => True
  Video => True
  _     => False

||| Check if media type is executable/dangerous
public export
isDangerous : MediaType -> Bool
isDangerous mt = case mt.category of
  Application => mt.subtype == "javascript" || mt.subtype == "x-javascript" ||
                 mt.subtype == "x-shockwave-flash" || mt.subtype == "x-msdownload" ||
                 mt.subtype == "octet-stream"
  _           => False

||| Get charset parameter if present
public export
getCharset : MediaType -> Maybe String
getCharset mt = map (.paramValue) (find (\p => p.paramName == "charset") mt.params)

||| Common media types
public export
textPlain : MediaType
textPlain = MkMediaType Text "plain" [MkMediaParam "charset" "utf-8"]

public export
applicationJson : MediaType
applicationJson = MkMediaType Application "json" [MkMediaParam "charset" "utf-8"]

public export
textHtml : MediaType
textHtml = MkMediaType Text "html" [MkMediaParam "charset" "utf-8"]

public export
applicationOctetStream : MediaType
applicationOctetStream = MkMediaType Application "octet-stream" []
