-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeContentTypeProps

import Proven.Core
import Proven.SafeContentType
import Data.List
import System

%default total

-- Real API: `parse : String -> ContentTypeResult ContentType` (with
-- `render : ContentType -> String`); the media type, subtype, charset and
-- boundary are record fields (`ct.media.mediaType`, `ct.media.subtype`,
-- `ct.charset`, `ct.boundary`, `ct.params`). The parser
-- (`Parser.parseContentType`) is `export`, not `public export`, so closed
-- parse results do not reduce at the type level; they are execution checks.

||| OWED: render/parse round-trip: anything the parser accepts re-parses
||| from its rendering to the same value. Blocked by: `parseContentType` is
||| `export` (opaque here) and built on String `split`/`trim`/`toLower`
||| primitives. This names the proposition; it is not a witness.
public export
ContentTypeRenderParseObligation : Type
ContentTypeRenderParseObligation =
  (s : String) -> (ct : ContentType) -> parse s = Ok ct -> parse (render ct) = Ok ct

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
okAnd : (ContentType -> Bool) -> ContentTypeResult ContentType -> Bool
okAnd p (Ok ct) = p ct
okAnd _ (Err _) = False

private
corpus : List String
corpus = ["text/html", "text/html; charset=utf-8", "application/json",
          "multipart/form-data; boundary=----WebKitFormBoundary", "*/*", "text/*",
          "text/html; level=1", "TEXT/HTML; Charset=UTF-8", "application/vnd.api+json",
          "text/plain; charset=\"iso-8859-1\""]

private
renderParses : String -> Bool
renderParses s = case parse s of
  Err _ => False
  Ok ct => okAnd (\ct' => ct' == ct && render ct' == render ct) (parse (render ct))

||| Test runner for content type properties
export
runContentTypeProperties : IO ()
runContentTypeProperties = do
  putStrLn "SafeContentType Property Tests"
  putStrLn "=============================="
  check "valid MIME type text/html parses" (isOk (parse "text/html"))
  check "MIME type with charset parses; charset extracted as utf-8"
    (okAnd (\ct => ct.charset == Just UTF8) (parse "text/html; charset=utf-8"))
  check "application/json parses" (isOk (parse "application/json"))
  check "multipart/form-data with boundary parses; boundary extracted"
    (okAnd (\ct => ct.boundary == Just "----WebKitFormBoundary")
           (parse "multipart/form-data; boundary=----WebKitFormBoundary"))
  check "multipart/form-data without boundary is rejected"
    (isErr (parse "multipart/form-data"))
  check "invalid format (no slash) fails" (isErr (parse "invalid"))
  check "empty type fails" (isErr (parse ""))
  check "type extraction: text/html -> text"
    (okAnd (\ct => ct.media.mediaType == "text") (parse "text/html"))
  check "subtype extraction: text/html -> html"
    (okAnd (\ct => ct.media.subtype == "html") (parse "text/html"))
  check "parameter extraction: level=1 kept as a parameter"
    (okAnd (\ct => map show ct.params == ["level=1"]) (parse "text/html; level=1"))
  check "wildcard type */* parses" (isOk (parse "*/*"))
  check "wildcard subtype text/* parses" (isOk (parse "text/*"))
  check "wildcard accepted as Accept media range"
    (map (render . fst) (parseAccept "*/*") == ["*/*"])
  check "common MIME types recognised"
    (all (isOk . parse) ["text/plain", "text/css", "text/javascript", "application/xml",
                         "application/pdf", "image/png", "image/jpeg"])
  check "formatting is consistent: parse (render ct) = ct on corpus"
    (all renderParses corpus)
  putStrLn ""
