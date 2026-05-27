-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeContentTypeProps

import Proven.Core
import Proven.SafeContentType

%default total

||| Property: Valid MIME type parses
prop_validMimeType : isOk (parseContentType "text/html") = True
prop_validMimeType = Refl

||| Property: MIME type with charset parses
prop_mimeWithCharset : isOk (parseContentType "text/html; charset=utf-8") = True
prop_mimeWithCharset = Refl

||| Property: Application JSON parses
prop_applicationJson : isOk (parseContentType "application/json") = True
prop_applicationJson = Refl

||| Property: Multipart form-data parses
prop_multipartFormData : isOk (parseContentType "multipart/form-data; boundary=----WebKitFormBoundary") = True
prop_multipartFormData = Refl

||| Property: Invalid format fails
prop_invalidFormatFails : isErr (parseContentType "invalid") = True
prop_invalidFormatFails = Refl

||| Property: Empty type fails
prop_emptyTypeFails : isErr (parseContentType "") = True
prop_emptyTypeFails = Refl

||| OWED: Type extraction works
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_typeExtraction_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_typeExtraction : getType (parseContentType "text/html") = Just "text"

||| OWED: Subtype extraction works
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_subtypeExtraction_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_subtypeExtraction : getSubtype (parseContentType "text/html") = Just "html"

||| OWED: Parameter extraction works
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_parameterExtraction_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_parameterExtraction : getParameter "charset" (parseContentType "text/html; charset=utf-8") = Just "utf-8"

||| Property: Wildcard type parses
prop_wildcardType : isOk (parseContentType "*/*") = True
prop_wildcardType = Refl

||| Property: Wildcard subtype parses
prop_wildcardSubtype : isOk (parseContentType "text/*") = True
prop_wildcardSubtype = Refl

||| Property: Common MIME types recognized
prop_commonMimeTypes : all isOk [parseContentType "text/plain",
                                  parseContentType "text/css",
                                  parseContentType "text/javascript",
                                  parseContentType "application/xml",
                                  parseContentType "application/pdf",
                                  parseContentType "image/png",
                                  parseContentType "image/jpeg"] = True
prop_commonMimeTypes = Refl

||| OWED: Formatting is consistent
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_formatConsistent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_formatConsistent : (ct : ValidContentType) ->
                          parseContentType (format ct) = Ok ct

||| Test runner for content type properties
export
runContentTypeProperties : IO ()
runContentTypeProperties = do
  putStrLn "SafeContentType Property Tests"
  putStrLn "=============================="
  putStrLn "prop_validMimeType: PASS (proven by type)"
  putStrLn "prop_mimeWithCharset: PASS (proven by type)"
  putStrLn "prop_applicationJson: PASS (proven by type)"
  putStrLn "prop_multipartFormData: PASS (proven by type)"
  putStrLn "prop_invalidFormatFails: PASS (proven by type)"
  putStrLn "prop_emptyTypeFails: PASS (proven by type)"
  putStrLn "prop_wildcardType: PASS (proven by type)"
  putStrLn "prop_wildcardSubtype: PASS (proven by type)"
  putStrLn "prop_commonMimeTypes: PASS (proven by type)"
  putStrLn ""
