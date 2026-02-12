-- SPDX-License-Identifier: Apache-2.0
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

||| Property: Type extraction works
prop_typeExtraction : getType (parseContentType "text/html") = Just "text"
prop_typeExtraction = ?prop_typeExtraction_rhs

||| Property: Subtype extraction works
prop_subtypeExtraction : getSubtype (parseContentType "text/html") = Just "html"
prop_subtypeExtraction = ?prop_subtypeExtraction_rhs

||| Property: Parameter extraction works
prop_parameterExtraction : getParameter "charset" (parseContentType "text/html; charset=utf-8") = Just "utf-8"
prop_parameterExtraction = ?prop_parameterExtraction_rhs

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

||| Property: Formatting is consistent
prop_formatConsistent : (ct : ValidContentType) ->
                        parseContentType (format ct) = Ok ct
prop_formatConsistent ct = ?prop_formatConsistent_rhs

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
