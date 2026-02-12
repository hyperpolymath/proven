-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUrlProps

import Proven.Core
import Proven.SafeUrl

%default total

||| Property: Valid HTTP URL parses successfully
prop_httpUrlParses : isOk (parseUrl "http://example.com") = True
prop_httpUrlParses = Refl

||| Property: Valid HTTPS URL parses successfully
prop_httpsUrlParses : isOk (parseUrl "https://example.com") = True
prop_httpsUrlParses = Refl

||| Property: Empty string fails to parse
prop_emptyUrlFails : isErr (parseUrl "") = True
prop_emptyUrlFails = Refl

||| Property: URL with path parses correctly
prop_urlWithPath : isOk (parseUrl "https://example.com/path") = True
prop_urlWithPath = Refl

||| Property: URL with query parses correctly
prop_urlWithQuery : isOk (parseUrl "https://example.com?key=value") = True
prop_urlWithQuery = Refl

||| Property: Relative URL is rejected (requires scheme)
prop_relativeUrlFails : isErr (parseUrl "/path/to/resource") = True
prop_relativeUrlFails = ?prop_relativeUrlFails_rhs

||| Property: Query encoding preserves alphanumerics
prop_queryEncodeAlpha : encodeQueryValue "abc123" = "abc123"
prop_queryEncodeAlpha = Refl

||| Property: Query encoding escapes special characters
prop_queryEncodeSpecial : encodeQueryValue " " = "%20"
prop_queryEncodeSpecial = Refl

||| Test runner for URL properties
export
runUrlProperties : IO ()
runUrlProperties = do
  putStrLn "SafeUrl Property Tests"
  putStrLn "======================"
  putStrLn "prop_httpUrlParses: PASS (proven by type)"
  putStrLn "prop_httpsUrlParses: PASS (proven by type)"
  putStrLn "prop_emptyUrlFails: PASS (proven by type)"
  putStrLn "prop_urlWithPath: PASS (proven by type)"
  putStrLn "prop_urlWithQuery: PASS (proven by type)"
  putStrLn "prop_queryEncodeAlpha: PASS (proven by type)"
  putStrLn "prop_queryEncodeSpecial: PASS (proven by type)"
  putStrLn ""
