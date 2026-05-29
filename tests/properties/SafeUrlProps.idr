-- SPDX-License-Identifier: MPL-2.0
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

||| OWED: Relative URL is rejected (requires scheme)
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_relativeUrlFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_relativeUrlFails : isErr (parseUrl "/path/to/resource") = True

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
