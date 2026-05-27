-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHeaderProps

import Proven.Core
import Proven.SafeHeader

%default total

||| Property: Valid header name parses
prop_validHeaderName : isOk (validateHeaderName "Content-Type") = True
prop_validHeaderName = Refl

||| Property: Standard header names parse
prop_standardHeaderNames : isOk (validateHeaderName "X-Custom-Header") = True
prop_standardHeaderNames = Refl

||| Property: Empty header name fails
prop_emptyHeaderNameFails : isErr (validateHeaderName "") = True
prop_emptyHeaderNameFails = Refl

||| Property: Header name with space fails
prop_headerNameSpaceFails : isErr (validateHeaderName "Invalid Header") = True
prop_headerNameSpaceFails = Refl

||| Property: Header name with colon fails
prop_headerNameColonFails : isErr (validateHeaderName "Invalid:Header") = True
prop_headerNameColonFails = Refl

||| Property: Valid header value parses
prop_validHeaderValue : isOk (validateHeaderValue "application/json") = True
prop_validHeaderValue = Refl

||| Property: Header value with newline fails (CRLF injection)
prop_crlfInjectionBlocked : isErr (validateHeaderValue "value\r\nEvil: header") = True
prop_crlfInjectionBlocked = Refl

||| OWED: Header value with null fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_nullInValueFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_nullInValueFails : isErr (validateHeaderValue "value\0") = True

||| Property: Header comparison is case-insensitive for names
prop_caseInsensitiveNames : headerNameEq "Content-Type" "content-type" = True
prop_caseInsensitiveNames = Refl

||| Property: Response splitting blocked
prop_responseSplittingBlocked : isErr (validateHeaderValue "value\r\n\r\n<html>evil</html>") = True
prop_responseSplittingBlocked = Refl

||| OWED: Sanitize removes dangerous characters
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sanitizeRemovesDangerous_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sanitizeRemovesDangerous : containsCRLF (sanitizeHeaderValue "test\r\nvalue") = False

||| Test runner for header properties
export
runHeaderProperties : IO ()
runHeaderProperties = do
  putStrLn "SafeHeader Property Tests"
  putStrLn "========================="
  putStrLn "prop_validHeaderName: PASS (proven by type)"
  putStrLn "prop_standardHeaderNames: PASS (proven by type)"
  putStrLn "prop_emptyHeaderNameFails: PASS (proven by type)"
  putStrLn "prop_headerNameSpaceFails: PASS (proven by type)"
  putStrLn "prop_headerNameColonFails: PASS (proven by type)"
  putStrLn "prop_validHeaderValue: PASS (proven by type)"
  putStrLn "prop_crlfInjectionBlocked: PASS (proven by type)"
  putStrLn "prop_caseInsensitiveNames: PASS (proven by type)"
  putStrLn "prop_responseSplittingBlocked: PASS (proven by type)"
  putStrLn ""
