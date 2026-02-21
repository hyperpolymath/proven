-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCookieProps

import Proven.Core
import Proven.SafeCookie

%default total

||| Property: Valid cookie name parses
prop_validCookieName : isOk (validateCookieName "session_id") = True
prop_validCookieName = Refl

||| Property: Empty cookie name fails
prop_emptyCookieNameFails : isErr (validateCookieName "") = True
prop_emptyCookieNameFails = Refl

||| Property: Cookie name with equals fails
prop_cookieNameEqualsFails : isErr (validateCookieName "name=value") = True
prop_cookieNameEqualsFails = Refl

||| Property: Cookie name with semicolon fails
prop_cookieNameSemicolonFails : isErr (validateCookieName "name;evil") = True
prop_cookieNameSemicolonFails = Refl

||| Property: Valid cookie value parses
prop_validCookieValue : isOk (validateCookieValue "abc123") = True
prop_validCookieValue = Refl

||| Property: Cookie value with semicolon fails
prop_cookieValueSemicolonFails : isErr (validateCookieValue "value;HttpOnly") = True
prop_cookieValueSemicolonFails = Refl

||| Property: Cookie value with newline fails
prop_cookieValueNewlineFails : isErr (validateCookieValue "value\nSet-Cookie: evil") = True
prop_cookieValueNewlineFails = Refl

||| Property: HttpOnly attribute works
prop_httpOnlyAttribute : hasAttribute HttpOnly (setCookie "name" "value" [HttpOnly]) = True
prop_httpOnlyAttribute = Refl

||| Property: Secure attribute works
prop_secureAttribute : hasAttribute Secure (setCookie "name" "value" [Secure]) = True
prop_secureAttribute = Refl

||| Property: SameSite attribute works
prop_sameSiteAttribute : hasAttribute (SameSite Strict) (setCookie "name" "value" [SameSite Strict]) = True
prop_sameSiteAttribute = Refl

||| Property: Domain validation
prop_domainValidation : isOk (validateCookieDomain "example.com") = True
prop_domainValidation = Refl

||| Property: Path validation
prop_pathValidation : isOk (validateCookiePath "/api/v1") = True
prop_pathValidation = Refl

||| Property: Max-Age validation
prop_maxAgeValidation : isOk (validateMaxAge 3600) = True
prop_maxAgeValidation = Refl

||| Property: Negative Max-Age fails
prop_negativeMaxAgeFails : isErr (validateMaxAge (-1)) = True
prop_negativeMaxAgeFails = Refl

||| Test runner for cookie properties
export
runCookieProperties : IO ()
runCookieProperties = do
  putStrLn "SafeCookie Property Tests"
  putStrLn "========================="
  putStrLn "prop_validCookieName: PASS (proven by type)"
  putStrLn "prop_emptyCookieNameFails: PASS (proven by type)"
  putStrLn "prop_cookieNameEqualsFails: PASS (proven by type)"
  putStrLn "prop_cookieNameSemicolonFails: PASS (proven by type)"
  putStrLn "prop_validCookieValue: PASS (proven by type)"
  putStrLn "prop_cookieValueSemicolonFails: PASS (proven by type)"
  putStrLn "prop_cookieValueNewlineFails: PASS (proven by type)"
  putStrLn "prop_httpOnlyAttribute: PASS (proven by type)"
  putStrLn "prop_secureAttribute: PASS (proven by type)"
  putStrLn "prop_sameSiteAttribute: PASS (proven by type)"
  putStrLn "prop_domainValidation: PASS (proven by type)"
  putStrLn "prop_pathValidation: PASS (proven by type)"
  putStrLn "prop_maxAgeValidation: PASS (proven by type)"
  putStrLn "prop_negativeMaxAgeFails: PASS (proven by type)"
  putStrLn ""
