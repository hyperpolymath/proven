-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHeaderProps

import Proven.Core
import Proven.SafeHeader
import System

%default total

-- Real API: `header : String -> String -> HeaderResult Header` (default
-- options), `headerWith strictOptions` (RFC 7230 token names), `get` (case-
-- insensitive lookup), `parseLine`, `render`, and the predicate `hasCRLF`.
-- There is no `validateHeaderName`/`sanitizeHeaderValue`: unsafe values are
-- rejected, not rewritten. The validators are `export`, so results are
-- checked by execution.

--------------------------------------------------------------------------------
-- Type-level proofs (the injection predicate the validator uses)
--------------------------------------------------------------------------------

||| The CRLF detector fires on an injection payload.
prop_crlfDetected : hasCRLF "value\r\nEvil: header" = True
prop_crlfDetected = Refl

||| The CRLF detector fires on a response-splitting payload.
prop_responseSplittingDetected : hasCRLF "value\r\n\r\n<html>evil</html>" = True
prop_responseSplittingDetected = Refl

||| A bare LF is also detected.
prop_bareLfDetected : hasCRLF "a\nb" = True
prop_bareLfDetected = Refl

||| An ordinary media type is not flagged.
prop_plainValueClean : hasCRLF "application/json" = False
prop_plainValueClean = Refl

||| RFC 7230 token check: space and colon are not token characters.
prop_tokenRejectsSpaceAndColon : (isValidToken "Invalid Header", isValidToken "Invalid:Header")
                                 = (False, False)
prop_tokenRejectsSpaceAndColon = Refl

||| RFC 7230 token check: standard names are tokens.
prop_tokenAcceptsStandard : (isValidToken "Content-Type", isValidToken "X-Custom-Header")
                            = (True, True)
prop_tokenAcceptsStandard = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: any header value accepted by `header` is CRLF-free (validation
||| replaces the original "sanitiser removes CRLF" intent: the API rejects
||| rather than rewrites). Blocked by: `validateValue` is `export` and
||| `hasCRLF` over an abstract String does not reduce. Names the proposition only.
public export
AcceptedValueNoCRLFObligation : Type
AcceptedValueNoCRLFObligation =
  (n, v : String) -> (h : Header) -> header n v = Ok h -> hasCRLF h.value.value = False

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
strictName : String -> Bool
strictName n = isOk (headerWith strictOptions n "v")

private
lookupAfterAdd : String -> String -> String -> Maybe String
lookupAfterAdd n v q = case add empty n v of
  Ok hs => get hs q
  Err _ => Nothing

private
parsedValue : String -> Maybe String
parsedValue line = case parseLine line of
  Ok h => Just h.value.value
  Err _ => Nothing

||| Test runner for header properties
export
runHeaderProperties : IO ()
runHeaderProperties = do
  putStrLn "SafeHeader Property Tests"
  putStrLn "========================="
  check "valid header names accepted"
    (isOk (header "Content-Type" "text/html") && isOk (header "X-Custom-Header" "1"))
  check "empty header name rejected" (isErr (header "" "v"))
  check "strict options: space and colon in name rejected"
    (not (strictName "Invalid Header") && not (strictName "Invalid:Header") &&
     strictName "Content-Type")
  check "valid header value accepted" (isOk (header "Accept" "application/json"))
  check "CRLF injection in value rejected" (isErr (header "X-A" "value\r\nEvil: header"))
  check "response splitting rejected"
    (isErr (header "X-A" "value\r\n\r\n<html>evil</html>"))
  check "framework-owned headers blocked by default"
    (isErr (header "Transfer-Encoding" "chunked") && isErr (header "Host" "evil"))
  check "header names compare case-insensitively"
    (lookupAfterAdd "Content-Type" "text/html" "content-type" == Just "text/html" &&
     lookupAfterAdd "content-type" "text/html" "CONTENT-TYPE" == Just "text/html")
  check "parseLine splits name and trims value"
    (parsedValue "Content-Type:  text/html " == Just "text/html" &&
     parsedValue "no colon here" == Nothing)
  -- Suspected library defects (reported, expectations kept). Under the
  -- default options `strictTokens = False`, so `validateName` never checks
  -- for CR/LF or ':' in a name, and `hasCRLF` only looks for CR/LF in values.
  check "NUL in header value rejected" (isErr (header "X-A" "value\0"))
  check "CRLF in header name rejected (default options)"
    (isErr (header "X-A\r\nSet-Cookie" "evil"))
  check "colon in header name rejected (default options)"
    (isErr (header "Invalid:Header" "v"))
  putStrLn ""
