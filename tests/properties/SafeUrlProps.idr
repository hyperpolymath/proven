-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeUrlProps

import Proven.Core
-- The URL parser and query helpers live in the sub-modules; `Proven.SafeUrl`
-- itself does not re-export them.
import Proven.SafeUrl.Parser
import Proven.SafeUrl.Query
import Proven.SafeUrl.Proofs
import Data.Maybe
import System

%default total

-- Rewritten against the real API: `parseURL : String -> Either URLParseError
-- ParsedURL` (plus `parseURLMaybe`, `isValidURL`) and `urlEncode`/`urlDecode`.
-- There is no `parseUrl` or `encodeQueryValue`.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| The empty string is rejected (was prop_emptyUrlFails).
prop_emptyUrlFails : parseURLMaybe "" = Nothing
prop_emptyUrlFails = Refl

||| The empty string is not a valid URL.
prop_emptyUrlInvalid : isValidURL "" = False
prop_emptyUrlInvalid = Refl

||| URL-encoding the empty string yields the empty string.
prop_encodeEmpty : urlEncode "" = ""
prop_encodeEmpty = Refl

||| Adding a query parameter increases the parameter count by one (library theorem).
prop_addParamCount : (k, v : String) -> (qb : QueryBuilder) ->
                     paramCount (addParam k v qb).params = S (paramCount qb.params)
prop_addParamCount = addParamIncreasesCount

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

||| OWED: encoding preserves unreserved alphanumerics (was prop_queryEncodeAlpha,
||| generalised). Blocked by: `isAlphaNum` is a Char primitive and `concat`/
||| `unpack` do not reduce over an abstract String. This names the proposition;
||| it is not a witness.
public export
UrlEncodeAlnumFixedObligation : Type
UrlEncodeAlnumFixedObligation =
  (s : String) -> all isAlphaNum (unpack s) = True -> urlEncode s = s

||| OWED: decoding inverts encoding. Blocked by: String/Char primitives in
||| `urlEncode`/`urlDecode` do not reduce for an abstract String.
||| This names the proposition; it is not a witness.
public export
UrlEncodeDecodeObligation : Type
UrlEncodeDecodeObligation = (s : String) -> urlDecode (urlEncode s) = Just s

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

||| Apply a predicate to the parse result; a parse error fails the check.
private
parsedSatisfies : String -> (ParsedURL -> Bool) -> Bool
parsedSatisfies s p = case parseURL s of
  Right u => p u
  Left _ => False

private
isHost : String -> ParsedURL -> Bool
isHost h u = u.host == Just (Domain h)

||| Test runner for URL properties
export
runUrlProperties : IO ()
runUrlProperties = do
  putStrLn "SafeUrl Property Tests"
  putStrLn "======================"
  putStrLn "prop_emptyUrlFails: PASS (proven by type)"
  putStrLn "prop_emptyUrlInvalid: PASS (proven by type)"
  putStrLn "prop_encodeEmpty: PASS (proven by type)"
  putStrLn "prop_addParamCount: PASS (proven by type)"
  check "prop_httpUrlParses: http scheme and host"
    (parsedSatisfies "http://example.com"
       (\u => u.scheme == Just HTTP && isHost "example.com" u && u.path == []))
  check "prop_httpsUrlParses: https scheme and host"
    (parsedSatisfies "https://example.com" (\u => u.scheme == Just HTTPS && isHost "example.com" u))
  check "prop_urlWithPath: path segments"
    (parsedSatisfies "https://example.com/path/to" (\u => u.path == ["path", "to"]))
  check "prop_urlWithQuery: query pairs without a path"
    (parsedSatisfies "https://example.com?key=value"
       (\u => isHost "example.com" u && u.query == [("key", "value")]))
  check "port, userinfo, fragment"
    (parsedSatisfies "https://bob@example.com:8443/a?x=1#frag"
       (\u => u.port == Just 8443 && isHost "example.com" u &&
              map (.username) u.userInfo == Just "bob" &&
              u.path == ["a"] && u.fragment == Just "frag"))
  check "prop_relativeUrlFails: a relative reference carries no scheme or host"
    (parsedSatisfies "/path/to/resource"
       (\u => isNothing u.scheme && isNothing u.host && u.path == ["path", "to", "resource"]))
  check "prop_queryEncodeAlpha: alphanumerics unchanged"
    (urlEncode "abc123" == "abc123")
  check "prop_queryEncodeSpecial: space and reserved characters escaped"
    (urlEncode " " == "%20" && urlEncode "a&b=c" == "a%26b%3Dc")
  check "urlDecode inverts urlEncode on samples"
    (all (\s => urlDecode (urlEncode s) == Just s) ["", "abc", "a b&c=d/e?f", "100%"])
  check "urlDecode rejects a malformed escape"
    (isNothing (urlDecode "%zz"))
  check "query builder round-trips through parseQueryString"
    (parseQueryString (buildQueryString (addParam "q" "a b" (addParam "n" "1" emptyQuery)))
       == [("n", "1"), ("q", "a b")])
  putStrLn ""
