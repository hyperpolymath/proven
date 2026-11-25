-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHtmlProps

import Proven.Core
import Proven.SafeHtml
import Proven.SafeString.Escape
import Data.List
import Data.String
import System

%default total

-- Real API: `escapeHtmlContent : String -> String`, the total sanitisers
-- `sanitizeStandard`/`sanitizeStrict : String -> String` (documented as a
-- simplified implementation: strip every tag, then escape), `sanitizeUrl`,
-- `sanitizeAttrValue`, and the policy predicates `isTagAllowed`,
-- `isAttrAllowed`, `isDangerousAttr`, `isBlacklistedTag`. There is no
-- `sanitizeHtml` returning a Result. `Proven.SafeString.Escape` is imported
-- so that `escapeHtmlContent = escapeHTML` unfolds at the type level.
--
-- The original `prop_escapeIdempotent` (escape (escape s) = escape s) is
-- false for any HTML escaper: escape "&" = "&amp;" but escape "&amp;" =
-- "&amp;amp;". It is replaced by an execution check of that counterexample.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| Less-than is escaped.
prop_ltEscaped : escapeHtmlContent "<" = "&lt;"
prop_ltEscaped = Refl

||| Greater-than is escaped.
prop_gtEscaped : escapeHtmlContent ">" = "&gt;"
prop_gtEscaped = Refl

||| Ampersand is escaped.
prop_ampEscaped : escapeHtmlContent "&" = "&amp;"
prop_ampEscaped = Refl

||| Double quote is escaped.
prop_quoteEscaped : escapeHtmlContent "\"" = "&quot;"
prop_quoteEscaped = Refl

||| A whole script tag is neutralised by escaping.
prop_scriptEscaped : escapeHtmlContent "<script>" = "&lt;script&gt;"
prop_scriptEscaped = Refl

||| `javascript:` URLs are refused, case- and whitespace-insensitively.
prop_jsUrlRemoved : (sanitizeUrl "javascript:alert(1)", sanitizeUrl " JavaScript:alert(1)")
                    = (Nothing, Nothing)
prop_jsUrlRemoved = Refl

||| An https URL passes through unchanged.
prop_httpsUrlKept : sanitizeUrl "https://example.com" = Just "https://example.com"
prop_httpsUrlKept = Refl

||| Policy: `p` is an allowed tag and `href` an allowed attribute on `a`.
prop_safeTagAndAttrAllowed : (isTagAllowed Sanitize.standardConfig "p",
                              isAttrAllowed Sanitize.standardConfig "a" "href") = (True, True)
prop_safeTagAndAttrAllowed = Refl

||| Policy: event handlers are dangerous attributes; `script` is blacklisted.
prop_eventHandlerAndScriptBlocked : (isDangerousAttr "onclick",
                                     isBlacklistedTag "script") = (True, True)
prop_eventHandlerAndScriptBlocked = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: escaped output contains no raw `<`, `>` or `"`.
||| Blocked by: `escapeHTML` is `pack (go (unpack s))`; for abstract `s` this
||| needs `unpack (pack xs) = xs`, which is not definitional for the String
||| primitives. Names the proposition only.
public export
EscapeNoRawHtmlObligation : Type
EscapeNoRawHtmlObligation =
  (s : String) ->
  ( elem '<' (unpack (escapeHtmlContent s))
  , elem '>' (unpack (escapeHtmlContent s))
  , elem '"' (unpack (escapeHtmlContent s)) ) = (False, False, False)

||| OWED: sanitised output never contains a `<`, so no tag (script, event
||| handler carrier, javascript: link) can survive. Blocked by: same
||| `pack`/`unpack` round-trip as above. Names the proposition only.
public export
SanitizeNoTagObligation : Type
SanitizeNoTagObligation = (s : String) -> elem '<' (unpack (sanitizeStandard s)) = False

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
noRaw : String -> Bool
noRaw s = not (any (\c => c == '<' || c == '>' || c == '"') (unpack s))

private
attackCorpus : List String
attackCorpus =
  [ "<script>alert('xss')</script>"
  , "<div onclick='evil()'>text</div>"
  , "<a href=\"javascript:alert(1)\">link</a>"
  , "<img src=x onerror=alert(1)>"
  , "<<script>script>", "a < b > c", "\"quoted\" & 'single'" ]

||| Test runner for HTML properties
export
runHtmlProperties : IO ()
runHtmlProperties = do
  putStrLn "SafeHtml Property Tests"
  putStrLn "======================="
  check "plain text unchanged by sanitisation" (sanitizeStandard "Hello World" == "Hello World")
  check "script tag removed" (sanitizeStandard "<script>alert('xss')</script>" == "alert(&#x27;xss&#x27;)")
  check "event-handler attribute removed with its tag"
    (sanitizeStandard "<div onclick='evil()'>text</div>" == "text")
  check "javascript: link removed, link text kept"
    (sanitizeStandard "<a href=\"javascript:alert(1)\">link</a>" == "link")
  check "sanitised attack corpus has no raw < > \" and no dangerous tag"
    (all (\s => noRaw (sanitizeStandard s) &&
                not (isInfixOf "<script" (toLower (sanitizeStandard s))))
         attackCorpus)
  check "escaped output has no raw < > \" (corpus)"
    (all (noRaw . escapeHtmlContent) attackCorpus)
  check "escaping is not idempotent (double escape of < is &amp;lt;)"
    (escapeHtmlContent (escapeHtmlContent "<") == "&amp;lt;")
  check "safe paragraph text preserved" (sanitizeStandard "<p>paragraph</p>" == "paragraph")
  check "safe href attribute value preserved, event handler refused"
    (sanitizeAttrValue "a" "href" "https://example.com" == Just "https://example.com" &&
     sanitizeAttrValue "div" "onclick" "evil()" == Nothing &&
     sanitizeAttrValue "a" "href" "javascript:alert(1)" == Nothing)
  putStrLn ""
