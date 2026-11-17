-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeStringProps

import Proven.Core
import Proven.SafeString
import Data.List
import Data.String
import System

%default total

-- Rewritten against the real API: the escapers are `escapeHTML`, `escapeSQL`,
-- `urlEncode`, `escapeShell`; `isValidUTF8` takes `List Bits8` (use
-- `stringToBytes`). There is no `unescapeHtml`, `ValidUTF8 s` predicate,
-- `AlreadyURLEncoded` or `IsASCII` in the library.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| SQL escaping doubles a single quote.
prop_sqlEscapeQuotes : escapeSQL "'" = "''"
prop_sqlEscapeQuotes = Refl

||| The empty byte sequence is valid UTF-8 (was prop_emptyStringValid).
prop_emptyBytesValid : isValidUTF8 [] = True
prop_emptyBytesValid = Refl

||| The empty string encodes to valid UTF-8.
prop_emptyStringValid : isValidUTF8 (stringToBytes "") = True
prop_emptyStringValid = Refl

||| A concrete ASCII byte sequence ("Hi!") is valid UTF-8.
prop_asciiBytesValid : isValidUTF8 [0x48, 0x69, 0x21] = True
prop_asciiBytesValid = Refl

||| A lone continuation byte is rejected.
prop_loneContinuationInvalid : isValidUTF8 [0x80] = False
prop_loneContinuationInvalid = Refl

||| A truncated two-byte sequence is rejected.
prop_truncatedSequenceInvalid : isValidUTF8 [0xC3] = False
prop_truncatedSequenceInvalid = Refl

||| A well-formed two-byte sequence (U+00E9) is accepted.
prop_twoByteSequenceValid : isValidUTF8 [0xC3, 0xA9] = True
prop_twoByteSequenceValid = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

||| OWED: HTML escaping loses no information, i.e. is injective (the library has
||| no `unescapeHTML`, so the original round-trip is stated as injectivity).
||| Blocked by: `pack`/`unpack` over an abstract String do not reduce at the type
||| level and the per-character `go` is a private `where` helper.
||| This names the proposition; it is not a witness.
public export
EscapeHTMLInjectiveObligation : Type
EscapeHTMLInjectiveObligation =
  (s1, s2 : String) -> escapeHTML s1 = escapeHTML s2 -> s1 = s2

||| OWED: HTML escaping never returns empty for non-empty input.
||| Blocked by: `length`/`pack`/`unpack` over an abstract String do not reduce.
||| This names the proposition; it is not a witness.
public export
EscapeHTMLNonEmptyObligation : Type
EscapeHTMLNonEmptyObligation =
  (s : String) -> length s > 0 = True -> length (escapeHTML s) > 0 = True

||| OWED: URL encoding is the identity on strings made only of RFC 3986
||| unreserved characters (the original "idempotent on encoded strings" is false
||| as stated: `%` is itself re-encoded to `%25`). Blocked by: `concat`/`singleton`
||| over an abstract String do not reduce. This names the proposition; it is not
||| a witness.
public export
UrlEncodeUnreservedFixedObligation : Type
UrlEncodeUnreservedFixedObligation =
  (s : String) -> all isURLSafe (unpack s) = True -> urlEncode s = s

||| OWED: shell escaping always wraps its output in single quotes.
||| Blocked by: `++` over an abstract String does not reduce at the type level.
||| This names the proposition; it is not a witness.
public export
ShellEscapeWrapsObligation : Type
ShellEscapeWrapsObligation =
  (s : String) ->
  (startsWith "'" (escapeShell s) && endsWith "'" (escapeShell s)) = True

||| OWED: every ASCII string encodes to valid UTF-8.
||| Blocked by: `unpack` over an abstract String does not reduce, and the
||| validator's fuelled loop is a private `where` helper.
||| This names the proposition; it is not a witness.
public export
AsciiIsValidUTF8Obligation : Type
AsciiIsValidUTF8Obligation =
  (s : String) -> isAscii s = True -> isValidUTF8 (stringToBytes s) = True

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
corpus : List String
corpus = ["", "a", "plain text", "<script>alert('x')</script>", "a & b", "\"q\"",
          "100%", "it's", "x/y?z=1", "tab\there", "&amp;"]

private
noRawHtmlSpecials : String -> Bool
noRawHtmlSpecials s = all (\c => not (c `elem` ['<', '>', '"', '\''])) (unpack s)

private
pairs : List a -> List (a, a)
pairs xs = [(x, y) | x <- xs, y <- xs]

||| Test runner for string properties
export
runStringProperties : IO ()
runStringProperties = do
  putStrLn "SafeString Property Tests"
  putStrLn "========================="
  putStrLn "prop_sqlEscapeQuotes: PASS (proven by type)"
  putStrLn "prop_emptyBytesValid: PASS (proven by type)"
  putStrLn "prop_emptyStringValid: PASS (proven by type)"
  putStrLn "prop_asciiBytesValid: PASS (proven by type)"
  putStrLn "prop_loneContinuationInvalid: PASS (proven by type)"
  putStrLn "prop_truncatedSequenceInvalid: PASS (proven by type)"
  putStrLn "prop_twoByteSequenceValid: PASS (proven by type)"
  check "prop_escapeRoundTrip: escapeHTML injective on corpus"
    (all (\(a, b) => escapeHTML a /= escapeHTML b || a == b) (pairs corpus))
  check "escapeHTML leaves no raw < > \" ' on corpus"
    (all (noRawHtmlSpecials . escapeHTML) corpus)
  check "escapeHTML concrete output"
    (escapeHTML "<a href=\"x\">&</a>" == "&lt;a href=&quot;x&quot;&gt;&amp;&lt;/a&gt;")
  check "prop_escapeNonEmpty: non-empty input gives non-empty output on corpus"
    (all (\s => length s == 0 || length (escapeHTML s) > 0) corpus)
  check "prop_urlEncodeIdempotent: unreserved strings are fixed points"
    (all (\s => urlEncode s == s) ["", "abc123", "A-Z_a.z~0"])
  check "urlEncode escapes reserved characters"
    (urlEncode " " == "%20" && urlEncode "a&b=c" == "a%26b%3Dc" && urlEncode "%" == "%25")
  check "prop_shellEscapeWraps: output starts and ends with a single quote"
    (all (\s => startsWith "'" (escapeShell s) && endsWith "'" (escapeShell s)) corpus)
  check "escapeShell neutralises embedded single quote"
    (escapeShell "it's" == "'it'\\''s'")
  check "prop_asciiIsValidUTF8: ASCII corpus encodes to valid UTF-8"
    (all (\s => not (isAscii s) || isValidUTF8 (stringToBytes s)) corpus)
  putStrLn ""
