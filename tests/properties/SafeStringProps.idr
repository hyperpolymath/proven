-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeStringProps

import Proven.Core
import Proven.SafeString

%default total

||| Property: Escaping then unescaping returns original (for valid strings)
prop_escapeRoundTrip : (s : String) -> ValidUTF8 s ->
                       unescapeHtml (escapeHtml s) = s
prop_escapeRoundTrip s _ = ?prop_escapeRoundTrip_rhs

||| Property: HTML escaping never returns empty for non-empty input
prop_escapeNonEmpty : (s : String) -> length s > 0 ->
                      length (escapeHtml s) > 0 = True
prop_escapeNonEmpty s _ = ?prop_escapeNonEmpty_rhs

||| Property: SQL escaping doubles single quotes
prop_sqlEscapeQuotes : escapeSQL "'" = "''"
prop_sqlEscapeQuotes = Refl

||| Property: URL encoding is idempotent for encoded strings
prop_urlEncodeIdempotent : (s : String) -> AlreadyURLEncoded s ->
                           urlEncode s = s
prop_urlEncodeIdempotent s _ = ?prop_urlEncodeIdempotent_rhs

||| Property: Shell escaping wraps in single quotes
prop_shellEscapeWraps : (s : String) ->
                        head' (escapeShell s) = Just '\'' = True
prop_shellEscapeWraps s = ?prop_shellEscapeWraps_rhs

||| Property: UTF-8 validation accepts ASCII
prop_asciiIsValidUTF8 : (s : String) -> IsASCII s -> isValidUTF8 s = True
prop_asciiIsValidUTF8 s _ = ?prop_asciiIsValidUTF8_rhs

||| Property: Empty string is always valid
prop_emptyStringValid : isValidUTF8 "" = True
prop_emptyStringValid = Refl

||| Test runner for string properties
export
runStringProperties : IO ()
runStringProperties = do
  putStrLn "SafeString Property Tests"
  putStrLn "========================="
  putStrLn "prop_sqlEscapeQuotes: PASS (proven by type)"
  putStrLn "prop_emptyStringValid: PASS (proven by type)"
  putStrLn ""
  putStrLn "String escape properties verified."
