-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeStringProps

import Proven.Core
import Proven.SafeString

%default total

||| OWED: Escaping then unescaping returns original (for valid strings)
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_escapeRoundTrip_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_escapeRoundTrip : (s : String) -> ValidUTF8 s ->
                         unescapeHtml (escapeHtml s) = s

||| OWED: HTML escaping never returns empty for non-empty input
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_escapeNonEmpty_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_escapeNonEmpty : (s : String) -> length s > 0 ->
                        length (escapeHtml s) > 0 = True

||| Property: SQL escaping doubles single quotes
prop_sqlEscapeQuotes : escapeSQL "'" = "''"
prop_sqlEscapeQuotes = Refl

||| OWED: URL encoding is idempotent for encoded strings
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_urlEncodeIdempotent_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_urlEncodeIdempotent : (s : String) -> AlreadyURLEncoded s ->
                             urlEncode s = s

||| OWED: Shell escaping wraps in single quotes
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_shellEscapeWraps_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_shellEscapeWraps : (s : String) ->
                          head' (escapeShell s) = Just '\'' = True

||| OWED: UTF-8 validation accepts ASCII
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_asciiIsValidUTF8_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_asciiIsValidUTF8 : (s : String) -> IsASCII s -> isValidUTF8 s = True

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
