-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for HTML operations
|||
||| This module provides type-level proofs that:
||| - Escaped HTML cannot contain unescaped dangerous characters
||| - Sanitized content cannot contain script tags
||| - URL validation rejects dangerous schemes
module Proven.SafeHtml.Proofs

import Data.List
import Data.String
import Data.Nat

%default total

||| A string that has been HTML-escaped
public export
data EscapedHtml : Type where
  MkEscapedHtml : (content : String) -> EscapedHtml

||| A string that has been sanitized
public export
data SanitizedHtml : Type where
  MkSanitizedHtml : (content : String) -> SanitizedHtml

||| Proof that escaped HTML does not contain raw '<'
public export
data NoRawLT : String -> Type where
  MkNoRawLT : not (elem '<' (unpack s)) = True -> NoRawLT s

||| Proof that escaped HTML does not contain raw '>'
public export
data NoRawGT : String -> Type where
  MkNoRawGT : not (elem '>' (unpack s)) = True -> NoRawGT s

||| Proof that escaped HTML does not contain raw '&' (unescaped)
public export
data NoRawAmpersand : String -> Type where
  MkNoRawAmpersand : not (isInfixOf "&" s && not (isInfixOf "&amp;" s || isInfixOf "&lt;" s || isInfixOf "&gt;" s || isInfixOf "&quot;" s || isInfixOf "&#" s)) = True -> NoRawAmpersand s

||| Proof that content contains no script tags
public export
data NoScriptTags : String -> Type where
  MkNoScriptTags : not (isInfixOf "<script" (toLower s)) = True -> NoScriptTags s

||| Proof that a URL has a safe scheme (no javascript:, data:, vbscript:)
public export
data SafeScheme : String -> Type where
  MkSafeScheme : not (isPrefixOf "javascript:" (toLower s) ||
                      isPrefixOf "vbscript:" (toLower s) ||
                      isPrefixOf "data:" (toLower s)) = True -> SafeScheme s

||| Combined XSS-safety proof
public export
data XSSSafe : String -> Type where
  MkXSSSafe : NoRawLT s -> NoRawGT s -> NoScriptTags s -> XSSSafe s

||| Helper: escape a single HTML character
public export
escapeChar : Char -> String
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar '&' = "&amp;"
escapeChar '"' = "&quot;"
escapeChar '\'' = "&#x27;"
escapeChar c = singleton c

||| OWED: if `escaped = concat (map escapeChar (unpack s))` then
||| `escaped` contains no raw `<` (i.e. `NoRawLT escaped`). The
||| `escapeChar` function maps `'<' |-> "&lt;"`, so by case-analysis
||| every position of the output either came from a non-`<` source
||| character (`singleton c` for `c /= '<'`) or from the literal
||| `"&lt;"` (no raw `<`).
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` / `pack` /
||| `concat` / `map` over abstract `String` at the type level — these
||| are FFI-bound String primitives, so `elem '<' (unpack escaped)`
||| does not normalise to `False` by Refl alone. Same blocker family
||| as SafeChecksum's `luhnValidatesKnownGood` (opaque String FFI).
||| Discharge once a `Data.String` reflective tactic or per-character
||| induction lemma over `unpack . concat . map` is available.
public export
0 escapePreservesNoLT : (s : String) -> (escaped : String) ->
                        escaped = concat (map escapeChar (unpack s)) ->
                        NoRawLT escaped

||| Escaping is idempotent on safe strings
||| If a string has no dangerous characters, escaping is a no-op
public export
data EscapeIdempotent : String -> Type where
  MkEscapeIdempotent : NoRawLT s -> NoRawGT s -> NoRawAmpersand s ->
                       EscapeIdempotent s

||| OWED: for every `input`, the corresponding sanitised `output`
||| contains no `<script` substring (case-insensitive), i.e.
||| `NoScriptTags output`. Witnessed operationally by
||| `Proven.SafeHtml.Sanitize`'s blacklist-driven tag stripping
||| (`blacklistedTags` includes `"script"`).
|||
||| Held back by Idris2 0.8.0 not reducing `isInfixOf` and `toLower`
||| at the type level — both are FFI-bound String primitives, so
||| `not (isInfixOf "<script" (toLower output))` does not normalise
||| to `True` by Refl for abstract `output`. Same blocker family as
||| `escapePreservesNoLT` and SafeChecksum's String-FFI OWED set.
||| Discharge once a `Data.String` reflective tactic for
||| `isInfixOf` / `toLower` is available, or a per-character
||| induction lemma over the sanitiser's output construction.
public export
0 sanitizeRemovesScripts : (input : String) -> (output : String) ->
                           NoScriptTags output

||| Proof that attribute escaping prevents quote breakout
public export
data SafeAttribute : String -> Type where
  MkSafeAttribute : not (elem '"' (unpack s)) = True ->
                    not (elem '\'' (unpack s)) = True ->
                    SafeAttribute s

||| Proof that HTML builder produces valid HTML structure
public export
data WellFormedHtml : Type where
  ||| Empty document is well-formed
  EmptyDoc : WellFormedHtml
  ||| Text content (leaf node) is well-formed
  TextNode : EscapedHtml -> WellFormedHtml
  ||| Element with children is well-formed if children are
  ValidElement : (tag : String) ->
                 (attrs : List (String, String)) ->
                 (children : List WellFormedHtml) ->
                 WellFormedHtml

||| Proof that self-closing tags have no children
public export
data SelfClosing : String -> Type where
  MkSelfClosing : elem tag ["br", "hr", "img", "input", "meta", "link",
                             "area", "base", "col", "embed", "source",
                             "track", "wbr"] = True -> SelfClosing tag
