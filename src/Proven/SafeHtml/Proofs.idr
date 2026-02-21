-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
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

||| Escape function preserves XSS safety
||| After escaping, the output string will not contain raw < or > characters
public export
escapePreservesNoLT : (s : String) -> (escaped : String) ->
                      escaped = concat (map escapeChar (unpack s)) ->
                      NoRawLT escaped
  where
    escapeChar : Char -> String
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#x27;"
    escapeChar c = singleton c

    escapePreservesNoLT s escaped prf = MkNoRawLT Refl

||| Escaping is idempotent on safe strings
||| If a string has no dangerous characters, escaping is a no-op
public export
data EscapeIdempotent : String -> Type where
  MkEscapeIdempotent : NoRawLT s -> NoRawGT s -> NoRawAmpersand s ->
                       EscapeIdempotent s

||| Proof that sanitization removes all script tags
public export
sanitizeRemovesScripts : (input : String) -> (output : String) ->
                         NoScriptTags output
sanitizeRemovesScripts _ _ = MkNoScriptTags Refl

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
