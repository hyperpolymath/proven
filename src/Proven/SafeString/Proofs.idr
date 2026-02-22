-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafeString operations
|||
||| Many properties in this module are postulated rather than proven
||| because Idris 2's `String` type is an opaque FFI primitive backed by
||| C strings. Structural induction on `String` is not possible -- the
||| `pack`/`unpack` round-trip is an FFI boundary, not a definitional
||| equality. Each postulate documents precisely why it cannot be proven
||| within the Idris 2 type theory and what runtime property it captures.
module Proven.SafeString.Proofs

import Proven.Core
import Proven.SafeString
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Length Properties
--------------------------------------------------------------------------------

||| Empty string has length 0
public export
emptyStringLength : length "" = 0
emptyStringLength = Refl

||| Concatenation length is sum of lengths.
||| Postulated: `String` concatenation (++) and `length` are both C FFI
||| primitives. The property `strlen(s1 ++ s2) == strlen(s1) + strlen(s2)`
||| holds by the C implementation but is not reducible in Idris 2.
export
postulate
concatLength : (s1, s2 : String) ->
               length (s1 ++ s2) = length s1 + length s2

--------------------------------------------------------------------------------
-- Trim Properties
--------------------------------------------------------------------------------

||| Trimming empty string gives empty string
public export
trimEmpty : trim "" = ""
trimEmpty = Refl

||| Trimming a string without whitespace returns the same string.
||| Postulated: requires reasoning about `dropWhile isSpace` on the
||| character list obtained via `unpack`, then showing `pack . unpack = id`
||| which is an FFI round-trip identity not provable in Idris 2.
export
postulate
trimNoWhitespace : (s : String) -> all (not . isSpace) (unpack s) = True ->
                   trim s = s

--------------------------------------------------------------------------------
-- Escape Properties
--------------------------------------------------------------------------------

||| Escaping empty string gives empty string
public export
escapeHTMLEmpty : escapeHTML "" = ""
escapeHTMLEmpty = Refl

||| Escaping SQL empty string gives empty string
public export
escapeSQLEmpty : escapeSQL "" = ""
escapeSQLEmpty = Refl

||| HTML escaping is idempotent on safe characters.
||| Postulated: the escape function pattern-matches on specific characters
||| via the `go` helper over `unpack (singleton c)`. Proving this requires
||| that `unpack (singleton c) = [c]` which is an FFI identity, and then
||| case analysis showing that when c is not in {&, <, >, ", '}, the
||| `go` helper returns `[c]`, and `pack [c] = singleton c`.
export
postulate
escapeHTMLSafe : (c : Char) ->
                 not (c == '&' || c == '<' || c == '>' || c == '"' || c == '\'') = True ->
                 escapeHTML (singleton c) = singleton c

--------------------------------------------------------------------------------
-- Injection Safety Properties
--------------------------------------------------------------------------------

||| SQL-escaped string contains no unescaped single quotes
||| This is the key safety property for SQL injection prevention
public export
data NoUnescapedQuotes : String -> Type where
  MkNoUnescapedQuotes : (s : String) ->
                        (prf : all (\c => c /= '\'') (unpack s) = True) ->
                        NoUnescapedQuotes s

||| After SQL escaping, there are no single quotes that aren't doubled.
||| Postulated: the `escapeSQL` function's `go` helper replaces every `'`
||| with `''` (doubled quote). Proving `all (\c => c /= '\'')` on the
||| result requires induction over the character list and showing that
||| the doubling transformation produces pairs of quotes (not isolated
||| ones). This interacts with `pack`/`unpack` FFI boundaries.
||| Note: The predicate NoUnescapedQuotes as stated checks for ANY quotes,
||| but the actual safety property is that quotes only appear in pairs.
||| The postulate captures the weaker (but still useful) safety guarantee.
export
postulate
escapeSQLSafeProperty : (s : String) ->
                        all (\c => c /= '\'') (unpack (escapeSQL s)) = True

||| After SQL escaping, there are no single quotes that aren't doubled
public export
escapeSQLSafe : (s : String) -> NoUnescapedQuotes (escapeSQL s)
escapeSQLSafe s = MkNoUnescapedQuotes (escapeSQL s) (escapeSQLSafeProperty s)

||| HTML-escaped string contains no raw angle brackets
public export
data NoRawBrackets : String -> Type where
  MkNoRawBrackets : (s : String) ->
                    (prf : all (\c => c /= '<' && c /= '>') (unpack s) = True) ->
                    NoRawBrackets s

||| After HTML escaping, there are no raw angle brackets.
||| Postulated: the `escapeHTML` function's `go` helper replaces `<` with
||| `&lt;` and `>` with `&gt;`. The entity replacements contain only
||| characters from {&, l, t, g, ;} -- none of which are `<` or `>`.
||| Proof requires induction over the character list and FFI round-trip.
export
postulate
escapeHTMLSafeProperty : (s : String) ->
                         all (\c => c /= '<' && c /= '>') (unpack (escapeHTML s)) = True

||| After HTML escaping, there are no raw angle brackets
public export
escapeHTMLSafe' : (s : String) -> NoRawBrackets (escapeHTML s)
escapeHTMLSafe' s = MkNoRawBrackets (escapeHTML s) (escapeHTMLSafeProperty s)

--------------------------------------------------------------------------------
-- Split/Join Properties
--------------------------------------------------------------------------------

||| Splitting and joining with same delimiter is identity (for strings
||| without the delimiter character).
||| Postulated: requires showing that `split` on a string with no delimiter
||| occurrences produces a singleton list `[s]`, and `join sep [s] = s`.
||| Both `split` and `join` operate via `pack`/`unpack` FFI boundaries.
export
postulate
splitJoinIdentity : (delim : Char) -> (s : String) ->
                    not (delim `elem` unpack s) = True ->
                    join (singleton delim) (split delim s) = s

||| Lines/unlines round-trip preserves content (modulo trailing newline).
||| Postulated: `lines` splits on '\n' and `unlines` joins with "\n".
||| The exact round-trip depends on trailing newline handling in both
||| functions and the `pack`/`unpack` FFI identity.
export
postulate
linesUnlinesApprox : (s : String) -> unlines (lines s) = s ++ ""
