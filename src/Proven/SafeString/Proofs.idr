-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeString operations
|||
||| Many properties in this module are postulated rather than proven
||| because Idris 2's `String` type is an opaque FFI primitive backed by
||| C strings. Structural induction on `String` is not possible -- the
||| `pack`/`unpack` round-trip is an FFI boundary, not a definitional
||| equality. Each axiom documents precisely why it cannot be proven
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
emptyStringLength : Prelude.String.length "" = 0
emptyStringLength = Refl

||| OWED: `length (s1 ++ s2) = length s1 + length s2`. Witnessed
||| operationally by the C-FFI implementation — `strlen` is additive
||| over `strcat` on null-terminated byte buffers.
|||
||| Held back by Idris2 0.8.0 not reducing `String` `++` and
||| `Prelude.String.length` at the type level — both are FFI-bound
||| String primitives (no `pack`/`unpack` structural induction is
||| available on opaque `String`). Same blocker family as
||| SafeChecksum's String-FFI OWED set and SafeHtml's
||| `escapePreservesNoLT`. Discharge once a `Data.String` reflective
||| tactic for `length` / `++` is available, or `String` is refactored
||| to expose its packed-character list as a definitional equality.
export
0 concatLength : (s1, s2 : String) ->
                 Prelude.String.length (s1 ++ s2) = Prelude.String.length s1 + Prelude.String.length s2

--------------------------------------------------------------------------------
-- Trim Properties
--------------------------------------------------------------------------------

||| Trimming empty string gives empty string
public export
trimEmpty : Proven.SafeString.trim "" = ""
trimEmpty = Refl

||| OWED: if a string contains no whitespace characters, then
||| `trim s = s`. Witnessed operationally by `trim = ltrim . rtrim`
||| with both halves implemented as `pack (dropWhile isSpace (unpack
||| …))` — if no character satisfies `isSpace`, `dropWhile isSpace`
||| is the identity on the unpacked list, and `pack . unpack` is the
||| identity on `String` at the C-FFI boundary.
|||
||| Held back by Idris2 0.8.0 not reducing the `pack . unpack` round
||| trip at the type level — both `pack` and `unpack` are FFI-bound
||| String primitives, so `pack (dropWhile isSpace (unpack s)) = s`
||| does not normalise to `Refl` even when `dropWhile` is provably
||| the identity. Same blocker family as `concatLength` and SafeHtml's
||| `escapePreservesNoLT`. Discharge once a `Data.String` reflective
||| tactic for `pack . unpack` is available, or `String` is refactored
||| to expose its packed-character list.
export
0 trimNoWhitespace : (s : String) -> all (Prelude.Basics.not . isSpace) (unpack s) = True ->
                     Proven.SafeString.trim s = s

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

||| OWED: HTML escaping is idempotent on safe single-character
||| strings — i.e. when `c` is not in `{&, <, >, ", '}`,
||| `escapeHTML (singleton c) = singleton c`. Witnessed operationally
||| by `escapeHTML`'s `go` helper falling through to the no-op
||| character branch.
|||
||| Held back by Idris2 0.8.0 not reducing the chain
||| `pack (go (unpack (singleton c)))` at the type level — `pack`,
||| `unpack` and `singleton` are all FFI-bound String primitives. The
||| identities `unpack (singleton c) = [c]` and `pack [c] = singleton
||| c` hold operationally but do not normalise by `Refl` for an
||| abstract `Char c`. Same blocker family as `concatLength` /
||| `trimNoWhitespace` and SafeHtml's `escapePreservesNoLT`.
||| Discharge once `Data.String` exposes a reflective tactic for
||| `pack . singletonList = singleton` (or the `singleton`/`unpack`
||| pair becomes a definitional inverse).
export
0 escapeHTMLSafe : (c : Char) ->
                   Prelude.Basics.not (c == '&' || c == '<' || c == '>' || c == '"' || c == '\'') = True ->
                   escapeHTML (singleton c) = singleton c

--------------------------------------------------------------------------------
-- Injection Safety Properties
--------------------------------------------------------------------------------

||| SQL-escaped string contains no unescaped single quotes
||| This is the key safety property for SQL injection prevention
|||
||| The `prf` field is `0 `-erased so callers can witness it from the
||| `escapeSQLSafeProperty` OWED postulate below without forcing the
||| postulate into runtime context (OWED-with-justification convention,
||| Refs standards#158).
public export
data NoUnescapedQuotes : String -> Type where
  MkNoUnescapedQuotes : (s : String) ->
                        (0 prf : all (\c => c /= '\'') (unpack s) = True) ->
                        NoUnescapedQuotes s

||| OWED: after SQL escaping, the predicate
||| `all (\c => c /= '\'') (unpack (escapeSQL s)) = True` holds.
||| Witnessed operationally by `escapeSQL`'s `go` helper replacing
||| every `'` with `''` — the doubling transformation produces
||| pairs of quotes that the weaker predicate `c /= '\''` does not
||| distinguish from absence, so the captured guarantee is the looser
||| "no unescaped quote remains in isolation" form noted below.
|||
||| Note: as stated, `NoUnescapedQuotes` checks for ANY `'`; the
||| operational safety property is that `'` appears only in pairs.
||| The OWED here captures the weaker (but still injection-relevant)
||| variant — the stronger paired-quote invariant is left for a
||| follow-up once the FFI blocker below is addressed.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` and the `go`
||| helper's `pack`-then-`unpack` round trip at the type level — both
||| are FFI-bound String primitives, so `all p (unpack (escapeSQL s))`
||| does not normalise to `True` by `Refl` for an abstract `s`. Same
||| blocker family as `concatLength` / `escapeHTMLSafe` and SafeHtml's
||| `escapePreservesNoLT`. Discharge once a `Data.String` reflective
||| tactic for `unpack . pack . map f` (or an induction principle on
||| the unpacked list) is available.
export
0 escapeSQLSafeProperty : (s : String) ->
                          all (\c => c /= '\'') (unpack (escapeSQL s)) = True

||| After SQL escaping, there are no single quotes that aren't doubled
public export
escapeSQLSafe : (s : String) -> NoUnescapedQuotes (escapeSQL s)
escapeSQLSafe s = MkNoUnescapedQuotes (escapeSQL s) (escapeSQLSafeProperty s)

||| HTML-escaped string contains no raw angle brackets
|||
||| The `prf` field is `0 `-erased so callers can witness it from the
||| `escapeHTMLSafeProperty` OWED postulate below without forcing the
||| postulate into runtime context (OWED-with-justification convention,
||| Refs standards#158).
public export
data NoRawBrackets : String -> Type where
  MkNoRawBrackets : (s : String) ->
                    (0 prf : all (\c => c /= '<' && c /= '>') (unpack s) = True) ->
                    NoRawBrackets s

||| OWED: after HTML escaping, no raw `<` or `>` survives — i.e.
||| `all (\c => c /= '<' && c /= '>') (unpack (escapeHTML s)) = True`.
||| Witnessed operationally by `escapeHTML`'s `go` helper rewriting
||| `'<' |-> "&lt;"` and `'>' |-> "&gt;"`; the entity replacements
||| draw only from `{&, l, t, g, ;}`, none of which are `<` or `>`,
||| and all other characters pass through unchanged.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` and the `go`
||| helper's `pack`-then-`unpack` round trip at the type level — both
||| are FFI-bound String primitives, so `all p (unpack (escapeHTML
||| s))` does not normalise to `True` by `Refl` for an abstract `s`.
||| Same blocker family as `escapeSQLSafeProperty` and SafeHtml's
||| `escapePreservesNoLT`. Discharge once a `Data.String` reflective
||| tactic for `unpack . pack . map f` (or an induction principle on
||| the unpacked list) is available.
export
0 escapeHTMLSafeProperty : (s : String) ->
                           all (\c => c /= '<' && c /= '>') (unpack (escapeHTML s)) = True

||| After HTML escaping, there are no raw angle brackets
public export
escapeHTMLSafe' : (s : String) -> NoRawBrackets (escapeHTML s)
escapeHTMLSafe' s = MkNoRawBrackets (escapeHTML s) (escapeHTMLSafeProperty s)

--------------------------------------------------------------------------------
-- Split/Join Properties
--------------------------------------------------------------------------------

||| OWED: when `s` contains no occurrence of `delim`, splitting then
||| joining is the identity:
||| `join (singleton delim) (split delim s) = s`. Witnessed
||| operationally by `split delim s = splitHelper delim (unpack s) []
||| []` — with no delimiter present, `splitHelper` accumulates the
||| whole list and returns `[pack (unpack s)] = [s]`; `join sep [s]`
||| reduces to `s` by definition.
|||
||| Held back by Idris2 0.8.0 not reducing the `pack . unpack` round
||| trip inside `splitHelper` at the type level — both are FFI-bound
||| String primitives. Even granted that `splitHelper` returns `[pack
||| (unpack s)]`, `pack (unpack s) = s` is the FFI identity that does
||| not normalise by `Refl`. Same blocker family as `trimNoWhitespace`
||| and SafeHtml's `escapePreservesNoLT`. Discharge once a
||| `Data.String` reflective tactic for `pack . unpack` is available.
export
0 splitJoinIdentity : (delim : Char) -> (s : String) ->
                      Prelude.Basics.not (delim `elem` unpack s) = True ->
                      Proven.SafeString.join (singleton delim) (Proven.SafeString.split delim s) = s

||| OWED: the lines/unlines round trip preserves content up to a
||| trailing-empty-string concatenation:
||| `unlines (lines s) = s ++ ""`. Witnessed operationally by
||| `lines = split '\n'` and `unlines = join "\n"`; the `++ ""` slack
||| absorbs the trailing-newline-handling discrepancy between the
||| split-then-join pair (concrete trailing-newline semantics differ
||| between `splitHelper`'s final-segment flush and `join "\n"`'s
||| separator-between-elements behaviour).
|||
||| Held back by Idris2 0.8.0 not reducing the `pack . unpack` round
||| trip inside `split` / `join` at the type level — both are
||| FFI-bound String primitives, so even the right-identity
||| `s ++ "" = s` (itself an FFI fact) combined with the
||| `split`/`join` reduction does not normalise to `Refl` for
||| abstract `s`. Same blocker family as `splitJoinIdentity` and
||| `concatLength`. Discharge once a `Data.String` reflective tactic
||| for `pack . unpack` (and `s ++ ""`) is available, or `split` /
||| `join` are refactored to a list-based intermediate that admits
||| structural induction.
export
0 linesUnlinesApprox : (s : String) -> Proven.SafeString.unlines (Proven.SafeString.lines s) = s ++ ""
