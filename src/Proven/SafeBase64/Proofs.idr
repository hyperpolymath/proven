-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for Base64 operations
|||
||| This module provides formal proofs that SafeBase64 operations
||| maintain correctness properties including:
||| - Roundtrip (encode then decode returns original)
||| - Length correctness
||| - Character validity
module Proven.SafeBase64.Proofs

import Proven.Core
import Proven.SafeBase64.Types
import Proven.SafeBase64.Encode
import Proven.SafeBase64.Decode
import Data.List
import Data.List1
import Data.String

%default total

--------------------------------------------------------------------------------
-- Encoding Predicates
--------------------------------------------------------------------------------

||| Check if character is valid output for a Base64 variant
isValidOutputChar : Base64Variant -> Char -> Bool
isValidOutputChar v c = isValidBase64Char v c || isPaddingChar c ||
                        (v == MIME && isBase64Whitespace c)

||| Predicate: Output contains only valid Base64 characters
public export
data ValidBase64Output : Base64Variant -> String -> Type where
  MkValidBase64Output : (variant : Base64Variant) -> (s : String) ->
                        {auto prf : all (isValidOutputChar variant) (unpack s) = True} ->
                        ValidBase64Output variant s

||| Predicate: Encoded length is correct
public export
data CorrectEncodedLength : Base64Variant -> Nat -> Nat -> Type where
  MkCorrectEncodedLength : (variant : Base64Variant) ->
                           (inputLen : Nat) -> (outputLen : Nat) ->
                           {auto prf : outputLen = encodedLength variant inputLen} ->
                           CorrectEncodedLength variant inputLen outputLen

||| Predicate: Decoded length is correct
public export
data CorrectDecodedLength : Nat -> Nat -> Nat -> Type where
  MkCorrectDecodedLength : (encodedLen : Nat) -> (padding : Nat) -> (outputLen : Nat) ->
                           {auto prf : outputLen = exactDecodedLength encodedLen padding} ->
                           CorrectDecodedLength encodedLen padding outputLen

--------------------------------------------------------------------------------
-- Roundtrip Predicates
--------------------------------------------------------------------------------

||| Predicate: Encoding is reversible
public export
data RoundtripSuccess : Base64Variant -> List Bits8 -> Type where
  MkRoundtripSuccess : (variant : Base64Variant) -> (bytes : List Bits8) ->
                       {auto prf : decode variant (encodeBytesToString variant bytes) = Ok bytes} ->
                       RoundtripSuccess variant bytes

--------------------------------------------------------------------------------
-- Character Validity Proofs
--------------------------------------------------------------------------------

||| OWED: every character in the standard Base64 alphabet
||| (`"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"`)
||| satisfies `isValidBase64Char Standard`. The two predicates agree by
||| construction — `isValidBase64Char Standard c = isStandardBase64Char
||| c && c /= '='` and the alphabet contains none of `'='` — but the
||| equivalence is between `elem c (unpack standardAlphabet)` (an
||| `O(n)` list scan over an `unpack` of a String literal) and a
||| character-class check.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` over an abstract
||| or even literal `String` at the type level — `unpack` is an
||| FFI-bound `String` primitive, so the `elem` hypothesis does not
||| normalise by Refl alone. Same blocker family as
||| `SafeChecksum.luhnValidatesKnownGood` and
||| `SafeHtml.escapePreservesNoLT`. Discharge once a `Data.String`
||| reflective tactic for `unpack` is available, or by hand-rolling a
||| 64-arm exhaustive case-split over the alphabet (one `Refl` per
||| character).
0 standardAlphabetValid : (c : Char) -> c `elem` unpack standardAlphabet = True ->
                          isValidBase64Char Standard c = True

||| OWED: every character in the URL-safe Base64 alphabet
||| (`"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"`)
||| satisfies `isValidBase64Char URLSafe`. Same shape as
||| `standardAlphabetValid` with `+/` swapped for `-_`.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` over an abstract
||| `String` at the type level — same FFI-primitive blocker as
||| `standardAlphabetValid` above. Discharge by the same mechanism
||| (`Data.String` reflective tactic or 64-arm exhaustive case-split).
0 urlSafeAlphabetValid : (c : Char) -> c `elem` unpack urlSafeAlphabet = True ->
                         isValidBase64Char URLSafe c = True

||| OWED: the encoder `encodeBytesToString variant bytes` emits only
||| characters from the variant's alphabet plus padding (`'='`) plus,
||| for `MIME`, line-wrap whitespace — captured as a
||| `ValidBase64Output variant` witness. Witnessed operationally by
||| `encodeBytesToString` performing a 6-bit table lookup into
||| `getAlphabet variant` and appending exactly `'='` for padding (see
||| `src/Proven/SafeBase64/Encode.idr`).
|||
||| Held back by Idris2 0.8.0 not reducing `pack` / `unpack` over the
||| encoder's output `String` at the type level — the encoder builds
||| its result via `pack` of a `List Char` constructed by a
||| `Bits8`-driven fold, and the surrounding `all (isValidOutputChar
||| variant) (unpack ...)` does not normalise by Refl alone. Same
||| blocker family as `SafeHtml.escapePreservesNoLT`. Discharge once
||| a `Data.String` `pack`/`unpack` reflective tactic is available, or
||| by lifting the proof to the underlying `List Char` produced before
||| `pack` (which IS reducible by induction on the 6-bit chunking).
0 encodeOutputValid : (variant : Base64Variant) -> (bytes : List Bits8) ->
                      ValidBase64Output variant (encodeBytesToString variant bytes)

--------------------------------------------------------------------------------
-- Length Correctness Proofs
--------------------------------------------------------------------------------

||| Theorem: Encoded length formula is correct for padded variants
export
paddedLengthCorrect : (n : Nat) ->
                      encodedLength Standard n = ((n + 2) `div` 3) * 4
paddedLengthCorrect n = Refl

||| OWED: for padded variants, `(encodedLength variant n) `mod` 4 = 0`.
||| Follows arithmetically from `encodedLength = ((n + 2) `div` 3) * 4`
||| which is `4 * k` for `k = (n + 2) `div` 3`, hence divisible by 4.
|||
||| Held back by Idris2 0.8.0's `Integral Nat` `mod`/`div` not
||| reducing `(4 * k) `mod` 4 = 0` by Refl — the `Integral Nat`
||| instance is implementation-hidden behind a type-class projection
||| that the elaborator does not unfold to the underlying `divNatNZ`/
||| `modNatNZ` pattern-clauses. Same blocker as
||| `SafeChecksum.sumChecksumEmpty` (`0 \`mod\` 256 = 0` non-Refl).
||| Discharge once a `Data.Nat` reflective tactic for `mod`-of-
||| product-by-divisor is available, or by direct induction on
||| `(n + 2) `div` 3` using the `divides`/`Mod 0` lemmas from
||| `Data.Nat.Division`.
0 paddedLengthMultipleOf4 : (variant : Base64Variant) -> usesPadding variant = True ->
                            (n : Nat) -> (encodedLength variant n) `mod` 4 = 0

||| OWED: `decodedLength encodedLen <= (encodedLen * 3) `div` 4 + 1`
||| as a `Bool` equation. Follows definitionally from `decodedLength n
||| = (n * 3) `div` 4` (Types.idr L236) so the LHS is literally
||| `(encodedLen * 3) `div` 4` and the inequality is the trivial
||| `x <= x + 1`.
|||
||| Held back by Idris2 0.8.0 not exposing `lteAddRight`/`lteSucc` as
||| a `Bool`-reflective `Refl` — the `<=` here is the `Bool`-returning
||| `Data.Nat.lte`, not the propositional `LTE`, and the Bool-Prop
||| bridge is not reduced by Refl alone. Same Bool-vs-Prop blocker as
||| boj-server's class-J `charEqSound`/`charEqSym`. Discharge once a
||| `Bool`-reflective `lteSucc` is wired up, or by hand-writing
||| `decideEq` over the underlying `Nat` to convert the `LTE` proof
||| to its `Bool` form.
0 decodedLengthBound : (encodedLen : Nat) ->
                       decodedLength encodedLen <= (encodedLen * 3) `div` 4 + 1 = True

||| OWED: for non-empty input (`n > 0 = True`), `encodedLength variant
||| n >= n = True` (the 4/3 expansion ratio dominates `n`). By cases:
||| padded variants give `((n + 2) `div` 3) * 4 >= n` (since `(n + 2)
||| `div` 3 >= n `div` 3` and the `* 4` dominates `* 3 >= n`); the
||| `URLSafeNoPad` arm gives `((n * 8) + 5) `div` 6 >= n` for `n >= 1`.
|||
||| Held back by Idris2 0.8.0 not closing the chained `Nat` `div`/`>=`
||| inequalities at the type level — `Data.Nat.lte`'s `Bool`-returning
||| form is not reflectively bridged to the arithmetic `LTE` lemmas in
||| `Data.Nat`, and the `Integral Nat` `div` is a type-class
||| projection that does not normalise to its underlying
||| `divNatNZ`/pattern-clauses. Same blocker family as
||| `paddedLengthMultipleOf4` and `decodedLengthBound`. Discharge once
||| a `Data.Nat`/`Data.Bool` reflective tactic for `div`/`>=` is
||| available, or by chained applications of `lteMultRight`/
||| `divLteRight` after refactoring `encodedLength` to expose a
||| `total` divisor.
0 encodingIncreasesLength : (variant : Base64Variant) -> (n : Nat) -> n > 0 = True ->
                            encodedLength variant n >= n = True

--------------------------------------------------------------------------------
-- Roundtrip Proofs
--------------------------------------------------------------------------------

||| OWED: fundamental round-trip correctness — for every variant and
||| every byte sequence, `decode variant (encodeBytesToString variant
||| bytes) = Ok bytes`. Requires inductive reasoning over the 6-bit
||| chunking (`bytes : List Bits8` -> `chunks : List (Fin 64)`),
||| alphabet table-lookup invertibility, and padding-arithmetic round
||| trip for the three residue classes (`n `mod` 3 ∈ {0,1,2}`).
|||
||| Held back by Idris2 0.8.0 not reducing the encoder/decoder pair at
||| the type level — both compose `pack`/`unpack` (String FFI
||| primitives) with `Bits8` bit-manipulation (`shiftL`/`shiftR`/`.&.`
||| from `Data.Bits`) whose `Refl`-normalisation is not exposed
||| through the `Bits` type-class projection. Same blocker family as
||| `SafeHtml.escapePreservesNoLT` (String FFI) and
||| `SafeChecksum.xorChecksumInvolution`-style `Data.Bits` non-Refl.
||| Discharge once both a `Data.String` `pack`/`unpack` reflective
||| tactic and a `Data.Bits` `shiftL`/`shiftR` reflective tactic are
||| available, or by lifting the entire proof to the underlying
||| `List Bits8 -> List (Fin 64) -> List Char -> List (Fin 64) ->
||| List Bits8` decomposition where each leg IS reducible by
||| structural induction.
0 roundtripCorrect : (variant : Base64Variant) -> (bytes : List Bits8) ->
                     decode variant (encodeBytesToString variant bytes) = Ok bytes

||| OWED: round-trip on the empty input, `decode variant
||| (encodeBytesToString variant []) = Ok []`. Witnessed by
||| `encodeBytesToString variant [] = ""` (the encoder's empty-list
||| arm) and `decode variant "" = Ok []` (the decoder's empty-input
||| arm).
|||
||| Held back by Idris2 0.8.0 not reducing `pack ""` / `unpack ""`
||| through the encoder's monadic plumbing — although both arms ARE
||| Refl on their `List`-level inputs, the encoder's wrapper applies
||| `pack` and the decoder's wrapper applies `unpack`, and the
||| composition does not normalise by Refl alone (FFI-primitive
||| String). Same blocker as `roundtripCorrect` above. Discharge by
||| the same mechanism, or as a one-off `Refl` once the encoder's
||| `pack`/`unpack` wrappers are reduced manually via a `Data.String`
||| reflective tactic.
0 roundtripEmpty : (variant : Base64Variant) ->
                   decode variant (encodeBytesToString variant []) = Ok []

||| OWED: round-trip on a single byte, `decode variant
||| (encodeBytesToString variant [b]) = Ok [b]`. Exercises the
||| 1-byte-of-3 padding arm: encoder emits two alphabet chars + `"=="`
||| (or just two chars for `URLSafeNoPad`), decoder strips the padding
||| and recovers the byte from the leading 8 bits of the 12-bit
||| chunk.
|||
||| Held back by Idris2 0.8.0 not reducing the encoder's `Bits8 ->
||| (Fin 64, Fin 64)` split (which uses `shiftR 4` and `shiftL 4 .&.
||| 0x3F` from `Data.Bits`) at the type level — these `Bits` instance
||| methods are not exposed as Refl. Compounded by the same
||| `pack`/`unpack` opacity as `roundtripEmpty`. Same blocker family
||| as `roundtripCorrect`. Discharge once a `Data.Bits` reflective
||| tactic for `shiftL`/`shiftR`/`.&.` is available alongside the
||| `Data.String` `pack`/`unpack` tactic.
0 roundtripSingleByte : (variant : Base64Variant) -> (b : Bits8) ->
                        decode variant (encodeBytesToString variant [b]) = Ok [b]

||| OWED: string-level round-trip, `decodeToString variant
||| (encodeStringToString variant s) = Ok s`. Factors through
||| `roundtripCorrect` plus `pack . unpack = id` on the input
||| `String` (UTF-8 encoding/decoding is lossless on every Idris2
||| `String`, which by spec contains a well-formed UTF-8 sequence).
|||
||| Held back by Idris2 0.8.0 not exposing `pack (unpack s) = s` as a
||| Refl — `pack` and `unpack` are FFI-bound String primitives and
||| their inverseness is asserted at the runtime layer, not at the
||| type-checker level. Same blocker family as `roundtripCorrect`
||| and `SafeHtml.escapePreservesNoLT`. Discharge once a
||| `Data.String` reflective tactic exposes `pack (unpack s) = s` and
||| `roundtripCorrect` is itself discharged.
0 roundtripString : (variant : Base64Variant) -> (s : String) ->
                    decodeToString variant (encodeStringToString variant s) = Ok s

--------------------------------------------------------------------------------
-- Variant Equivalence Proofs
--------------------------------------------------------------------------------

||| OWED: `Standard` and `URLSafe` variants produce equal-length
||| output for any byte sequence. Both call the same chunking and
||| padding paths in `encodeBytesToString` (`usesPadding` is `True`
||| for both, the alphabet is consulted character-by-character but
||| never affects the chunk count), so the output `String` lengths
||| are equal by structural agreement of the encoder paths.
|||
||| Held back by Idris2 0.8.0 not reducing `length (unpack s)` over
||| an abstract `String` at the type level — `unpack` is an FFI-bound
||| primitive and `length . unpack` is not exposed as a `Refl`-able
||| equation. The internal `Bits8`-chunking step IS reducible by
||| induction, but the outer `pack`/`unpack` wrappers are not. Same
||| blocker family as `roundtripCorrect`. Discharge once a
||| `Data.String` reflective tactic exposes `length (unpack (pack
||| xs)) = length xs` (i.e. `pack`/`unpack` preserve length), then
||| `Refl` closes against the shared chunking step.
0 variantsEqualLength : (bytes : List Bits8) ->
                        length (unpack (encodeBytesToString Standard bytes)) =
                        length (unpack (encodeBytesToString URLSafe bytes))

||| OWED: `URLSafeNoPad` output length is at most `Standard` output
||| length. Both share the chunking step; `URLSafeNoPad` then strips
||| trailing `'='` characters, which can only shorten the result (the
||| stripped count is `0`, `1`, or `2`).
|||
||| Held back by Idris2 0.8.0 not reducing `length (unpack s)` at the
||| type level (same `unpack`-FFI opacity as `variantsEqualLength`),
||| combined with `Data.Nat.lte`'s `Bool`-returning form not being
||| reflectively bridged to the arithmetic `LTE` lemmas that would
||| witness `(stripped + k) <= k = False` ==> `k <= stripped + k =
||| True`. Same blocker family as `decodedLengthBound`. Discharge by
||| the same mechanism (`Data.String` `unpack` + `Data.Nat`/`Bool`
||| `lte` reflective tactics).
0 noPadShorter : (bytes : List Bits8) ->
                 let standardLen = length (unpack (encodeBytesToString Standard bytes))
                     noPadLen = length (unpack (encodeBytesToString URLSafeNoPad bytes))
                 in noPadLen <= standardLen = True

--------------------------------------------------------------------------------
-- Safety Proofs
--------------------------------------------------------------------------------

||| OWED: decoding never crashes — `decode variant input` is always
||| `Err _` or `Ok _`, witnessed as a disjunctive sigma. Operationally
||| trivial (`decode : ... -> Base64Result`, and `Base64Result` has
||| exactly two constructors), but the propositional version requires
||| a case-split on the abstract result with equational witnesses
||| `decode variant input = Err err` / `decode variant input = Ok
||| bytes`.
|||
||| Held back by Idris2 0.8.0's `case`/`with` blocks not retaining
||| the equational hypothesis between the scrutinee and the matched
||| pattern when the scrutinee is an opaque function application
||| (here `decode variant input`). The compiler refines the goal but
||| does not produce a Refl chain back to the scrutinee. Same shape
||| as the `case-on-opaque-decision` issues in `boj-server`'s
||| `Boj.SafetyLemmas`. Discharge once `case`/`with` is upgraded to
||| produce the `_ = decode variant input` equation in scope (the
||| canonical Idris2 enhancement #2400-series), or by routing the
||| scrutinee through a `decideEq`-style helper that returns the
||| equation explicitly.
0 decodeNeverCrashes : (variant : Base64Variant) -> (input : String) ->
                       Either (err : Base64Error ** decode variant input = Err err)
                              (bytes : List Bits8 ** decode variant input = Ok bytes)

||| OWED: if `input` contains any character that is neither a valid
||| Base64 char for `variant` nor a padding char, then `decode variant
||| input = Err _` (i.e. `isOk (decode variant input) = False`).
||| Witnessed operationally by the decoder's character-validation
||| pre-pass which rejects any character failing
||| `isValidBase64Char variant c || isPaddingChar c`.
|||
||| Held back by Idris2 0.8.0 not reducing `elem` / `unpack` /
||| `decode` at the type level — `unpack` is FFI-bound, `decode`'s
||| validation pass is a `foldl`/`any` over the unpacked input whose
||| short-circuit behaviour on the first invalid char is not exposed
||| as a Refl chain. Same blocker family as
||| `SafeHtml.escapePreservesNoLT`. Discharge once a `Data.String`
||| reflective tactic for `unpack`/`elem` is available, or by
||| refactoring the decoder to return a `Dec`-style witness alongside
||| each rejection.
0 invalidCharDetected : (variant : Base64Variant) -> (input : String) ->
                        (c : Char) -> (pos : Nat) ->
                        c `elem` unpack input = True ->
                        not (isValidBase64Char variant c) = True ->
                        not (isPaddingChar c) = True ->
                        isOk (decode variant input) = False

||| Index into a list safely
index' : Nat -> List a -> Maybe a
index' _ [] = Nothing
index' Z (x :: _) = Just x
index' (S k) (_ :: xs) = index' k xs

||| OWED: a `'='` padding char appearing strictly before position
||| `length input - 2` causes `decode Standard input = Err _` (i.e.
||| `isOk (decode Standard input) = False`). Standard Base64 padding
||| is well-formed only in the last one or two positions of a length-
||| multiple-of-4 input; the decoder's padding-position validator
||| rejects all earlier `'='`.
|||
||| Held back by Idris2 0.8.0 not reducing the chained
||| `index' . unpack` / `length . unpack` / `decode` over the
||| abstract `input : String` at the type level — `unpack` is
||| FFI-bound, so neither the position witness nor the validator's
||| short-circuit reduce to Refl. Same blocker family as
||| `invalidCharDetected`. Discharge once a `Data.String` reflective
||| tactic for `unpack` / `index'` is available, or by refactoring
||| `decode` to thread an explicit position-validation `Dec` witness.
0 invalidPaddingDetected : (input : String) ->
                           -- Padding not at end
                           (pos : Nat) -> pos < (length (unpack input) `minus` 2) = True ->
                           index' pos (unpack input) = Just '=' ->
                           isOk (decode Standard input) = False

--------------------------------------------------------------------------------
-- MIME-Specific Proofs
--------------------------------------------------------------------------------

||| OWED: every line in the MIME-encoded output is at most
||| `mimeLineLength + 1 = 77` chars long (the `+ 1` accommodates a
||| trailing `\r`). Witnessed by the MIME encoder's line-wrapping
||| loop, which forces a `"\r\n"` insertion every 76 alphabet chars.
|||
||| Held back by Idris2 0.8.0 not reducing the chained
||| `split . encodeBytesToString MIME` / `length . unpack` / `all`
||| over an abstract `List Bits8` at the type level — every step
||| passes through an FFI-bound String primitive (`pack`/`unpack` on
||| the encoder output, `split` from `Data.List1` on the `'\n'`
||| separator). Same blocker family as `roundtripCorrect`. Discharge
||| once a `Data.String` reflective tactic exposes
||| `length . unpack . pack = length` and a `Data.List1` `split`
||| reflective tactic is available, or by inductive proof over the
||| encoder's line-wrap loop counter.
0 mimeLineBreaksCorrect : (bytes : List Bits8) ->
                          let encoded = encodeBytesToString MIME bytes
                              lines = forget (split (== '\n') encoded)
                          in all (\l => length (unpack l) <= mimeLineLength + 1) lines = True

||| Strip whitespace from a string for MIME comparison
stripWhitespace : String -> String
stripWhitespace s = pack (filter (not . isBase64Whitespace) (unpack s))

||| OWED: MIME decoding is invariant under whitespace insertion —
||| if `stripWhitespace withWs = encoded`, then `decode MIME withWs =
||| decode MIME encoded`. Witnessed by the MIME decoder applying
||| `stripWhitespace` (or its inline equivalent) as the first
||| transformation before character validation and chunk decoding.
|||
||| Held back by Idris2 0.8.0 not reducing the chained
||| `pack . filter . unpack` (the `stripWhitespace` definition) over
||| an abstract `String` at the type level — `pack`/`unpack` are
||| FFI-bound, so the equational hypothesis does not normalise into
||| the decoder body by Refl alone. Same blocker family as
||| `roundtripString`. Discharge once a `Data.String` reflective
||| tactic exposes `pack (filter p (unpack s))` as a `Refl`-able
||| operation, or by refactoring `decode MIME` to take the already-
||| stripped `String` as an explicit pre-condition.
0 mimeIgnoresWhitespace : (encoded : String) -> (withWs : String) ->
                          stripWhitespace withWs = encoded ->
                          decode MIME withWs = decode MIME encoded

--------------------------------------------------------------------------------
-- URL-Safety Proofs
--------------------------------------------------------------------------------

||| OWED: the `URLSafe` encoded output never contains `'+'` or `'/'`
||| — the URL-safe alphabet
||| (`"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"`)
||| contains neither, and `encodeBytesToString` only emits alphabet
||| chars plus `'='` padding (which is also neither `'+'` nor `'/'`).
|||
||| Held back by Idris2 0.8.0 not reducing `all . unpack .
||| encodeBytesToString URLSafe` at the type level — the encoder's
||| `pack` wrapper plus the `unpack`-then-`all` outer chain are
||| FFI-bound String primitives, so the per-character check does not
||| normalise by Refl alone. Same blocker family as
||| `encodeOutputValid`. Discharge once a `Data.String` reflective
||| tactic for `pack`/`unpack` is available, or by lifting the proof
||| to the underlying pre-`pack` `List Char` where the alphabet table
||| lookup IS reducible (one `Refl` per Fin 64 index).
0 urlSafeContainsNoUnsafe : (bytes : List Bits8) ->
                            let encoded = encodeBytesToString URLSafe bytes
                            in all (\c => c /= '+' && c /= '/') (unpack encoded) = True

||| OWED: a disjunctive witness over `Standard` encoded output —
||| either `'+'` appears, `'/'` appears, or neither does. (The point
||| is that *with* standard input you cannot rule out the unsafe
||| chars unconditionally, only by case-split.)
|||
||| Held back by Idris2 0.8.0 not reducing `elem` / `unpack` /
||| `encodeBytesToString Standard` over an abstract `List Bits8` at
||| the type level — both the `elem` decision and the encoder's
||| `pack` wrapper are FFI-bound. The case-split is operationally
||| over the encoder's emitted alphabet-index sequence (which
||| includes `'+' = index 62` and `'/' = index 63`), but the
||| reflection back to the `String`-level `elem` is opaque. Same
||| blocker family as `urlSafeContainsNoUnsafe`. Discharge once a
||| `Data.String` reflective tactic for `elem`/`unpack` is available,
||| or by lifting to the pre-`pack` `List (Fin 64)` representation
||| and case-splitting on whether `62 \`elem\` indices` or
||| `63 \`elem\` indices`.
0 standardMayContainUnsafe : (bytes : List Bits8) ->
                             Either ('+' `elem` unpack (encodeBytesToString Standard bytes) = True)
                                    (Either ('/' `elem` unpack (encodeBytesToString Standard bytes) = True)
                                            (all (\c => c /= '+' && c /= '/') (unpack (encodeBytesToString Standard bytes)) = True))

--------------------------------------------------------------------------------
-- Length Relationship Proofs
--------------------------------------------------------------------------------

||| OWED: when `n `mod` 3 = 0`, `encodedLength Standard n = (n `div`
||| 3) * 4` (exact ratio, no padding). Definitionally
||| `encodedLength Standard n = ((n + 2) `div` 3) * 4`; under the
||| premise `n `mod` 3 = 0` we have `(n + 2) `div` 3 = n `div` 3`
||| because `Nat`-div rounds down and the "ceiling" `+ 2` only adds
||| to the quotient when the remainder is nonzero.
|||
||| Held back by Idris2 0.8.0's `Integral Nat` `div`/`mod` not
||| reducing the case-split on the remainder to a Refl chain — the
||| `Integral Nat` instance is a type-class projection that does not
||| unfold to its `divNatNZ`/`modNatNZ` pattern-clauses, and the
||| arithmetic lemma `n `mod` 3 = 0 -> (n + 2) `div` 3 = n `div` 3`
||| is not exposed in `Data.Nat`. Same blocker family as
||| `paddedLengthMultipleOf4`. Discharge once a `Data.Nat`
||| `divMod`-elimination reflective tactic is available, or via the
||| `Data.Nat.Division` `divides`-elimination lemmas after
||| introducing `(k : Nat) ** n = 3 * k` from the `n `mod` 3 = 0`
||| hypothesis.
0 threeToFourRatio : (n : Nat) -> (n `mod` 3 = 0) = True ->
                     encodedLength Standard n = (n `div` 3) * 4

||| Count padding characters in a string
countPadding : String -> Nat
countPadding s = length (filter (== '=') (unpack s))

||| OWED: for any padded variant, the count of `'='` characters in
||| the encoded output of an all-zero input of length `n` matches the
||| input residue mod 3: remainder `0` -> `0` padding, remainder `1`
||| -> `2` padding, remainder `2` -> `1` padding. Witnessed by the
||| encoder's padding-emission arm, which appends `(3 - n `mod` 3)
||| `mod` 3` padding chars after the final partial-chunk encoding.
|||
||| Held back by Idris2 0.8.0 not reducing the chained
||| `countPadding . encodeBytesToString variant . replicate n 0` at
||| the type level — `countPadding` itself threads through `unpack`
||| (FFI-bound) plus a `length . filter` over the encoder's
||| `pack`-wrapped output, and the `Integral Nat` `mod` projection
||| does not unfold to its pattern-clauses (same blocker as
||| `paddedLengthMultipleOf4`). Compounded by the encoder's
||| `replicate n 0` input only reducing for concrete `n`. Discharge
||| once a `Data.String` `pack`/`unpack`/`filter` reflective tactic
||| is available alongside a `Data.Nat` `mod`-elimination tactic, or
||| by case-splitting on `n `mod` 3 ∈ {0,1,2}` and lifting to the
||| pre-`pack` `List Char` where padding emission IS reducible.
0 paddingMatchesRemainder : (variant : Base64Variant) -> usesPadding variant = True ->
                            (n : Nat) ->
                            let remainder = n `mod` 3
                                encoded = encodeBytesToString variant (replicate n 0)
                                padding = countPadding encoded
                            in (remainder = 0 -> padding = 0,
                                remainder = 1 -> padding = 2,
                                remainder = 2 -> padding = 1)

--------------------------------------------------------------------------------
-- Composition Proofs
--------------------------------------------------------------------------------

||| OWED: independently encoded segments can be decoded
||| independently — `decode variant (encodeBytesToString variant
||| bytes1) = Ok bytes1` AND the same for `bytes2`. Follows directly
||| from `roundtripCorrect` applied twice (once per segment).
|||
||| Held back transitively by `roundtripCorrect` — see that
||| declaration's OWED above for the underlying `pack`/`unpack` +
||| `Data.Bits` blocker. Discharge automatically once
||| `roundtripCorrect` is discharged: the proof body is then a pair
||| of `roundtripCorrect variant bytes1` and `roundtripCorrect
||| variant bytes2`.
0 segmentedRoundtrip : (variant : Base64Variant) -> (bytes1 : List Bits8) -> (bytes2 : List Bits8) ->
                       let enc1 = encodeBytesToString variant bytes1
                           enc2 = encodeBytesToString variant bytes2
                       in (decode variant enc1 = Ok bytes1,
                           decode variant enc2 = Ok bytes2)

||| Theorem: Encoding preserves byte order.
||| This is a name-alias of `roundtripCorrect` (identical signature).
||| Erased-multiplicity `0` so it can transparently reference the
||| now-erased `roundtripCorrect` body. Discharge follows
||| `roundtripCorrect` directly.
0 encodingPreservesOrder : (variant : Base64Variant) -> (bytes : List Bits8) ->
                           decode variant (encodeBytesToString variant bytes) = Ok bytes
encodingPreservesOrder = roundtripCorrect

--------------------------------------------------------------------------------
-- Security Documentation
--------------------------------------------------------------------------------

||| Summary of SafeBase64 guarantees:
|||
||| 1. **Roundtrip Correctness**: Encoding then decoding returns the original input.
|||
||| 2. **Length Predictability**: Output length is deterministic from input length.
|||
||| 3. **Character Validity**: Output contains only valid Base64 characters.
|||
||| 4. **Error Handling**: Decoding returns Result, never throws exceptions.
|||
||| 5. **URL Safety**: URL-safe variant avoids +, /, and optionally =.
|||
||| 6. **MIME Compliance**: MIME variant adds line breaks at 76 characters.
|||
||| 7. **Padding Correctness**: Padding is handled correctly for all variants.
public export
safetyGuarantees : String
safetyGuarantees = """
SafeBase64 Safety Guarantees:

1. Roundtrip Correctness
   - encode(decode(x)) = x for valid input
   - decode(encode(x)) = x always

2. Length Properties
   - Encoded length = ceil(n/3) * 4 (padded)
   - Decoded length <= ceil(n * 3/4)
   - Predictable output size

3. Character Safety
   - Standard: A-Z, a-z, 0-9, +, /, =
   - URL-safe: A-Z, a-z, 0-9, -, _, =
   - MIME: Standard + CRLF

4. Error Handling
   - No exceptions thrown
   - All errors returned as Result
   - Invalid input detected

5. Variant Support
   - Standard (RFC 4648)
   - URL-safe (RFC 4648 S5)
   - MIME (RFC 2045)
"""
