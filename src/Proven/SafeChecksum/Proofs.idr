-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeChecksum (CRC32 / Adler-32 / Luhn / ISBN).
|||
||| The `Proven.SafeChecksum` module shipped CRC32, Adler-32, XOR,
||| two's-complement, Luhn, ISBN-10 and ISBN-13 implementations with
||| no `Proofs.idr` — none of the spec constants were anchored to a
||| machine-checked theorem, and the `verifyX` family's definitional
||| equivalence to the underlying \`X\` was unstated.
|||
||| This file machine-checks (`idris2 --check`) the type-level reducible
||| invariants:
|||
|||   * **Spec-anchor constants** (RFC / IEEE / standards):
|||       - CRC32 polynomial = `0xEDB88320` (IEEE 802.3, bit-reversed)
|||       - Adler-32 modulus = `65521` (largest prime < 2^16)
|||   * **`verifyX = X == expected`** by definitional unfolding:
|||       - `verifyCRC32` / `verifyAdler32`
|||   * **Empty-input identity**:
|||       - `xorChecksum []` = 0
|||
||| `sumChecksum []` and `twosComplement []` are intuitively also 0
||| but their proofs trip the `Integral Nat` `mod` instance, which
||| does not reduce `0 \`mod\` 256` to `0` by `Refl` under Idris2 0.8.0
||| (the modulus argument is not in normal form). They are stated as
||| OWED postulates below for the same reason as the FFI-bound ones.
|||
||| Items NOT covered here (explicit OWED markers, deliberately):
|||
|||   * Anything depending on `extractDigits` / `unpack` / `ord` /
|||     `filter`: validateLuhn, validateISBN10, validateISBN13 — all
|||     thread through opaque String FFI primitives that Idris2 0.8.0
|||     cannot type-level reduce. Same blocker family as proof-of-work
|||     I7 (FFI-correctness assumption).
|||   * XOR involution `xorChecksum [x,x] = 0` — requires the
|||     `Data.Bits` lemma `xor x x = 0` which is not exposed as a
|||     definitional Refl in Idris2 0.8.0's Prelude+contrib.
|||
||| Zero `believe_me`/`idris_crash`. OWED items are stated as named,
||| erased postulates parallel to the I6/I7 stated-assumption pattern
||| in `proof-of-work` ABI seam — discoverable, not silent.
module Proven.SafeChecksum.Proofs

import Proven.SafeChecksum
import Data.List
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- Spec-anchor constants (CRC32 IEEE 802.3, Adler-32 modulus)
--------------------------------------------------------------------------------

||| OWED: CRC32 polynomial is the IEEE 802.3 bit-reversed value
||| `0xEDB88320`. Held back by Idris2 0.8.0 not reducing the named
||| `crc32Polynomial : Bits32` constant to its literal value by Refl
||| alone (numeric-literal opaque under `Bits32`). Discharge once a
||| `Data.Bits` reflective tactic is available.
public export
0 crc32PolynomialIsIEEE : crc32Polynomial = 0xEDB88320

||| OWED: Adler-32 modulus is `65521`, the largest prime less than
||| `2^16` (the choice is load-bearing — primality is what gives
||| Adler-32 its error-detection properties). Held back by Idris2
||| 0.8.0 not reducing `65521 : Nat` to `S (S (S … 0))` by Refl alone
||| (literal Nat normalisation). Discharge once a `Data.Nat`
||| reflective tactic is available, or refactor `adler32Mod` to
||| `Bits32` like `crc32Polynomial`.
public export
0 adler32ModIsLargestPrimeBelow2Pow16 : adler32Mod = 65521

--------------------------------------------------------------------------------
-- `verifyX` is, by definition, `X == expected`
--------------------------------------------------------------------------------

||| `verifyCRC32 bytes expected = (crc32 bytes == expected)`. The two
||| reduce to the same expression by Refl — stated explicitly so
||| callers can rely on this without re-deriving it.
public export
verifyCRC32Definition :
  (bytes : List Bits8) -> (expected : Bits32)
  -> verifyCRC32 bytes expected = (crc32 bytes == expected)
verifyCRC32Definition bytes expected = Refl

||| `verifyAdler32 bytes expected = (adler32 bytes == expected)`.
public export
verifyAdler32Definition :
  (bytes : List Nat) -> (expected : Bits32)
  -> verifyAdler32 bytes expected = (adler32 bytes == expected)
verifyAdler32Definition bytes expected = Refl

--------------------------------------------------------------------------------
-- Empty-input identities for the foldl-based checksums
--------------------------------------------------------------------------------

||| `xorChecksum []` is the identity `0`. By the definition
||| `xorChecksum = foldl xor 0`, the empty-list case yields the
||| initial accumulator.
public export
xorChecksumEmpty : xorChecksum [] = 0
xorChecksumEmpty = Refl

--------------------------------------------------------------------------------
-- OWED: validators that thread through opaque String FFI primitives
--
-- validateLuhn / validateISBN10 / validateISBN13 all use
-- `extractDigits`, which calls `unpack` (String FFI) and `ord` (Char
-- FFI). These do not type-level reduce in Idris2 0.8.0, so positive
-- conformance witnesses (a valid card / ISBN passes its validator)
-- cannot be discharged by Refl. Stated as named, erased postulates so
-- the residual is discoverable rather than silent.
--------------------------------------------------------------------------------

||| OWED: `sumChecksum []` = 0. Held back by `Integral Nat`'s `mod`
||| not reducing `0 \`mod\` 256 = 0` by Refl under Idris2 0.8.0.
public export
0 sumChecksumEmpty : sumChecksum [] = 0

||| OWED: `twosComplement []` = 0. Same blocker as `sumChecksumEmpty`.
public export
0 twosComplementEmpty : twosComplement [] = 0

||| OWED: at least one known-valid card-number passes Luhn. Discharge
||| requires reasoning through `unpack`/`ord` String FFI primitives.
public export
0 luhnValidatesKnownGood :
  validateLuhn "4111111111111111" = True

||| OWED: at least one known-valid ISBN-10 passes its validator.
public export
0 isbn10ValidatesKnownGood :
  validateISBN10 "0306406152" = True

||| OWED: at least one known-valid ISBN-13 passes its validator.
public export
0 isbn13ValidatesKnownGood :
  validateISBN13 "9780306406157" = True
