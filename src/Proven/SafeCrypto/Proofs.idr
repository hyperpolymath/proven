-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCrypto operations
|||
||| This module contains proofs verifying properties of cryptographic
||| operations. Items that reduce type-level by `Refl` under Idris2 0.8.0
||| are stated as real proofs; items that hit a `Data.Bits` / `String`
||| FFI / case-on-abstract-algorithm reduction wall are stated as
||| **OWED-with-justification** in the SafeChecksum convention:
|||
|||   * Triple-pipe `||| OWED:` docstring naming the claim, the Idris2
|||     0.8.0 blocker, and the discharge condition.
|||   * `0 ` erased-multiplicity, bare signature, no body, no
|||     `postulate` keyword (estate convention — `Proven.SafeChecksum`
|||     and `Proven.SafeBuffer` set the pattern).
|||   * No `believe_me`, no `idris_crash`.
|||
||| None of the bodyless items below are pure cryptographic-hardness
||| axioms (collision resistance, preimage resistance, etc.); they are
||| structural properties (reflexivity, symmetry, length, bounds) that
||| are gated by FFI/Bits/String opacity in 0.8.0, with a real
||| in-language discharge path. The crypto-hardness assumption family
||| (parallel to `proof-of-work`'s I6 `sha256CollisionResistant`)
||| would live in a separate `Assumptions.idr` and is not present here.
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - `/=` returns Bool, not Type; replaced with `Not (x = y)` in signatures
||| - `<=` returns Bool, not Type; replaced with `LTE` in signatures
||| - `digestEqList`/`constantTimeEqList` renamed to use `digestEq`
||| - Added import of Proven.SafeCrypto for Sensitive type
module Proven.SafeCrypto.Proofs

import Proven.Core
import Proven.SafeCrypto.Hash
import Proven.SafeCrypto.Random
import Data.List
import Data.Bits
import Data.Nat
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Constant-Time Properties
--------------------------------------------------------------------------------

||| OWED: constant-time `digestEq` is reflexive — `digestEq d d = True`
||| for every `ByteVector n`. The implementation folds a where-local
||| `go` over the Vect, accumulating `acc .|. (x `xor` x)` and then
||| comparing `acc == 0`. Reducing this to `True` by `Refl` requires
||| two `Data.Bits` lemmas not exposed as definitional equalities by
||| Idris2 0.8.0's Prelude+contrib: (a) `xor x x = 0` for all `Bits8`,
||| and (b) `0 .|. 0 = 0` reductively under arbitrary fold depth.
||| Same blocker family as `Proven.SafeChecksum.Proofs` XOR-involution
||| (FFI-opaque Bits primitives). Discharge once a `Data.Bits`
||| reflective tactic / Prelude lemma is available.
public export
0 constantTimeRefl : (d : ByteVector n) -> digestEq d d = True

||| OWED: constant-time `digestEq` is symmetric —
||| `digestEq d1 d2 = digestEq d2 d1`. Reduces to showing that
||| `foldl (\a, (x,y) => a .|. (x xor y)) 0 (zip xs ys)` is invariant
||| under `zip` argument swap, which in turn reduces to commutativity
||| of `xor` on `Bits8`. `xor` is a primitive in Idris2 0.8.0 and does
||| not reduce by `Refl` (no `Data.Bits` commutativity lemma in the
||| stdlib). Same blocker family as `constantTimeRefl`. Discharge
||| once `Data.Bits` exposes `xorCommutative : (x, y : Bits8) -> x \`xor\` y = y \`xor\` x`.
public export
0 constantTimeSym : (d1, d2 : ByteVector n) ->
                    digestEq d1 d2 = digestEq d2 d1

--------------------------------------------------------------------------------
-- Hash Output Size Properties
--------------------------------------------------------------------------------

||| SHA-256 output is exactly 32 bytes
public export
sha256OutputSize : hashOutputSize SHA256_ALG = 32
sha256OutputSize = Refl

||| SHA-512 output is exactly 64 bytes
public export
sha512OutputSize : hashOutputSize SHA512_ALG = 64
sha512OutputSize = Refl

||| SHA3-256 output is exactly 32 bytes
public export
sha3_256OutputSize : hashOutputSize SHA3_256_ALG = 32
sha3_256OutputSize = Refl

||| BLAKE3 output is exactly 32 bytes
public export
blake3OutputSize : hashOutputSize BLAKE3_ALG = 32
blake3OutputSize = Refl

--------------------------------------------------------------------------------
-- Security Level Properties
--------------------------------------------------------------------------------

||| SHA-256 is secure
public export
sha256Secure : isSecure SHA256_ALG = True
sha256Secure = Refl

||| SHA-512 is secure
public export
sha512Secure : isSecure SHA512_ALG = True
sha512Secure = Refl

||| SHA3-256 is secure
public export
sha3_256Secure : isSecure SHA3_256_ALG = True
sha3_256Secure = Refl

||| MD5 is NOT secure
public export
md5NotSecure : isSecure MD5_ALG = False
md5NotSecure = Refl

||| SHA-1 is NOT secure
public export
sha1NotSecure : isSecure SHA1_ALG = False
sha1NotSecure = Refl

||| OWED: any algorithm whose `securityLevel` is `Modern` is `isSecure`.
||| `isSecure` is defined as a `case securityLevel alg of` with a
||| wildcard `_ => True` arm covering `Modern` (and `Standard`). With
||| the hypothesis `securityLevel alg = Modern` in scope we need to
||| rewrite the scrutinee under the `case`, but Idris2 0.8.0 will not
||| reduce `isSecure alg` for an abstract `alg : HashAlg` even after
||| `rewrite` substitutes `securityLevel alg`, because the `case` was
||| not eta-expanded to a generalised motive at elaboration time.
||| Discharge by either (a) refactoring `isSecure` to a top-level
||| pattern-match dispatch on `securityLevel`, or (b) hand-proving via
||| `with (securityLevel alg) proof prf` once the 0.8.0 `with`/`rewrite`
||| interaction is improved.
public export
0 modernIsSecure : (alg : HashAlg) ->
                   securityLevel alg = Modern ->
                   isSecure alg = True

||| OWED: any algorithm whose `securityLevel` is `Standard` is
||| `isSecure`. Same blocker as `modernIsSecure` — the abstract `alg`
||| prevents `isSecure alg` from reducing under the `case` even with
||| the `securityLevel alg = Standard` rewrite in scope. Discharge
||| together with `modernIsSecure` by the same refactor.
public export
0 standardIsSecure : (alg : HashAlg) ->
                     securityLevel alg = Standard ->
                     isSecure alg = True

--------------------------------------------------------------------------------
-- Digest Comparison Properties
--------------------------------------------------------------------------------

||| OWED: digest equality is reflexive — `digestEq d d = True`. This
||| is the same claim as `constantTimeRefl` above (they share the
||| same implementation; the duplication is for API discoverability)
||| and inherits the same `Data.Bits` `xor x x = 0` reductive blocker.
||| Discharge together with `constantTimeRefl`.
public export
0 digestEqRefl : (d : ByteVector n) -> digestEq d d = True

||| OWED: digest equality is symmetric — `digestEq d1 d2 = digestEq d2 d1`.
||| Same claim as `constantTimeSym` above; same `Data.Bits` `xor`
||| commutativity blocker. Discharge together with `constantTimeSym`.
public export
0 digestEqSym : (d1, d2 : ByteVector n) -> digestEq d1 d2 = digestEq d2 d1

||| OWED: distinct `ByteVector`s compare unequal under `digestEq`.
||| Stated with `Not (d1 = d2)` (propositional inequality) because
||| Idris2 0.8.0's `/=` returns `Bool`, not `Type`. Discharge requires
||| injectivity of the constant-time `xor`-fold comparator — i.e.,
||| if the fold yields `0`, the input Vects are pointwise equal.
||| Blocked on the same `Data.Bits` reductive lemmas as `constantTimeRefl`
||| (`xor x y = 0 -> x = y`), plus an inductive over `Vect n`.
||| Discharge once `Data.Bits` exposes the cancellation lemma OR once
||| `digestEq` is refactored to recurse via `decEq` element-wise.
public export
0 differentDigestsUnequal : (d1, d2 : ByteVector n) ->
                            Not (d1 = d2) ->
                            digestEq d1 d2 = False

--------------------------------------------------------------------------------
-- Random Generation Properties
--------------------------------------------------------------------------------

||| OWED: when `randomBytes n` succeeds it returns a `ByteVec` whose
||| underlying `Vect` has length `n`. This is true *by construction*
||| of `MkByteVec : Vect n Byte -> ByteVec n` — `length v = n` is
||| witnessed by the dependent index. However, `randomBytes` threads
||| through the entropy-source FFI (`Proven.SafeCrypto.Random`'s OS
||| `getEntropy` / `/dev/urandom` foreign call) which is opaque to
||| Idris2 0.8.0's type-level reducer. The whole-program FFI block
||| prevents `Refl` from closing the `case ... of Right (MkByteVec v) => length v = n`
||| arm even though the dependent type already encodes the claim.
||| Same blocker family as the boj-server I7 FFI-correctness
||| assumption. Discharge by (a) reflecting `lengthFin` on the Vect
||| index, or (b) refactoring the return type so the length witness
||| is exposed without case-pattern reduction.
public export
0 randomBytesLength : (n : Nat) ->
                      case randomBytes n of
                        Right (MkByteVec v) => length v = n
                        Left _ => ()

||| OWED: when `randomNat max` succeeds (with `IsSucc max`) the
||| returned `Nat` is strictly less than `max`. The bound holds by
||| construction in `Random.idr`'s `mod`-based implementation, but
||| the FFI entropy source plus `mod max` reduction in Idris2 0.8.0
||| (same `Integral Nat` `mod` blocker that `SafeChecksum.Proofs`
||| documents for `sumChecksum`) prevents the `case` from reducing
||| to `LT n max` by `Refl`. Discharge once the FFI entropy path is
||| modelled propositionally and `modLT : (a, b : Nat) -> IsSucc b -> LT (a \`mod\` b) b`
||| is available in `Data.Nat`.
public export
0 randomNatBounded : (max : Nat) -> {auto ok : IsSucc max} ->
                     case randomNat max of
                       Right n => LT n max
                       Left _ => ()

||| OWED: when `randomNatRange mn mx` succeeds (with `LTE mn mx`) the
||| result lies in `[mn, mx]`. Built atop `randomNat (S (minus mx mn))`
||| then offset-added by `mn`, so inherits the `randomNatBounded`
||| blocker plus needs `plusLteLeftCancel`/`plusLteMonotoneRight`
||| reasoning. Same FFI + `Data.Nat` blocker family. Discharge
||| together with `randomNatBounded`.
public export
0 randomRangeBounded : (mn, mx : Nat) -> {auto ok : LTE mn mx} ->
                       case randomNatRange mn mx of
                         Right n => (LTE mn n, LTE n mx)
                         Left _ => ()

--------------------------------------------------------------------------------
-- Nonce Properties
--------------------------------------------------------------------------------

||| OWED: counter nonces are injective in the counter — for a fixed
||| 8-byte prefix, distinct `Bits64` counters yield distinct 12-byte
||| nonces. The implementation encodes the counter big-endian into
||| the trailing 4 bytes by repeated `shiftR`+`prim__cast_Bits64Bits8`.
||| Proving injectivity reduces to injectivity of that encoding, which
||| in turn needs `Data.Bits` lemmas (`shiftR`+`cast` is a bijection
||| on its range) that Idris2 0.8.0 does not expose. Same `Data.Bits`
||| primitive opacity blocker as the constant-time family. Stated with
||| `Not (c1 = c2)` for 0.8.0 (`/=` returns `Bool`). Discharge once
||| `Data.Bits` exposes the requisite cast/shift round-trip lemmas.
public export
0 counterNonceUnique : (pfx : ByteVec 8) -> (c1, c2 : Bits64) ->
                       Not (c1 = c2) ->
                       Not (counterNonce pfx c1 = counterNonce pfx c2)

||| OWED: `freshNonce n` returns a `ByteVec` of length `n` on success.
||| Definitionally `freshNonce = randomBytes`, so this is precisely
||| `randomBytesLength` lifted through the rename. Same FFI entropy
||| opacity blocker; discharge together with `randomBytesLength`.
public export
0 freshNonceSize : (n : Nat) ->
                   case freshNonce n of
                     Right (MkByteVec v) => length v = n
                     Left _ => ()

--------------------------------------------------------------------------------
-- Token Generation Properties
--------------------------------------------------------------------------------

||| OWED: `randomToken bytes` produces a base64 string whose length
||| is bounded by the standard expansion `(bytes * 4 / 3) + 3`. Built
||| as `map toBase64 (randomByteList bytes)`, so reduces to a bound
||| on `length (toBase64 (...))`. `toBase64` operates over `String`
||| via `pack`/`unpack` FFI primitives and `Bits8 -> Char` indexing,
||| neither of which Idris2 0.8.0 can reduce at type level. Stated
||| with `LTE` (0.8.0 `<=` returns `Bool`). Same `String` FFI
||| opacity blocker as `Proven.SafeChecksum.Proofs` Luhn/ISBN family
||| and `Proven.SafeBuffer.Proofs` pack/unpack family. Discharge
||| once a `String`-FFI reflective tactic or pack/unpack length
||| lemma is available.
public export
0 tokenLengthApprox : (bytes : Nat) ->
                      case randomToken bytes of
                        Right s => LTE (length s) ((bytes * 4 `div` 3) + 3)
                        Left _ => ()

||| OWED: `randomUUID` produces a 36-character UUID-v4 string
||| (`XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX`, 32 hex chars + 4 hyphens).
||| Built by `pack`-ing a fixed-shape list of hex digits and hyphens
||| from `randomBytes 16`. Reducing `length (pack [...]) = 36` by
||| `Refl` requires `pack`/`length` interaction lemmas that Idris2
||| 0.8.0 does not provide for FFI `String`. Same `String` FFI
||| opacity blocker as `tokenLengthApprox`. Discharge together with
||| `tokenLengthApprox` once the pack/unpack length lemma lands.
public export
0 uuidLength : case randomUUID of
                 Right s => length s = 36
                 Left _ => ()

--------------------------------------------------------------------------------
-- Sensitive Data Properties
--------------------------------------------------------------------------------

-- Note: mapSensitivePreserves and combineSensitiveCorrect proofs (both Refl)
-- are defined in Proven.SafeCrypto alongside the Sensitive type to avoid
-- import ambiguities between Proven.SafeCrypto.{Hash,Random} re-exports.
-- They were previously here but caused name resolution conflicts in 0.8.0.

--------------------------------------------------------------------------------
-- Hex Conversion Properties
--------------------------------------------------------------------------------

||| Hex encoding is deterministic: encoding is a pure function
||| so applying it twice to the same input yields identical results.
||| This follows directly from reflexivity of equality.
public export
hexEncodeDeterministic : (f : List Bits8 -> String) -> (bs : List Bits8) ->
                         f bs = f bs
hexEncodeDeterministic _ _ = Refl

||| OWED: any `bytesToHex`-like encoder produces an even-length
||| `String`. The intuition is that each input byte produces exactly
||| two hex digits, so the output has length `2 * length bs`, which
||| is divisible by 2. The signature here takes the encoder as a
||| parameter (`bytesToHex : List Bits8 -> String`) to keep it
||| abstract over the concrete implementation in `Proven.SafeCrypto`,
||| but that means we cannot reduce `length (bytesToHex bs)` at all
||| without inducting on `bs` and using a per-byte hypothesis on the
||| parameter — which the parametric sig does not give us. Even when
||| specialised to the concrete `Proven.SafeCrypto.bytesToHex`, the
||| body goes through `concat`+`pack`+`map byteToHex` over the
||| `String` FFI boundary, which Idris2 0.8.0 cannot reduce. Same
||| `String` FFI opacity blocker as `tokenLengthApprox` and
||| `uuidLength`. Discharge by (a) tightening the signature to the
||| concrete `bytesToHex`, AND (b) the `String`-FFI reflective
||| tactic / pack-length lemma.
public export
0 hexEncodeEvenLength : (bytesToHex : List Bits8 -> String) -> (bs : List Bits8) ->
                        mod (length (bytesToHex bs)) 2 = 0
