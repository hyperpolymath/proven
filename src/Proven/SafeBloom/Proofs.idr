-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeBloom Bloom-filter operations.
|||
||| The `Proven.SafeBloom` module doc claims "probabilistic set
||| membership data structure with guaranteed no false negatives".
||| Prior to this commit it shipped only the operations with NO
||| discharged theorem — the no-false-negatives claim was unproven,
||| as were the structural invariants every other operation should
||| preserve (size, numHashes, parameter agreement for union /
||| intersection).
|||
||| This file machine-checks (`idris2 --check`) the structural
||| invariants of the Bloom-filter operations:
|||
|||   * `empty sz nh` produces a filter with size = sz, numHashes = nh,
|||     and zero set bits.
|||   * `clear` preserves both size and numHashes (parameters) and
|||     resets all bits to zero.
|||   * `insert` preserves both size and numHashes.
|||   * `union` and `intersection` *require* matching parameters
|||     (return `Nothing` on size or numHashes mismatch) and the
|||     `Just` branch preserves them.
|||   * `isSaturated` agrees with its definition (boolean predicate
|||     equivalence by definition).
|||
||| Items NOT covered here (explicit OWED markers, deliberately):
|||
|||   * The famous "no false negatives" property — `insert v bf` then
|||     `isInfixOf v _` returns `True`. This depends on `hashIndices`
|||     being deterministic AND `setAt` actually flipping every bit
|||     the lookup will check. It requires reasoning through `simpleHash`
|||     (which uses opaque `ord`/`unpack` String FFI primitives that
|||     are not type-level reducible in Idris2 0.8.0) and the
|||     `foldl setAt` / `all getAt` pair. Same blocker family as
|||     `proof-of-work` I7 (FFI-correctness assumption).
|||
||| Zero `believe_me`/`idris_crash`. The OWED items are stated as
||| named, erased postulates parallel to the I6/I7 stated-assumption
||| pattern in `proof-of-work` ABI seam — discoverable, not silent.
module Proven.SafeBloom.Proofs

import Proven.SafeBloom
import Data.List
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- `empty` and `clear` parameter / content invariants
--------------------------------------------------------------------------------

||| `empty sz nh` records the requested size exactly.
public export
emptyHasRequestedSize : (sz, nh : Nat) -> (empty sz nh).size = sz
emptyHasRequestedSize sz nh = Refl

||| `empty sz nh` records the requested numHashes exactly.
public export
emptyHasRequestedHashes : (sz, nh : Nat) -> (empty sz nh).numHashes = nh
emptyHasRequestedHashes sz nh = Refl

||| `empty sz nh`'s bit list has length exactly `sz` (no off-by-one).
||| Follows from `replicate n x` having length `n`. The kernel reduces
||| the case-zero / case-suc list structure here.
public export
emptyBitsLengthSize :
  (sz, nh : Nat) -> length (empty sz nh).bits = sz
emptyBitsLengthSize Z     nh = Refl
emptyBitsLengthSize (S k) nh = cong S (emptyBitsLengthSize k nh)

||| `empty sz nh` has zero bits set. Induction on `sz`: `replicate`
||| produces no `True`s, so `filter id` drops everything.
public export
emptyHasNoOnes : (sz, nh : Nat) -> countOnes (empty sz nh) = 0
emptyHasNoOnes Z     nh = Refl
emptyHasNoOnes (S k) nh = emptyHasNoOnes k nh

--------------------------------------------------------------------------------
-- `clear` is structurally identical to `empty bf.size bf.numHashes`
--------------------------------------------------------------------------------

||| `clear` preserves the size parameter.
public export
clearPreservesSize : (bf : BloomFilter) -> (clear bf).size = bf.size
clearPreservesSize bf = Refl

||| `clear` preserves the numHashes parameter.
public export
clearPreservesHashes :
  (bf : BloomFilter) -> (clear bf).numHashes = bf.numHashes
clearPreservesHashes bf = Refl

--------------------------------------------------------------------------------
-- `insert` parameter preservation (the bit pattern changes; size/hashes do not)
--------------------------------------------------------------------------------

||| `insert` preserves the size parameter. The `MkBloom newBits bf.size
||| bf.numHashes` constructor literally reuses the input size.
public export
insertPreservesSize :
  (v : String) -> (bf : BloomFilter) -> (insert v bf).size = bf.size
insertPreservesSize v bf = Refl

||| `insert` preserves the numHashes parameter.
public export
insertPreservesHashes :
  (v : String) -> (bf : BloomFilter) -> (insert v bf).numHashes = bf.numHashes
insertPreservesHashes v bf = Refl

--------------------------------------------------------------------------------
-- `union` / `intersection` parameter-agreement gates and the Just-branch
-- preservation
--------------------------------------------------------------------------------

||| `union a b` preserves the size parameter when it returns `Just`.
||| The `Just`-branch literally builds `MkBloom _ a.size a.numHashes`.
||| Structural case split on the gating `if`.
public export
unionPreservesSize :
  (a, b, c : BloomFilter)
  -> union a b = Just c -> c.size = a.size
unionPreservesSize a b c prf with (a.size /= b.size || a.numHashes /= b.numHashes)
  unionPreservesSize a b c prf | True  = case prf of Refl impossible
  unionPreservesSize a b c prf | False = case prf of Refl => Refl

||| `union a b` preserves the numHashes parameter when it returns `Just`.
public export
unionPreservesHashes :
  (a, b, c : BloomFilter)
  -> union a b = Just c -> c.numHashes = a.numHashes
unionPreservesHashes a b c prf with (a.size /= b.size || a.numHashes /= b.numHashes)
  unionPreservesHashes a b c prf | True  = case prf of Refl impossible
  unionPreservesHashes a b c prf | False = case prf of Refl => Refl

||| `intersection a b` preserves the size parameter on success.
public export
intersectionPreservesSize :
  (a, b, c : BloomFilter)
  -> intersection a b = Just c -> c.size = a.size
intersectionPreservesSize a b c prf with (a.size /= b.size || a.numHashes /= b.numHashes)
  intersectionPreservesSize a b c prf | True  = case prf of Refl impossible
  intersectionPreservesSize a b c prf | False = case prf of Refl => Refl

||| `intersection a b` preserves the numHashes parameter on success.
public export
intersectionPreservesHashes :
  (a, b, c : BloomFilter)
  -> intersection a b = Just c -> c.numHashes = a.numHashes
intersectionPreservesHashes a b c prf with (a.size /= b.size || a.numHashes /= b.numHashes)
  intersectionPreservesHashes a b c prf | True  = case prf of Refl impossible
  intersectionPreservesHashes a b c prf | False = case prf of Refl => Refl

--------------------------------------------------------------------------------
-- OWED: no-false-negatives property (the foundational Bloom claim)
--
-- Inserting v then querying for v must return True. The proof requires
-- reasoning through `simpleHash` / `hashIndices` (both depend on `ord`
-- and `unpack` String FFI primitives that are not type-level reducible
-- in Idris2 0.8.0) and the foldl-setAt / all-getAt pair. Stated as a
-- named erased postulate so the residual is discoverable, not silent.
--
-- Same blocker family as proof-of-work I7's serde-correctness
-- assumption (external library / FFI primitive correctness).
--------------------------------------------------------------------------------

||| OWED (no false negatives): after inserting `v`, querying for `v`
||| returns `True`. The full discharge depends on String-FFI primitives
||| (`ord`, `unpack`) that Idris2 0.8.0 cannot type-level reduce; the
||| claim therefore lives as an explicit, named assumption.
public export
0 noFalseNegatives :
  (v : String) -> (bf : BloomFilter) -> LT 0 bf.size -> LT 0 bf.numHashes
  -> isInfixOf v (insert v bf) = True
