-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
|||
||| SafeRecord.Proofs — Formal proofs about record subset destructuring.
|||
||| Proves the key properties that make `{ extracted, ...rest } = record`
||| safe:
|||
|||   1. Exhaustiveness: extracted ∪ rest = full (no fields lost)
|||   2. Disjointness:   extracted ∩ rest = ∅   (no fields duplicated)
|||   3. Completeness:   |extracted| + |rest| = |full|
|||
||| These properties are impossible to express in ReScript's type system.
||| They require dependent types (to state the theorems) and total
||| functions (to construct the proofs). Ephapax's linear types will
||| enforce the single-use property at the value level; these proofs
||| operate at the type/schema level.
|||
module Proven.SafeRecord.Proofs

import Proven.SafeRecord.Types
import Data.Nat

%default total

-- =========================================================================
-- Helper: Elem and NotElem are contradictory
-- =========================================================================

||| If a field is both in and not in a schema, that is a contradiction.
||| Required for Idris 2 0.8.0 where lambda-case impossible syntax changed.
export
elemNotElemAbsurd : Elem f fs -> NotElem f fs -> Void
elemNotElemAbsurd Here (NotInCons neq _) = neq Refl
elemNotElemAbsurd (There later) (NotInCons _ rest) = elemNotElemAbsurd later rest

-- =========================================================================
-- Lemma: A field in the complement is NOT in the extracted set
-- =========================================================================

||| If a field appears in the remainder of a complement, it is not in
||| the extracted subset. This is half of the disjointness proof.
export
complementNotInExtracted : Complement sub full rest
                         -> Elem f rest
                         -> NotElem f sub
complementNotInExtracted (CompKeep notIn compRest) Here = notIn
complementNotInExtracted (CompKeep notIn compRest) (There later) =
  complementNotInExtracted compRest later
complementNotInExtracted (CompDrop _ compRest) elemRest =
  complementNotInExtracted compRest elemRest

-- =========================================================================
-- Lemma: A field in the extracted set is NOT in the remainder
-- =========================================================================

||| If a field appears in the extracted subset, it does not appear in
||| the remainder. This is the other half of the disjointness proof.
export
extractedNotInRemainder : {0 f : Field} -> Complement sub full rest
                        -> Elem f sub
                        -> NotElem f rest
extractedNotInRemainder CompNil _ = NotInNil
extractedNotInRemainder (CompKeep {f=hd} notInSub compRest) elemSub =
  let restProof = extractedNotInRemainder compRest elemSub
  in  NotInCons (\eq => void (elemNotElemAbsurd (replace {p = \x => Elem x sub} eq elemSub) notInSub))
      restProof
extractedNotInRemainder (CompDrop _ compRest) elemSub =
  extractedNotInRemainder compRest elemSub

-- =========================================================================
-- Theorem: The partition preserves length (completeness)
-- =========================================================================

||| The number of fields in extracted + remainder equals the number in full.
||| Counts the total number of fields seen in the complement witness.
export
partitionPreservesLength : Complement sub full rest -> Nat
partitionPreservesLength CompNil = Z
partitionPreservesLength (CompKeep _ compRest) =
  S (partitionPreservesLength compRest)
partitionPreservesLength (CompDrop _ compRest) =
  S (partitionPreservesLength compRest)

||| Count how many fields from `sub` appear in `full`.
export
countExtracted : Complement sub full rest -> Nat
countExtracted CompNil = Z
countExtracted (CompKeep _ compRest) = countExtracted compRest
countExtracted (CompDrop _ compRest) = S (countExtracted compRest)

||| Count how many fields are in the remainder.
export
countRemainder : Complement sub full rest -> Nat
countRemainder CompNil = Z
countRemainder (CompKeep _ compRest) = S (countRemainder compRest)
countRemainder (CompDrop _ compRest) = countRemainder compRest

||| The extracted count + remainder count = full length.
||| This is the completeness theorem: nothing is lost or duplicated.
export
completenessProof : (comp : Complement sub full rest)
                  -> countExtracted comp + countRemainder comp = length full
completenessProof CompNil = Refl
completenessProof (CompKeep _ compRest) =
  let ih = completenessProof compRest
  in  rewrite sym (plusSuccRightSucc (countExtracted compRest) (countRemainder compRest))
  in  cong S ih
completenessProof (CompDrop _ compRest) =
  let ih = completenessProof compRest
  in  cong S ih

-- =========================================================================
-- Theorem: Every field in full is in exactly one of extracted or rest
-- =========================================================================

||| For any field in the full schema, it is either in the extracted set
||| or in the remainder — never both, never neither.
||| This is the exhaustiveness + disjointness theorem combined.
export
data ExactlyOne : Field -> Schema -> Schema -> Type where
  InExtracted : Elem f sub -> NotElem f rest -> ExactlyOne f sub rest
  InRemainder : NotElem f sub -> Elem f rest -> ExactlyOne f sub rest

||| Given a valid complement, every field in `full` lands in exactly
||| one of `sub` or `rest`.
export
fieldInExactlyOne : Complement sub full rest
                  -> Elem f full
                  -> ExactlyOne f sub rest
fieldInExactlyOne (CompKeep notInSub compRest) Here =
  InRemainder notInSub Here
fieldInExactlyOne (CompKeep notInSub compRest) (There later) =
  case fieldInExactlyOne compRest later of
    InExtracted inSub notInRest =>
      InExtracted inSub (NotInCons (\eq =>
        -- f can't be the kept field (it's in sub) so it must be deeper
        elemNotElemAbsurd (rewrite sym eq in inSub) notInSub) notInRest)
    InRemainder notInSub' inRest =>
      InRemainder notInSub' (There inRest)
fieldInExactlyOne (CompDrop inSub compRest) Here =
  InExtracted inSub (extractedNotInRemainder compRest inSub)
fieldInExactlyOne (CompDrop inSub compRest) (There later) =
  case fieldInExactlyOne compRest later of
    InExtracted inSub' notInRest => InExtracted inSub' notInRest
    InRemainder notInSub' inRest => InRemainder notInSub' inRest

-- =========================================================================
-- Construction: Build a partition from a schema and a subset request
-- =========================================================================

||| Check if a field is in a list of fields (decidable).
export
isElem : (f : Field) -> (fs : Schema) -> Dec (Elem f fs)
isElem f [] = No (\case _ impossible)
isElem f (f' :: fs) = case fieldEq f f' of
  Yes prf => Yes (rewrite prf in Here)
  No neq  => case isElem f fs of
    Yes later => Yes (There later)
    No notLater => No (\case
      Here => neq Refl
      There l => notLater l)

||| Build a NotElem proof from a failed membership check.
export
buildNotElem : (f : Field) -> (fs : Schema) -> Not (Elem f fs) -> NotElem f fs
buildNotElem f [] _ = NotInNil
buildNotElem f (f' :: fs) notIn =
  NotInCons (\eq => notIn (rewrite eq in Here))
            (buildNotElem f fs (\later => notIn (There later)))

||| Compute the complement of `sub` in `full`, producing the remainder schema
||| and a proof that it is correct.
export
computeComplement : (sub : Schema) -> (full : Schema)
                  -> (rest : Schema ** Complement sub full rest)
computeComplement sub [] = ([] ** CompNil)
computeComplement sub (f :: fs) =
  let (rest ** compRest) = computeComplement sub fs
  in case isElem f sub of
    Yes inSub  => (rest ** CompDrop inSub compRest)
    No notIn   =>
      let notElemProof = buildNotElem f sub notIn
      in  (f :: rest ** CompKeep notElemProof compRest)
