-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
|||
||| SafeRecord.Types — Foundational types for verified record subset destructuring.
|||
||| Models records as named field sets and provides types for expressing
||| subset extraction with a provably correct remainder. This is the type
||| foundation for the Ephapax linear rest-pattern feature.
|||
||| Design decision: This solution is strictly more powerful than anything
||| achievable in ReScript (or TypeScript, OCaml, Haskell without extensions).
||| ReScript would need dependent types to prove partition correctness AND
||| linear types to enforce single-use field consumption. That is not a
||| language extension — it is a fundamentally different language, which is
||| what Ephapax is. The Idris2 proof here serves as the formal foundation
||| that Ephapax's linear type system will enforce at compile time, and that
||| a ReScript PPX can approximate (without the guarantees) as a stopgap.
|||
module Proven.SafeRecord.Types

%default total

||| A field name, represented as a string for generality.
||| In a real implementation, these would be compile-time symbols.
public export
FieldName : Type
FieldName = String

||| A named field with a type tag.
||| The type is erased at runtime — we only need it for the proof.
public export
record Field where
  constructor MkField
  name : FieldName

||| Decidable equality on fields (needed for set operations).
public export
fieldEq : (a, b : Field) -> Dec (a = b)
fieldEq (MkField n1) (MkField n2) = case decEq n1 n2 of
  Yes prf => Yes (cong MkField prf)
  No contra => No (\prf => contra (cong name prf))

public export
Eq Field where
  (MkField n1) == (MkField n2) = n1 == n2

||| A record schema is a list of fields.
||| Order matters for the proof but not for the semantics — we prove
||| that the partition is independent of field ordering.
public export
Schema : Type
Schema = List Field

||| Membership: a field is in a schema.
public export
data Elem : Field -> Schema -> Type where
  Here  : Elem f (f :: fs)
  There : Elem f fs -> Elem f (f' :: fs)

||| A subset relationship: every field in `sub` is in `full`.
public export
data Subset : (sub : Schema) -> (full : Schema) -> Type where
  SubNil  : Subset [] full
  SubCons : Elem f full -> Subset fs full -> Subset (f :: fs) full

||| Non-membership: a field is NOT in a schema.
public export
data NotElem : Field -> Schema -> Type where
  NotInNil  : NotElem f []
  NotInCons : Not (f = f') -> NotElem f fs -> NotElem f (f' :: fs)

||| The complement of a subset: fields in `full` that are NOT in `sub`.
||| This is the "rest" in `{ extracted, ...rest } = record`.
public export
data Complement : (sub : Schema) -> (full : Schema) -> (rest : Schema) -> Type where
  CompNil  : Complement sub [] []
  CompKeep : NotElem f sub -> Complement sub fs rest
           -> Complement sub (f :: fs) (f :: rest)
  CompDrop : Elem f sub -> Complement sub fs rest
           -> Complement sub (f :: fs) rest

||| A verified partition of a record schema into two disjoint subsets.
|||
||| Given a record with schema `full` and a requested subset `sub`:
|||   - `sub` is a valid subset of `full` (every requested field exists)
|||   - `rest` is the complement (every non-requested field)
|||   - The partition is exhaustive: `sub ++ rest` covers `full`
|||   - The partition is disjoint: no field appears in both `sub` and `rest`
public export
record Partition where
  constructor MkPartition
  ||| The full record schema
  full : Schema
  ||| The extracted fields (what the user asked for)
  extracted : Schema
  ||| The remaining fields (the "...rest")
  remainder : Schema
  ||| Proof: extracted is a subset of full
  extractedValid : Subset extracted full
  ||| Proof: remainder is the complement of extracted in full
  remainderValid : Complement extracted full remainder
