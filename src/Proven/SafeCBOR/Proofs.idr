-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCBOR (RFC 8949) data-model invariants.
|||
||| `Proven.SafeCBOR` ships the CBOR data model and validators. This
||| file machine-checks:
|||
|||   * `MajorType` enum self-equality (8 constructors — one per
|||     CBOR 3-bit major type).
|||   * `MajorType` Show wire-format anchors (RFC 8949 abbreviations).
|||   * `SimpleValue` enum self-equality (4 nullary constructors).
|||   * Well-known tag anchors (RFC 8949): tag 0 = date/time string,
|||     tag 1 = epoch, tag 2 = positive bignum, tag 24 = encoded CBOR,
|||     tag 32 = URI, tag 18 = COSE_Sign1.
|||   * `validateDepth Z _ = False` (depth-zero rejects everything).
|||   * `validateDepth _ (CBORSimple ...)` always succeeds at S k
|||     (terminal value, no recursion).
|||   * `hasDuplicateKeys []` is False (empty map has no dupes).
|||
||| OWED: large `Integer`-literal comparisons in `isValidInteger`,
||| `MaxNestingDepth = 64` Nat-opacity (standards#128).
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeCBOR.Proofs

import Proven.SafeCBOR

%default total

--------------------------------------------------------------------------------
-- MajorType enum self-equality
--------------------------------------------------------------------------------

public export
unsignedIntSelfEq : UnsignedInt == UnsignedInt = True
unsignedIntSelfEq = Refl

public export
negativeIntSelfEq : NegativeInt == NegativeInt = True
negativeIntSelfEq = Refl

public export
byteStringSelfEq : ByteString == ByteString = True
byteStringSelfEq = Refl

public export
textStringSelfEq : TextString == TextString = True
textStringSelfEq = Refl

public export
arraySelfEq : Array == Array = True
arraySelfEq = Refl

public export
mapSelfEq : Map == Map = True
mapSelfEq = Refl

public export
tagSelfEq : Tag == Tag = True
tagSelfEq = Refl

public export
simpleSelfEq : Simple == Simple = True
simpleSelfEq = Refl

public export
unsignedIntNotNegativeInt : UnsignedInt == NegativeInt = False
unsignedIntNotNegativeInt = Refl

--------------------------------------------------------------------------------
-- MajorType wire-format anchors
--------------------------------------------------------------------------------

public export
unsignedIntShow : show UnsignedInt = "uint"
unsignedIntShow = Refl

public export
negativeIntShow : show NegativeInt = "nint"
negativeIntShow = Refl

public export
byteStringShow : show ByteString = "bstr"
byteStringShow = Refl

public export
textStringShow : show TextString = "tstr"
textStringShow = Refl

public export
arrayShow : show Array = "array"
arrayShow = Refl

public export
mapShow : show Map = "map"
mapShow = Refl

public export
tagShow : show Tag = "tag"
tagShow = Refl

public export
simpleShow : show Simple = "simple"
simpleShow = Refl

--------------------------------------------------------------------------------
-- SimpleValue enum self-equality (4 nullary constructors)
--------------------------------------------------------------------------------

public export
cborFalseSelfEq : CBORFalse == CBORFalse = True
cborFalseSelfEq = Refl

public export
cborTrueSelfEq : CBORTrue == CBORTrue = True
cborTrueSelfEq = Refl

public export
cborNullSelfEq : CBORNull == CBORNull = True
cborNullSelfEq = Refl

public export
cborUndefinedSelfEq : CBORUndefined == CBORUndefined = True
cborUndefinedSelfEq = Refl

public export
cborFalseShow : show CBORFalse = "false"
cborFalseShow = Refl

public export
cborTrueShow : show CBORTrue = "true"
cborTrueShow = Refl

public export
cborNullShow : show CBORNull = "null"
cborNullShow = Refl

public export
cborUndefinedShow : show CBORUndefined = "undefined"
cborUndefinedShow = Refl

--------------------------------------------------------------------------------
-- Well-known CBOR tag anchors (RFC 8949)
--------------------------------------------------------------------------------

||| `dateTimeString` wraps a text value in tag 0.
public export
dateTimeStringTag :
  (s : String) -> dateTimeString s = CBORTagged 0 (CBORText s)
dateTimeStringTag s = Refl

||| `epochDateTime` uses tag 1.
public export
epochDateTimeTag :
  (n : Integer) -> epochDateTime n = CBORTagged 1 (CBORUInt n)
epochDateTimeTag n = Refl

||| `positiveBignum` uses tag 2.
public export
positiveBignumTag :
  (bs : List Bits8) -> positiveBignum bs = CBORTagged 2 (CBORBytes bs)
positiveBignumTag bs = Refl

||| `encodedCBOR` uses tag 24.
public export
encodedCBORTag :
  (bs : List Bits8) -> encodedCBOR bs = CBORTagged 24 (CBORBytes bs)
encodedCBORTag bs = Refl

||| `uri` uses tag 32.
public export
uriTag :
  (s : String) -> uri s = CBORTagged 32 (CBORText s)
uriTag s = Refl

||| `coseSign1` uses tag 18.
public export
coseSign1Tag :
  (v : CBORValue) -> coseSign1 v = CBORTagged 18 v
coseSign1Tag v = Refl

--------------------------------------------------------------------------------
-- `validateDepth` boundary
--------------------------------------------------------------------------------

||| Depth-zero rejects ANY value (including terminal ones).
public export
depthZeroRejectsUInt : (n : Integer) -> validateDepth Z (CBORUInt n) = False
depthZeroRejectsUInt n = Refl

public export
depthZeroRejectsArray : (xs : List CBORValue) -> validateDepth Z (CBORArray xs) = False
depthZeroRejectsArray xs = Refl

||| At S k, a terminal `CBORUInt` is always accepted.
public export
depthSAcceptsUInt :
  (k : Nat) -> (n : Integer) -> validateDepth (S k) (CBORUInt n) = True
depthSAcceptsUInt k n = Refl

||| At S k, a terminal `CBORSimple` is always accepted.
public export
depthSAcceptsSimple :
  (k : Nat) -> (s : SimpleValue) -> validateDepth (S k) (CBORSimple s) = True
depthSAcceptsSimple k s = Refl

--------------------------------------------------------------------------------
-- `hasDuplicateKeys` base case
--------------------------------------------------------------------------------

||| An empty map has no duplicate keys.
public export
emptyMapNoDupes : hasDuplicateKeys [] = False
emptyMapNoDupes = Refl

--------------------------------------------------------------------------------
-- OWED
--------------------------------------------------------------------------------

||| DISCHARGED: `MaxNestingDepth = 64`. `MaxNestingDepth` is `public
||| export` with body `64` (SafeCBOR.idr L150-152). Both sides are
||| the same `Nat` literal — `Refl` closes without unary expansion.
public export
maxNestingDepthAnchor : MaxNestingDepth = 64
maxNestingDepthAnchor = Refl
