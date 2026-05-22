-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeAngle unit-tagged angle operations.
|||
||| The `Proven.SafeAngle` module ships unit-tagged angles plus
||| conversion / normalisation / trigonometric wrappers but no
||| machine-checked theorem that the conversions and gates are
||| consistent. This file discharges the type-level reducible
||| invariants:
|||
|||   * `AngleUnit` enum self-equality under the hand-written `Eq`
|||     instance (all 4 constructors).
|||   * `AngleUnit` cross-pair inequality witness (anchors the
|||     `_ == _ = False` fall-through).
|||   * `toRadians` / `toDegrees` / `toGradians` / `toTurns` identity
|||     on their own unit (no spurious scaling when the unit is
|||     already correct).
|||   * `fromDegrees` / `fromRadians` constructor record-projection
|||     (built angle round-trips its own unit).
|||   * `safeAsin` / `safeAcos` boundary rejection on out-of-range
|||     inputs (validates the domain gate).
|||   * `normalize` preserves the unit tag (the gate keeps you in
|||     the unit you started in — no silent conversion).
|||
||| Items NOT covered here (explicit OWED markers):
|||
|||   * `Double` value-level equalities (e.g. `toDegrees (fromDegrees 90) = 90`).
|||     Idris2 0.8.0 cannot `Refl`-decide arbitrary IEEE-754 doubles;
|||     this needs an external numerical-equivalence tactic.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeAngle.Proofs

import Proven.SafeAngle

%default total

--------------------------------------------------------------------------------
-- AngleUnit self-equality (4 constructors)
--------------------------------------------------------------------------------

||| `Degrees` is self-equal under `Eq AngleUnit`.
public export
degreesSelfEq : Degrees == Degrees = True
degreesSelfEq = Refl

||| `Radians` is self-equal under `Eq AngleUnit`.
public export
radiansSelfEq : Radians == Radians = True
radiansSelfEq = Refl

||| `Gradians` is self-equal under `Eq AngleUnit`.
public export
gradiansSelfEq : Gradians == Gradians = True
gradiansSelfEq = Refl

||| `Turns` is self-equal under `Eq AngleUnit`.
public export
turnsSelfEq : Turns == Turns = True
turnsSelfEq = Refl

||| Distinct units are unequal — sample pair anchors the
||| `_ == _ = False` fall-through.
public export
degreesNotRadians : Degrees == Radians = False
degreesNotRadians = Refl

||| `Gradians` and `Turns` are distinct.
public export
gradiansNotTurns : Gradians == Turns = False
gradiansNotTurns = Refl

--------------------------------------------------------------------------------
-- Conversion identity on own unit
--------------------------------------------------------------------------------

||| `toRadians` of a `Radians`-tagged angle is the underlying value
||| unchanged.
public export
toRadiansOnRadians : (v : Double) -> toRadians (MkAngle v Radians) = v
toRadiansOnRadians v = Refl

||| `toDegrees` of a `Degrees`-tagged angle is the underlying value
||| unchanged.
public export
toDegreesOnDegrees : (v : Double) -> toDegrees (MkAngle v Degrees) = v
toDegreesOnDegrees v = Refl

||| `toGradians` of a `Gradians`-tagged angle is the underlying value
||| unchanged.
public export
toGradiansOnGradians : (v : Double) -> toGradians (MkAngle v Gradians) = v
toGradiansOnGradians v = Refl

||| `toTurns` of a `Turns`-tagged angle is the underlying value
||| unchanged.
public export
toTurnsOnTurns : (v : Double) -> toTurns (MkAngle v Turns) = v
toTurnsOnTurns v = Refl

--------------------------------------------------------------------------------
-- Constructors record-project as declared
--------------------------------------------------------------------------------

||| `fromDegrees d` records `unit = Degrees`.
public export
fromDegreesUnit : (d : Double) -> (fromDegrees d).unit = Degrees
fromDegreesUnit d = Refl

||| `fromDegrees d` records `value = d`.
public export
fromDegreesValue : (d : Double) -> (fromDegrees d).value = d
fromDegreesValue d = Refl

||| `fromRadians r` records `unit = Radians`.
public export
fromRadiansUnit : (r : Double) -> (fromRadians r).unit = Radians
fromRadiansUnit r = Refl

||| `fromRadians r` records `value = r`.
public export
fromRadiansValue : (r : Double) -> (fromRadians r).value = r
fromRadiansValue r = Refl

--------------------------------------------------------------------------------
-- `normalize` preserves the unit tag
--------------------------------------------------------------------------------

||| `normalize` on a `Degrees`-tagged angle stays in degrees.
public export
normalizePreservesDegrees : (v : Double)
                         -> (normalize (MkAngle v Degrees)).unit = Degrees
normalizePreservesDegrees v = Refl

||| `normalize` on a `Radians`-tagged angle stays in radians.
public export
normalizePreservesRadians : (v : Double)
                         -> (normalize (MkAngle v Radians)).unit = Radians
normalizePreservesRadians v = Refl

||| `normalize` on a `Gradians`-tagged angle stays in gradians.
public export
normalizePreservesGradians : (v : Double)
                          -> (normalize (MkAngle v Gradians)).unit = Gradians
normalizePreservesGradians v = Refl

||| `normalize` on a `Turns`-tagged angle stays in turns.
public export
normalizePreservesTurns : (v : Double)
                       -> (normalize (MkAngle v Turns)).unit = Turns
normalizePreservesTurns v = Refl

--------------------------------------------------------------------------------
-- `safeAsin` / `safeAcos` domain gate
--------------------------------------------------------------------------------

||| `safeAsin` rejects input above 1.0 (returns `Nothing`).
public export
safeAsinRejectsAbove : safeAsin 2.0 = Nothing
safeAsinRejectsAbove = Refl

||| `safeAsin` rejects input below -1.0 (returns `Nothing`).
public export
safeAsinRejectsBelow : safeAsin (-2.0) = Nothing
safeAsinRejectsBelow = Refl

||| `safeAcos` rejects input above 1.0 (returns `Nothing`).
public export
safeAcosRejectsAbove : safeAcos 2.0 = Nothing
safeAcosRejectsAbove = Refl

||| `safeAcos` rejects input below -1.0 (returns `Nothing`).
public export
safeAcosRejectsBelow : safeAcos (-2.0) = Nothing
safeAcosRejectsBelow = Refl

--------------------------------------------------------------------------------
-- Constructors of derived angles record their declared unit
--------------------------------------------------------------------------------

||| `safeAtan` always returns a `Radians`-tagged angle.
public export
safeAtanIsRadians : (x : Double) -> (safeAtan x).unit = Radians
safeAtanIsRadians x = Refl

||| `safeAtan2` always returns a `Radians`-tagged angle.
public export
safeAtan2IsRadians : (y, x : Double) -> (safeAtan2 y x).unit = Radians
safeAtan2IsRadians y x = Refl

||| `addAngle` returns a `Radians`-tagged angle.
public export
addAngleIsRadians : (a, b : Angle) -> (addAngle a b).unit = Radians
addAngleIsRadians a b = Refl

||| `subAngle` returns a `Radians`-tagged angle.
public export
subAngleIsRadians : (a, b : Angle) -> (subAngle a b).unit = Radians
subAngleIsRadians a b = Refl

||| `scaleAngle` returns a `Radians`-tagged angle.
public export
scaleAngleIsRadians : (s : Double) -> (a : Angle) -> (scaleAngle s a).unit = Radians
scaleAngleIsRadians s a = Refl

||| `angleDifference` returns a `Radians`-tagged angle.
public export
angleDifferenceIsRadians : (a, b : Angle) -> (angleDifference a b).unit = Radians
angleDifferenceIsRadians a b = Refl

--------------------------------------------------------------------------------
-- Explicit OWED postulates (named, erased, discoverable)
--
-- Idris2 0.8.0 cannot Refl-decide Double value-level equalities; the
-- claim "toDegrees (fromDegrees 90.0) = 90.0" needs an external IEEE-754
-- numerical-equivalence tactic. Stated as named, erased postulates so
-- the residual debt is visible.
--------------------------------------------------------------------------------

||| OWED: `fromDegrees` round-trips through `toDegrees`. Blocked on
||| the absence of a numerical-equivalence tactic for `Double` literals
||| in Idris2 0.8.0.
public export
0 fromToDegreesRoundtrip :
  (d : Double) -> toDegrees (fromDegrees d) = d

||| OWED: `fromRadians` round-trips through `toRadians`. Same blocker
||| as `fromToDegreesRoundtrip`.
public export
0 fromToRadiansRoundtrip :
  (r : Double) -> toRadians (fromRadians r) = r
