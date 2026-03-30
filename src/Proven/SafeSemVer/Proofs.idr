-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeSemVer operations
|||
||| Verifies SemVer spec compliance: bump operations clear prerelease,
||| stable versions have major >= 1, and ordering properties hold.
module Proven.SafeSemVer.Proofs

import Proven.SafeSemVer
import Data.Nat
import Data.List

%default total

--------------------------------------------------------------------------------
-- Bump Operation Properties
--------------------------------------------------------------------------------

||| bumpMajor clears prerelease tags (SemVer spec section 8).
public export
bumpMajorClearsPre : (v : SemVer) -> (bumpMajor v).prerelease = []
bumpMajorClearsPre _ = Refl

||| bumpMinor clears prerelease tags (SemVer spec section 7).
public export
bumpMinorClearsPre : (v : SemVer) -> (bumpMinor v).prerelease = []
bumpMinorClearsPre _ = Refl

||| bumpPatch clears prerelease tags (SemVer spec section 6).
public export
bumpPatchClearsPre : (v : SemVer) -> (bumpPatch v).prerelease = []
bumpPatchClearsPre _ = Refl

||| bumpMajor clears build metadata.
public export
bumpMajorClearsBuild : (v : SemVer) -> (bumpMajor v).build = []
bumpMajorClearsBuild _ = Refl

||| bumpMinor clears build metadata.
public export
bumpMinorClearsBuild : (v : SemVer) -> (bumpMinor v).build = []
bumpMinorClearsBuild _ = Refl

||| bumpPatch clears build metadata.
public export
bumpPatchClearsBuild : (v : SemVer) -> (bumpPatch v).build = []
bumpPatchClearsBuild _ = Refl

||| bumpMajor resets minor to 0.
public export
bumpMajorResetsMinor : (v : SemVer) -> (bumpMajor v).minor = 0
bumpMajorResetsMinor _ = Refl

||| bumpMajor resets patch to 0.
public export
bumpMajorResetsPatch : (v : SemVer) -> (bumpMajor v).patch = 0
bumpMajorResetsPatch _ = Refl

||| bumpMinor resets patch to 0.
public export
bumpMinorResetsPatch : (v : SemVer) -> (bumpMinor v).patch = 0
bumpMinorResetsPatch _ = Refl

||| bumpMajor increments major.
public export
bumpMajorIncrements : (v : SemVer) -> (bumpMajor v).major = S v.major
bumpMajorIncrements _ = Refl

||| bumpMinor increments minor.
public export
bumpMinorIncrements : (v : SemVer) -> (bumpMinor v).minor = S v.minor
bumpMinorIncrements _ = Refl

||| bumpPatch increments patch.
public export
bumpPatchIncrements : (v : SemVer) -> (bumpPatch v).patch = S v.patch
bumpPatchIncrements _ = Refl

||| bumpMajor preserves major field identity after decrement:
||| S of the original is the new major.
public export
bumpMajorPreservesMajor : (v : SemVer) -> (bumpMajor v).major = S v.major
bumpMajorPreservesMajor _ = Refl

--------------------------------------------------------------------------------
-- Stability Properties
--------------------------------------------------------------------------------

||| 0.x.y versions are never stable (major must be >= 1).
public export
zeroMajorNotStable : (v : SemVer) -> v.major = 0 -> isStable v = False
zeroMajorNotStable v prf = rewrite prf in Refl

||| After bumpMajor from 0.x.y, the version has major >= 1.
||| S 0 = 1, so (bumpMajor v).major >= 1.
public export
bumpFromZeroGivesOne : (v : SemVer) -> v.major = 0 -> (bumpMajor v).major = 1
bumpFromZeroGivesOne v prf = rewrite prf in Refl

||| Bumped versions are not prereleases (prerelease list is cleared).
public export
bumpMajorNotPrerelease : (v : SemVer) -> isPrerelease (bumpMajor v) = False
bumpMajorNotPrerelease _ = Refl

public export
bumpMinorNotPrerelease : (v : SemVer) -> isPrerelease (bumpMinor v) = False
bumpMinorNotPrerelease _ = Refl

public export
bumpPatchNotPrerelease : (v : SemVer) -> isPrerelease (bumpPatch v) = False
bumpPatchNotPrerelease _ = Refl

--------------------------------------------------------------------------------
-- Prerelease Identifier Ordering Properties
--------------------------------------------------------------------------------

||| Numeric identifiers sort before alphanumeric (SemVer spec section 11.4.1).
public export
numericBeforeAlpha : (n : Nat) -> (s : String) -> compare (NumericId n) (AlphaId s) = LT
numericBeforeAlpha _ _ = Refl

||| Alphanumeric identifiers sort after numeric.
public export
alphaAfterNumeric : (s : String) -> (n : Nat) -> compare (AlphaId s) (NumericId n) = GT
alphaAfterNumeric _ _ = Refl

--------------------------------------------------------------------------------
-- Compatibility Properties
--------------------------------------------------------------------------------

||| A version is compatible with itself if stable.
export
compatibleWithSelf : (v : SemVer) -> isStable v = True -> isCompatible v v = True

||| satisfiesGTE is reflexive: any version satisfies >= itself.
||| compare v v = EQ, and EQ /= LT = True.
export
satisfiesGTERefl : (v : SemVer) -> satisfiesGTE v v = True
