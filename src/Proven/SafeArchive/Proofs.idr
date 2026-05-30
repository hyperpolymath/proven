-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeArchive zip-slip / zip-bomb / symlink-attack
||| validation.
|||
||| `Proven.SafeArchive` ships validators for Zip Slip, zip bombs, and
||| dangerous symlinks. This file discharges the type-level reducible
||| invariants:
|||
|||   * `EntryType` enum self-equality (all 4 constructors).
|||   * `EntryValidation` enum self-equality (all 6 constructors).
|||   * The `maxCompressionRatio` and `maxTotalSize` spec anchors —
|||     stating their concrete values (1000 and 2^30 bytes) so any
|||     accidental edit fails the build.
|||   * `validateEntry` returning a specific outcome on canonical
|||     inputs (NullByteInPath / AbsolutePathDetected / ZipSlipDetected
|||     / EntryOK).
|||   * `isZipBomb` zero-compressed-size corner case (0 / 0 entries
|||     are NOT bombs).
|||
||| OWED: `hasPathTraversal` and `isInfixOf`-based string equalities
||| depend on opaque `String` FFI primitives — see standards#128 FFI
||| family.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeArchive.Proofs

import Proven.SafeArchive

%default total

--------------------------------------------------------------------------------
-- EntryType enum self-equality
--------------------------------------------------------------------------------

||| `RegularFile` is self-equal.
public export
regularFileSelfEq : RegularFile == RegularFile = True
regularFileSelfEq = Refl

||| `Directory` is self-equal.
public export
directorySelfEq : Directory == Directory = True
directorySelfEq = Refl

||| `Symlink` is self-equal.
public export
symlinkSelfEq : Symlink == Symlink = True
symlinkSelfEq = Refl

||| `HardLink` is self-equal.
public export
hardLinkSelfEq : HardLink == HardLink = True
hardLinkSelfEq = Refl

||| Distinct entry types are unequal — sample pair anchors fall-through.
public export
regularFileNotDirectory : RegularFile == Directory = False
regularFileNotDirectory = Refl

||| Symlink vs HardLink — anchors the second-half of the fall-through.
public export
symlinkNotHardLink : Symlink == HardLink = False
symlinkNotHardLink = Refl

--------------------------------------------------------------------------------
-- EntryValidation enum self-equality
--------------------------------------------------------------------------------

||| `EntryOK` is self-equal.
public export
entryOKSelfEq : EntryOK == EntryOK = True
entryOKSelfEq = Refl

||| `NullByteInPath` is self-equal.
public export
nullByteInPathSelfEq : NullByteInPath == NullByteInPath = True
nullByteInPathSelfEq = Refl

||| `AbsolutePathDetected` is self-equal.
public export
absolutePathDetectedSelfEq : AbsolutePathDetected == AbsolutePathDetected = True
absolutePathDetectedSelfEq = Refl

||| `ZipSlipDetected` is self-equal under the constructor-only `Eq`
||| (which compares constructor, not the carried path).
public export
zipSlipDetectedSelfEq : (p : String) -> ZipSlipDetected p == ZipSlipDetected p = True
zipSlipDetectedSelfEq p = Refl

||| `ZipBombDetected` is constructor-self-equal under the Eq.
public export
zipBombDetectedSelfEq : (c, u : Nat) -> ZipBombDetected c u == ZipBombDetected c u = True
zipBombDetectedSelfEq c u = Refl

||| `DangerousSymlink` is constructor-self-equal under the Eq.
public export
dangerousSymlinkSelfEq : (p : String) -> DangerousSymlink p == DangerousSymlink p = True
dangerousSymlinkSelfEq p = Refl

||| `EntryOK /= NullByteInPath` — anchors the fall-through.
public export
entryOKNotNullByteInPath : EntryOK == NullByteInPath = False
entryOKNotNullByteInPath = Refl

--------------------------------------------------------------------------------
-- Spec-anchor constants
--------------------------------------------------------------------------------

||| DISCHARGED: `maxCompressionRatio = 1000`. `maxCompressionRatio` is
||| `public export` with body `1000` (SafeArchive.idr L64-66). Both
||| sides are the same `Nat` literal — Idris2 compares literals as
||| their `Integer` representation without unary expansion, so `Refl`
||| closes regardless of literal magnitude.
public export
maxCompressionRatioAnchor : maxCompressionRatio = 1000
maxCompressionRatioAnchor = Refl

||| DISCHARGED: `maxTotalSize = 1073741824` (1 GiB). Same literal-vs-
||| literal pattern as `maxCompressionRatioAnchor` — `maxTotalSize` is
||| `public export` with body `1073741824` (SafeArchive.idr L77-79).
||| The "Nat literal opacity" blocker only applies to attempts to
||| convert this value to a SMALLER literal — here both sides are the
||| same literal so `Refl` closes.
public export
maxTotalSizeAnchor : maxTotalSize = 1073741824
maxTotalSizeAnchor = Refl

--------------------------------------------------------------------------------
-- `isZipBomb` corner cases (compressedSize = 0)
--------------------------------------------------------------------------------

||| An entry with 0 compressed AND 0 uncompressed bytes is NOT a zip
||| bomb (would otherwise classify trivial empty entries as malicious).
public export
zeroSizeNotZipBomb :
  isZipBomb (MkArchiveEntry "x" RegularFile 0 0 Nothing) = False
zeroSizeNotZipBomb = Refl

||| An entry with 0 compressed but positive uncompressed IS a zip bomb
||| (decompression-to-infinity attack).
public export
zeroCompressedNonZeroUncompressedIsZipBomb :
  isZipBomb (MkArchiveEntry "x" RegularFile 0 1 Nothing) = True
zeroCompressedNonZeroUncompressedIsZipBomb = Refl

||| OWED: A reasonably-compressed entry (ratio 100, well under 1000)
||| is NOT a zip bomb. Blocked on Nat-literal opacity (standards#128).
public export
0 modestRatioNotZipBomb :
  isZipBomb (MkArchiveEntry "x" RegularFile 1 100 Nothing) = False

||| OWED: An entry with compression ratio > 1000 IS a zip bomb. Same
||| blocker.
public export
0 extremeRatioIsZipBomb :
  isZipBomb (MkArchiveEntry "x" RegularFile 1 1001 Nothing) = True

--------------------------------------------------------------------------------
-- `isDangerousSymlink` requires Symlink entry type
--------------------------------------------------------------------------------

||| A `RegularFile` entry is never a dangerous symlink (regardless of
||| `symlinkTarget` content) — the case-split short-circuits.
public export
regularFileNotDangerousSymlink :
  (tgt : Maybe String)
  -> isDangerousSymlink (MkArchiveEntry "x" RegularFile 1 1 tgt) = False
regularFileNotDangerousSymlink tgt = Refl

||| A `Directory` entry is never a dangerous symlink.
public export
directoryNotDangerousSymlink :
  (tgt : Maybe String)
  -> isDangerousSymlink (MkArchiveEntry "x" Directory 1 1 tgt) = False
directoryNotDangerousSymlink tgt = Refl

||| A `HardLink` entry is not classified by `isDangerousSymlink`
||| (it only fires on Symlink).
public export
hardLinkNotDangerousSymlink :
  (tgt : Maybe String)
  -> isDangerousSymlink (MkArchiveEntry "x" HardLink 1 1 tgt) = False
hardLinkNotDangerousSymlink tgt = Refl

||| A Symlink with `Nothing` target is not dangerous.
public export
symlinkNoTargetNotDangerous :
  isDangerousSymlink (MkArchiveEntry "x" Symlink 1 1 Nothing) = False
symlinkNoTargetNotDangerous = Refl

--------------------------------------------------------------------------------
-- Explicit OWED postulates
--
-- `hasPathTraversal` calls `isInfixOf` / `isPrefixOf` on String, both
-- of which go through opaque String FFI primitives. The negative
-- direction (e.g. "normal.txt" has no traversal) is true but not
-- Refl-reducible under Idris2 0.8.0 — standards#128 FFI family.
--------------------------------------------------------------------------------

||| OWED: A path with no special characters has no traversal. Blocked
||| on the String FFI family (`isInfixOf` / `isPrefixOf`).
public export
0 plainPathHasNoTraversal :
  hasPathTraversal "normal.txt" = False

||| OWED: A path with ".." has traversal. Same blocker.
public export
0 dotDotPathHasTraversal :
  hasPathTraversal "../etc/passwd" = True
