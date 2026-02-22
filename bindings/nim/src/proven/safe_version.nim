# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe semantic versioning operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  NimSemanticVersion* = object
    ## A semantic version (major.minor.patch[-prerelease]).
    major*: uint32
    minor*: uint32
    patch*: uint32
    prerelease*: string

proc parseVersion*(s: string): Option[NimSemanticVersion] =
  ## Parse a version string (e.g., "1.2.3", "1.2.3-alpha").
  ## Returns None if the string is not valid semver.
  if s.len == 0:
    return none(NimSemanticVersion)
  var res = provenVersionParse(unsafeAddr s[0], csize_t(s.len))
  if res.status != PROVEN_OK:
    return none(NimSemanticVersion)

  # Extract all fields before freeing
  let major = res.version.major
  let minor = res.version.minor
  let patch = res.version.patch
  let pre = if res.version.prerelease != nil and res.version.prerelease_len > 0:
    $res.version.prerelease
  else: ""

  provenVersionFree(addr res.version)

  some(NimSemanticVersion(
    major: major,
    minor: minor,
    patch: patch,
    prerelease: pre
  ))

proc compare*(a, b: NimSemanticVersion): int =
  ## Compare two versions. Returns negative, 0, or positive.
  let sa = SemanticVersion(
    major: a.major,
    minor: a.minor,
    patch: a.patch,
    prerelease_len: csize_t(a.prerelease.len),
    prerelease: if a.prerelease.len > 0: cstring(a.prerelease) else: nil
  )
  let sb = SemanticVersion(
    major: b.major,
    minor: b.minor,
    patch: b.patch,
    prerelease_len: csize_t(b.prerelease.len),
    prerelease: if b.prerelease.len > 0: cstring(b.prerelease) else: nil
  )
  int(provenVersionCompare(sa, sb))

proc `$`*(v: NimSemanticVersion): string =
  ## String representation of version.
  result = $v.major & "." & $v.minor & "." & $v.patch
  if v.prerelease.len > 0:
    result.add("-" & v.prerelease)

proc `<`*(a, b: NimSemanticVersion): bool =
  ## Compare less than.
  compare(a, b) < 0

proc `<=`*(a, b: NimSemanticVersion): bool =
  ## Compare less than or equal.
  compare(a, b) <= 0

proc `>`*(a, b: NimSemanticVersion): bool =
  ## Compare greater than.
  compare(a, b) > 0

proc `>=`*(a, b: NimSemanticVersion): bool =
  ## Compare greater than or equal.
  compare(a, b) >= 0

proc `==`*(a, b: NimSemanticVersion): bool =
  ## Check equality.
  compare(a, b) == 0

proc isStable*(v: NimSemanticVersion): bool =
  ## Check if version is stable (major >= 1, no prerelease).
  v.major >= 1 and v.prerelease.len == 0

proc isPrerelease*(v: NimSemanticVersion): bool =
  ## Check if version is a prerelease.
  v.prerelease.len > 0
