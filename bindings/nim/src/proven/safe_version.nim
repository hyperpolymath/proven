# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe semantic versioning operations.

import std/[options, strutils]

type
  SemanticVersion* = object
    ## A semantic version (major.minor.patch).
    major*: int
    minor*: int
    patch*: int
    prerelease*: string
    build*: string

proc newVersion*(major, minor, patch: int): Option[SemanticVersion] =
  ## Create a new semantic version.
  if major < 0 or minor < 0 or patch < 0:
    return none(SemanticVersion)
  result = some(SemanticVersion(
    major: major,
    minor: minor,
    patch: patch,
    prerelease: "",
    build: ""
  ))

proc parseVersion*(s: string): Option[SemanticVersion] =
  ## Parse a version string (e.g., "1.2.3", "1.2.3-alpha", "1.2.3-alpha+build").
  if s.len == 0 or s.len > 256:
    return none(SemanticVersion)

  var version = s
  var build = ""
  var prerelease = ""

  # Extract build metadata
  let buildIdx = version.find('+')
  if buildIdx >= 0:
    build = version[buildIdx + 1 ..< version.len]
    version = version[0 ..< buildIdx]

  # Extract prerelease
  let prereleaseIdx = version.find('-')
  if prereleaseIdx >= 0:
    prerelease = version[prereleaseIdx + 1 ..< version.len]
    version = version[0 ..< prereleaseIdx]

  # Parse major.minor.patch
  let parts = version.split('.')
  if parts.len < 1 or parts.len > 3:
    return none(SemanticVersion)

  try:
    let major = parseInt(parts[0])
    let minor = if parts.len > 1: parseInt(parts[1]) else: 0
    let patch = if parts.len > 2: parseInt(parts[2]) else: 0

    if major < 0 or minor < 0 or patch < 0:
      return none(SemanticVersion)

    result = some(SemanticVersion(
      major: major,
      minor: minor,
      patch: patch,
      prerelease: prerelease,
      build: build
    ))
  except:
    return none(SemanticVersion)

proc toString*(v: SemanticVersion): string =
  ## Convert a version to string.
  result = $v.major & "." & $v.minor & "." & $v.patch
  if v.prerelease.len > 0:
    result.add("-" & v.prerelease)
  if v.build.len > 0:
    result.add("+" & v.build)

proc `$`*(v: SemanticVersion): string =
  ## String representation of version.
  toString(v)

proc compare*(a, b: SemanticVersion): int =
  ## Compare two versions. Returns -1, 0, or 1.
  if a.major != b.major:
    return if a.major < b.major: -1 else: 1
  if a.minor != b.minor:
    return if a.minor < b.minor: -1 else: 1
  if a.patch != b.patch:
    return if a.patch < b.patch: -1 else: 1

  # Prerelease comparison
  if a.prerelease.len == 0 and b.prerelease.len > 0:
    return 1  # No prerelease > prerelease
  if a.prerelease.len > 0 and b.prerelease.len == 0:
    return -1
  if a.prerelease != b.prerelease:
    return if a.prerelease < b.prerelease: -1 else: 1

  return 0

proc `<`*(a, b: SemanticVersion): bool =
  compare(a, b) < 0

proc `<=`*(a, b: SemanticVersion): bool =
  compare(a, b) <= 0

proc `>`*(a, b: SemanticVersion): bool =
  compare(a, b) > 0

proc `>=`*(a, b: SemanticVersion): bool =
  compare(a, b) >= 0

proc `==`*(a, b: SemanticVersion): bool =
  compare(a, b) == 0

proc bumpMajor*(v: SemanticVersion): SemanticVersion =
  ## Bump major version (resets minor and patch).
  SemanticVersion(
    major: v.major + 1,
    minor: 0,
    patch: 0,
    prerelease: "",
    build: ""
  )

proc bumpMinor*(v: SemanticVersion): SemanticVersion =
  ## Bump minor version (resets patch).
  SemanticVersion(
    major: v.major,
    minor: v.minor + 1,
    patch: 0,
    prerelease: "",
    build: ""
  )

proc bumpPatch*(v: SemanticVersion): SemanticVersion =
  ## Bump patch version.
  SemanticVersion(
    major: v.major,
    minor: v.minor,
    patch: v.patch + 1,
    prerelease: "",
    build: ""
  )

proc isStable*(v: SemanticVersion): bool =
  ## Check if version is stable (major >= 1, no prerelease).
  v.major >= 1 and v.prerelease.len == 0

proc isPrerelease*(v: SemanticVersion): bool =
  ## Check if version is a prerelease.
  v.prerelease.len > 0

proc isCompatible*(a, b: SemanticVersion): bool =
  ## Check if two versions are compatible (same major, a >= b).
  a.major == b.major and a >= b
