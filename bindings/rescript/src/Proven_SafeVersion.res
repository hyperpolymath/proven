// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeVersion - Safe semantic versioning operations that cannot crash.
 *
 * Implements SemVer 2.0.0 specification with safe parsing and comparison.
 */

/** Semantic version representation */
type version = {
  major: int,
  minor: int,
  patch: int,
  prerelease: option<string>,
  buildMetadata: option<string>,
}

/** Version constraint operators */
type constraintOp =
  | Eq // =
  | Gt // >
  | Gte // >=
  | Lt // <
  | Lte // <=
  | Tilde // ~
  | Caret // ^

/** Version constraint */
type versionConstraint = {
  op: constraintOp,
  version: version,
}

/** Create a new version */
let make = (~major: int, ~minor: int, ~patch: int): option<version> => {
  if major < 0 || minor < 0 || patch < 0 {
    None
  } else {
    Some({major, minor, patch, prerelease: None, buildMetadata: None})
  }
}

/** Create a version with prerelease */
let makeWithPrerelease = (
  ~major: int,
  ~minor: int,
  ~patch: int,
  ~prerelease: string,
): option<version> => {
  if major < 0 || minor < 0 || patch < 0 {
    None
  } else {
    Some({major, minor, patch, prerelease: Some(prerelease), buildMetadata: None})
  }
}

/** Create a full version with build metadata */
let makeFull = (
  ~major: int,
  ~minor: int,
  ~patch: int,
  ~prerelease: option<string>,
  ~buildMetadata: option<string>,
): option<version> => {
  if major < 0 || minor < 0 || patch < 0 {
    None
  } else {
    Some({major, minor, patch, prerelease, buildMetadata})
  }
}

/** Parse a version string */
let parse = (versionString: string): option<version> => {
  let str = Js.String2.trim(versionString)
  let str = if Js.String2.startsWith(str, "v") {
    Js.String2.sliceToEnd(str, ~from=1)
  } else {
    str
  }

  // Split off build metadata
  let (str, buildMetadata) = switch Js.String2.indexOf(str, "+") {
  | -1 => (str, None)
  | idx => (
      Js.String2.slice(str, ~from=0, ~to_=idx),
      Some(Js.String2.sliceToEnd(str, ~from=idx + 1)),
    )
  }

  // Split off prerelease
  let (str, prerelease) = switch Js.String2.indexOf(str, "-") {
  | -1 => (str, None)
  | idx => (
      Js.String2.slice(str, ~from=0, ~to_=idx),
      Some(Js.String2.sliceToEnd(str, ~from=idx + 1)),
    )
  }

  // Parse major.minor.patch
  let parts = Js.String2.split(str, ".")
  if Belt.Array.length(parts) != 3 {
    None
  } else {
    let major = Belt.Int.fromString(Belt.Array.getUnsafe(parts, 0))
    let minor = Belt.Int.fromString(Belt.Array.getUnsafe(parts, 1))
    let patch = Belt.Int.fromString(Belt.Array.getUnsafe(parts, 2))

    switch (major, minor, patch) {
    | (Some(maj), Some(min), Some(pat)) =>
      if maj < 0 || min < 0 || pat < 0 {
        None
      } else {
        Some({major: maj, minor: min, patch: pat, prerelease, buildMetadata})
      }
    | _ => None
    }
  }
}

/** Convert version to string */
let toString = (v: version): string => {
  let base = `${Belt.Int.toString(v.major)}.${Belt.Int.toString(v.minor)}.${Belt.Int.toString(
      v.patch,
    )}`
  let withPre = switch v.prerelease {
  | Some(pre) => base ++ "-" ++ pre
  | None => base
  }
  switch v.buildMetadata {
  | Some(build) => withPre ++ "+" ++ build
  | None => withPre
  }
}

/** Compare prerelease identifiers */
let comparePrerelease = (a: option<string>, b: option<string>): int => {
  switch (a, b) {
  | (None, None) => 0
  | (Some(_), None) => -1 // Prerelease < release
  | (None, Some(_)) => 1
  | (Some(preA), Some(preB)) => Js.String2.localeCompare(preA, preB)->Belt.Float.toInt
  }
}

/** Compare two versions: -1 if a < b, 0 if equal, 1 if a > b */
let compare = (a: version, b: version): int => {
  if a.major != b.major {
    if a.major < b.major {
      -1
    } else {
      1
    }
  } else if a.minor != b.minor {
    if a.minor < b.minor {
      -1
    } else {
      1
    }
  } else if a.patch != b.patch {
    if a.patch < b.patch {
      -1
    } else {
      1
    }
  } else {
    comparePrerelease(a.prerelease, b.prerelease)
  }
}

/** Check if a < b */
let lt = (a: version, b: version): bool => compare(a, b) < 0

/** Check if a <= b */
let lte = (a: version, b: version): bool => compare(a, b) <= 0

/** Check if a > b */
let gt = (a: version, b: version): bool => compare(a, b) > 0

/** Check if a >= b */
let gte = (a: version, b: version): bool => compare(a, b) >= 0

/** Check if two versions are equal (ignoring build metadata per SemVer spec) */
let eq = (a: version, b: version): bool => compare(a, b) == 0

/** Increment major version */
let incMajor = (v: version): version => {
  {major: v.major + 1, minor: 0, patch: 0, prerelease: None, buildMetadata: None}
}

/** Increment minor version */
let incMinor = (v: version): version => {
  {major: v.major, minor: v.minor + 1, patch: 0, prerelease: None, buildMetadata: None}
}

/** Increment patch version */
let incPatch = (v: version): version => {
  {major: v.major, minor: v.minor, patch: v.patch + 1, prerelease: None, buildMetadata: None}
}

/** Check if version satisfies constraint */
let satisfies = (v: version, versionConstraint: versionConstraint): bool => {
  switch versionConstraint.op {
  | Eq => eq(v, versionConstraint.version)
  | Gt => gt(v, versionConstraint.version)
  | Gte => gte(v, versionConstraint.version)
  | Lt => lt(v, versionConstraint.version)
  | Lte => lte(v, versionConstraint.version)
  | Tilde =>
    // ~1.2.3 := >=1.2.3 <1.3.0
    gte(v, versionConstraint.version) &&
    lt(v, {major: versionConstraint.version.major, minor: versionConstraint.version.minor + 1, patch: 0, prerelease: None, buildMetadata: None})
  | Caret =>
    // ^1.2.3 := >=1.2.3 <2.0.0 (for major > 0)
    // ^0.2.3 := >=0.2.3 <0.3.0 (for major = 0, minor > 0)
    // ^0.0.3 := >=0.0.3 <0.0.4 (for major = 0, minor = 0)
    if versionConstraint.version.major > 0 {
      gte(v, versionConstraint.version) &&
      lt(v, {major: versionConstraint.version.major + 1, minor: 0, patch: 0, prerelease: None, buildMetadata: None})
    } else if versionConstraint.version.minor > 0 {
      gte(v, versionConstraint.version) &&
      lt(v, {major: 0, minor: versionConstraint.version.minor + 1, patch: 0, prerelease: None, buildMetadata: None})
    } else {
      gte(v, versionConstraint.version) &&
      lt(v, {major: 0, minor: 0, patch: versionConstraint.version.patch + 1, prerelease: None, buildMetadata: None})
    }
  }
}

/** Check if version is stable (no prerelease) */
let isStable = (v: version): bool => {
  switch v.prerelease {
  | Some(_) => false
  | None => true
  }
}

/** Check if version is prerelease */
let isPrerelease = (v: version): bool => {
  switch v.prerelease {
  | Some(_) => true
  | None => false
  }
}

/** Get the maximum version from an array */
let max = (versions: array<version>): option<version> => {
  if Belt.Array.length(versions) == 0 {
    None
  } else {
    Some(Belt.Array.reduce(versions, Belt.Array.getUnsafe(versions, 0), (acc, v) => {
      if gt(v, acc) {
        v
      } else {
        acc
      }
    }))
  }
}

/** Get the minimum version from an array */
let min = (versions: array<version>): option<version> => {
  if Belt.Array.length(versions) == 0 {
    None
  } else {
    Some(Belt.Array.reduce(versions, Belt.Array.getUnsafe(versions, 0), (acc, v) => {
      if lt(v, acc) {
        v
      } else {
        acc
      }
    }))
  }
}

/** Sort versions in ascending order */
let sort = (versions: array<version>): array<version> => {
  let copy = Belt.Array.copy(versions)
  Belt.SortArray.stableSortInPlaceBy(copy, compare)
  copy
}
