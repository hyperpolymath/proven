# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeVersion - Semantic versioning for Nickel
#
# Provides semver parsing, comparison, and validation
# following the Semantic Versioning 2.0.0 specification.

{
  # Result type
  Result = {
    value | Dyn,
    ok | Bool,
  },

  # Semantic version representation
  Version = {
    major | Number,
    minor | Number,
    patch | Number,
    prerelease | optional String,
    build_metadata | optional String,
  },

  # Version requirement/constraint
  VersionReq = {
    operator | String,  # "=", ">", ">=", "<", "<=", "^", "~"
    version | Version,
  },

  # Check if number is a valid version component
  is_valid_component
    : Number -> Bool
    = fun n =>
      n >= 0 && std.number.truncate n == n,

  # Check if string is a valid prerelease identifier
  is_valid_prerelease
    : String -> Bool
    = fun s =>
      if std.string.length s == 0 then
        true
      else
        let chars = std.string.characters s in
        std.array.all
          (fun c =>
            (c >= "0" && c <= "9") ||
            (c >= "a" && c <= "z") ||
            (c >= "A" && c <= "Z") ||
            c == "-" || c == "."
          )
          chars,

  # Check if string is valid build metadata
  is_valid_build_metadata
    : String -> Bool
    = fun s =>
      is_valid_prerelease s,

  # Validate version
  is_valid_version
    : Version -> Bool
    = fun v =>
      is_valid_component v.major &&
      is_valid_component v.minor &&
      is_valid_component v.patch &&
      (v.prerelease == null || is_valid_prerelease v.prerelease) &&
      (v.build_metadata == null || is_valid_build_metadata v.build_metadata),

  # Create version safely
  make_version
    : Number -> Number -> Number -> { value : Version, ok : Bool }
    = fun major minor patch =>
      let v = {
        major = major,
        minor = minor,
        patch = patch,
        prerelease = null,
        build_metadata = null,
      } in
      if is_valid_version v then
        { value = v, ok = true }
      else
        { value = { major = 0, minor = 0, patch = 0, prerelease = null, build_metadata = null }, ok = false },

  # Create version with prerelease
  make_prerelease
    : Number -> Number -> Number -> String -> { value : Version, ok : Bool }
    = fun major minor patch pre =>
      let v = {
        major = major,
        minor = minor,
        patch = patch,
        prerelease = pre,
        build_metadata = null,
      } in
      if is_valid_version v then
        { value = v, ok = true }
      else
        { value = { major = 0, minor = 0, patch = 0, prerelease = null, build_metadata = null }, ok = false },

  # Compare two versions (returns -1, 0, 1)
  compare
    : Version -> Version -> Number
    = fun a b =>
      if a.major != b.major then
        if a.major < b.major then -1 else 1
      else if a.minor != b.minor then
        if a.minor < b.minor then -1 else 1
      else if a.patch != b.patch then
        if a.patch < b.patch then -1 else 1
      else
        # Prerelease comparison: no prerelease > has prerelease
        if a.prerelease == null && b.prerelease != null then 1
        else if a.prerelease != null && b.prerelease == null then -1
        else if a.prerelease != null && b.prerelease != null then
          if a.prerelease < b.prerelease then -1
          else if a.prerelease > b.prerelease then 1
          else 0
        else 0,

  # Check if a > b
  is_greater
    : Version -> Version -> Bool
    = fun a b => compare a b > 0,

  # Check if a >= b
  is_greater_or_equal
    : Version -> Version -> Bool
    = fun a b => compare a b >= 0,

  # Check if a < b
  is_less
    : Version -> Version -> Bool
    = fun a b => compare a b < 0,

  # Check if a <= b
  is_less_or_equal
    : Version -> Version -> Bool
    = fun a b => compare a b <= 0,

  # Check if versions are equal (ignoring build metadata)
  is_equal
    : Version -> Version -> Bool
    = fun a b => compare a b == 0,

  # Increment major version
  bump_major
    : Version -> Version
    = fun v =>
      { major = v.major + 1, minor = 0, patch = 0, prerelease = null, build_metadata = null },

  # Increment minor version
  bump_minor
    : Version -> Version
    = fun v =>
      { major = v.major, minor = v.minor + 1, patch = 0, prerelease = null, build_metadata = null },

  # Increment patch version
  bump_patch
    : Version -> Version
    = fun v =>
      { major = v.major, minor = v.minor, patch = v.patch + 1, prerelease = null, build_metadata = null },

  # Check if version satisfies caret requirement (^1.2.3)
  # ^1.2.3 := >=1.2.3 <2.0.0 (for major > 0)
  # ^0.2.3 := >=0.2.3 <0.3.0 (for major == 0)
  satisfies_caret
    : Version -> Version -> Bool
    = fun version requirement =>
      if requirement.major > 0 then
        version.major == requirement.major &&
        is_greater_or_equal version requirement
      else if requirement.minor > 0 then
        version.major == 0 &&
        version.minor == requirement.minor &&
        is_greater_or_equal version requirement
      else
        version.major == 0 &&
        version.minor == 0 &&
        version.patch >= requirement.patch,

  # Check if version satisfies tilde requirement (~1.2.3)
  # ~1.2.3 := >=1.2.3 <1.3.0
  satisfies_tilde
    : Version -> Version -> Bool
    = fun version requirement =>
      version.major == requirement.major &&
      version.minor == requirement.minor &&
      version.patch >= requirement.patch,

  # Format version as string
  format_version
    : Version -> String
    = fun v =>
      let base = std.string.from_number v.major ++ "." ++
                 std.string.from_number v.minor ++ "." ++
                 std.string.from_number v.patch in
      let with_pre =
        if v.prerelease != null then base ++ "-" ++ v.prerelease else base
      in
      if v.build_metadata != null then with_pre ++ "+" ++ v.build_metadata else with_pre,

  # Contracts
  ValidVersion = std.contract.from_predicate is_valid_version,
  VersionComponent = std.contract.from_predicate is_valid_component,

  # Common version patterns
  ZERO = { major = 0, minor = 0, patch = 0, prerelease = null, build_metadata = null },
  ONE_ZERO_ZERO = { major = 1, minor = 0, patch = 0, prerelease = null, build_metadata = null },
}
