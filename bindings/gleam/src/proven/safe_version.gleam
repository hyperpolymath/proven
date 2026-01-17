// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeVersion - Semantic versioning that cannot crash.
////
//// Provides safe version parsing and comparison following SemVer 2.0.0.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/string

/// A validated semantic version.
pub opaque type Version {
  Version(
    major: Int,
    minor: Int,
    patch: Int,
    prerelease: Option(String),
    build: Option(String),
  )
}

/// Error types for version operations.
pub type VersionError {
  InvalidFormat(message: String)
  NegativeVersion(component: String, value: Int)
  EmptyVersion
}

/// Create a version from components.
pub fn version(
  major: Int,
  minor: Int,
  patch: Int,
) -> Result(Version, VersionError) {
  case major < 0 {
    True -> Error(NegativeVersion(component: "major", value: major))
    False ->
      case minor < 0 {
        True -> Error(NegativeVersion(component: "minor", value: minor))
        False ->
          case patch < 0 {
            True -> Error(NegativeVersion(component: "patch", value: patch))
            False ->
              Ok(Version(
                major: major,
                minor: minor,
                patch: patch,
                prerelease: None,
                build: None,
              ))
          }
      }
  }
}

/// Create a version with prerelease tag.
pub fn version_with_prerelease(
  major: Int,
  minor: Int,
  patch: Int,
  prerelease: String,
) -> Result(Version, VersionError) {
  case version(major, minor, patch) {
    Ok(v) -> Ok(Version(..v, prerelease: Some(prerelease)))
    Error(err) -> Error(err)
  }
}

/// Create a version with build metadata.
pub fn version_with_build(
  major: Int,
  minor: Int,
  patch: Int,
  build: String,
) -> Result(Version, VersionError) {
  case version(major, minor, patch) {
    Ok(v) -> Ok(Version(..v, build: Some(build)))
    Error(err) -> Error(err)
  }
}

/// Get the major version number.
pub fn get_major(v: Version) -> Int {
  v.major
}

/// Get the minor version number.
pub fn get_minor(v: Version) -> Int {
  v.minor
}

/// Get the patch version number.
pub fn get_patch(v: Version) -> Int {
  v.patch
}

/// Get the prerelease tag.
pub fn get_prerelease(v: Version) -> Option(String) {
  v.prerelease
}

/// Get the build metadata.
pub fn get_build(v: Version) -> Option(String) {
  v.build
}

/// Parse a semantic version string.
pub fn parse(input: String) -> Result(Version, VersionError) {
  let trimmed = string.trim(input)
  case string.is_empty(trimmed) {
    True -> Error(EmptyVersion)
    False -> parse_version_string(trimmed)
  }
}

fn parse_version_string(input: String) -> Result(Version, VersionError) {
  // Remove leading 'v' or 'V' if present
  let without_v = case string.starts_with(input, "v") || string.starts_with(input, "V") {
    True -> string.drop_start(input, 1)
    False -> input
  }

  // Split off build metadata first (after +)
  let #(without_build, build_meta) = case string.split_once(without_v, "+") {
    Ok(#(before, after)) -> #(before, Some(after))
    Error(_) -> #(without_v, None)
  }

  // Split off prerelease (after -)
  let #(version_part, prerelease_tag) = case string.split_once(without_build, "-") {
    Ok(#(before, after)) -> #(before, Some(after))
    Error(_) -> #(without_build, None)
  }

  // Parse major.minor.patch
  case string.split(version_part, ".") {
    [major_str, minor_str, patch_str] ->
      case int.parse(major_str), int.parse(minor_str), int.parse(patch_str) {
        Ok(major), Ok(minor), Ok(patch) ->
          case major < 0 || minor < 0 || patch < 0 {
            True -> Error(InvalidFormat(message: "Version numbers cannot be negative"))
            False ->
              Ok(Version(
                major: major,
                minor: minor,
                patch: patch,
                prerelease: prerelease_tag,
                build: build_meta,
              ))
          }
        _, _, _ -> Error(InvalidFormat(message: "Version numbers must be integers"))
      }
    [major_str, minor_str] ->
      case int.parse(major_str), int.parse(minor_str) {
        Ok(major), Ok(minor) ->
          Ok(Version(
            major: major,
            minor: minor,
            patch: 0,
            prerelease: prerelease_tag,
            build: build_meta,
          ))
        _, _ -> Error(InvalidFormat(message: "Version numbers must be integers"))
      }
    [major_str] ->
      case int.parse(major_str) {
        Ok(major) ->
          Ok(Version(
            major: major,
            minor: 0,
            patch: 0,
            prerelease: prerelease_tag,
            build: build_meta,
          ))
        Error(_) -> Error(InvalidFormat(message: "Version numbers must be integers"))
      }
    _ -> Error(InvalidFormat(message: "Expected major.minor.patch format"))
  }
}

/// Format a version as a string.
pub fn to_string(v: Version) -> String {
  let base =
    int.to_string(v.major)
    <> "."
    <> int.to_string(v.minor)
    <> "."
    <> int.to_string(v.patch)

  let with_prerelease = case v.prerelease {
    Some(pre) -> base <> "-" <> pre
    None -> base
  }

  case v.build {
    Some(b) -> with_prerelease <> "+" <> b
    None -> with_prerelease
  }
}

/// Compare two versions according to SemVer rules.
/// Build metadata is ignored in comparisons.
pub fn compare(version_a: Version, version_b: Version) -> Order {
  case int.compare(version_a.major, version_b.major) {
    order.Eq ->
      case int.compare(version_a.minor, version_b.minor) {
        order.Eq ->
          case int.compare(version_a.patch, version_b.patch) {
            order.Eq -> compare_prerelease(version_a.prerelease, version_b.prerelease)
            other -> other
          }
        other -> other
      }
    other -> other
  }
}

fn compare_prerelease(pre_a: Option(String), pre_b: Option(String)) -> Order {
  case pre_a, pre_b {
    None, None -> order.Eq
    None, Some(_) -> order.Gt
    // No prerelease is greater
    Some(_), None -> order.Lt
    Some(a), Some(b) -> compare_prerelease_strings(a, b)
  }
}

fn compare_prerelease_strings(pre_a: String, pre_b: String) -> Order {
  let parts_a = string.split(pre_a, ".")
  let parts_b = string.split(pre_b, ".")
  compare_prerelease_parts(parts_a, parts_b)
}

fn compare_prerelease_parts(
  parts_a: List(String),
  parts_b: List(String),
) -> Order {
  case parts_a, parts_b {
    [], [] -> order.Eq
    [], _ -> order.Lt
    _, [] -> order.Gt
    [a, ..rest_a], [b, ..rest_b] ->
      case compare_prerelease_part(a, b) {
        order.Eq -> compare_prerelease_parts(rest_a, rest_b)
        other -> other
      }
  }
}

fn compare_prerelease_part(part_a: String, part_b: String) -> Order {
  case int.parse(part_a), int.parse(part_b) {
    Ok(num_a), Ok(num_b) -> int.compare(num_a, num_b)
    Ok(_), Error(_) -> order.Lt
    // Numeric comes before alphanumeric
    Error(_), Ok(_) -> order.Gt
    Error(_), Error(_) -> string.compare(part_a, part_b)
  }
}

/// Check if version_a is greater than version_b.
pub fn is_greater_than(version_a: Version, version_b: Version) -> Bool {
  compare(version_a, version_b) == order.Gt
}

/// Check if version_a is less than version_b.
pub fn is_less_than(version_a: Version, version_b: Version) -> Bool {
  compare(version_a, version_b) == order.Lt
}

/// Check if two versions are equal (ignoring build metadata).
pub fn equals(version_a: Version, version_b: Version) -> Bool {
  compare(version_a, version_b) == order.Eq
}

/// Check if version satisfies a simple constraint.
/// Supports: "1.2.3", ">1.2.3", ">=1.2.3", "<1.2.3", "<=1.2.3", "^1.2.3", "~1.2.3"
pub fn satisfies(v: Version, constraint: String) -> Bool {
  let trimmed = string.trim(constraint)
  case string.starts_with(trimmed, ">=") {
    True -> satisfies_gte(v, string.drop_start(trimmed, 2))
    False ->
      case string.starts_with(trimmed, "<=") {
        True -> satisfies_lte(v, string.drop_start(trimmed, 2))
        False ->
          case string.starts_with(trimmed, ">") {
            True -> satisfies_gt(v, string.drop_start(trimmed, 1))
            False ->
              case string.starts_with(trimmed, "<") {
                True -> satisfies_lt(v, string.drop_start(trimmed, 1))
                False ->
                  case string.starts_with(trimmed, "^") {
                    True -> satisfies_caret(v, string.drop_start(trimmed, 1))
                    False ->
                      case string.starts_with(trimmed, "~") {
                        True -> satisfies_tilde(v, string.drop_start(trimmed, 1))
                        False -> satisfies_exact(v, trimmed)
                      }
                  }
              }
          }
      }
  }
}

fn satisfies_exact(v: Version, constraint_version: String) -> Bool {
  case parse(constraint_version) {
    Ok(cv) -> equals(v, cv)
    Error(_) -> False
  }
}

fn satisfies_gt(v: Version, constraint_version: String) -> Bool {
  case parse(constraint_version) {
    Ok(cv) -> is_greater_than(v, cv)
    Error(_) -> False
  }
}

fn satisfies_gte(v: Version, constraint_version: String) -> Bool {
  case parse(constraint_version) {
    Ok(cv) -> is_greater_than(v, cv) || equals(v, cv)
    Error(_) -> False
  }
}

fn satisfies_lt(v: Version, constraint_version: String) -> Bool {
  case parse(constraint_version) {
    Ok(cv) -> is_less_than(v, cv)
    Error(_) -> False
  }
}

fn satisfies_lte(v: Version, constraint_version: String) -> Bool {
  case parse(constraint_version) {
    Ok(cv) -> is_less_than(v, cv) || equals(v, cv)
    Error(_) -> False
  }
}

fn satisfies_caret(v: Version, constraint_version: String) -> Bool {
  // ^1.2.3 means >=1.2.3 and <2.0.0 (major must match)
  case parse(constraint_version) {
    Ok(cv) -> v.major == cv.major && { is_greater_than(v, cv) || equals(v, cv) }
    Error(_) -> False
  }
}

fn satisfies_tilde(v: Version, constraint_version: String) -> Bool {
  // ~1.2.3 means >=1.2.3 and <1.3.0 (major and minor must match)
  case parse(constraint_version) {
    Ok(cv) ->
      v.major == cv.major && v.minor == cv.minor && { is_greater_than(v, cv) || equals(v, cv) }
    Error(_) -> False
  }
}

/// Increment the major version (resets minor and patch to 0).
pub fn increment_major(v: Version) -> Version {
  Version(major: v.major + 1, minor: 0, patch: 0, prerelease: None, build: None)
}

/// Increment the minor version (resets patch to 0).
pub fn increment_minor(v: Version) -> Version {
  Version(major: v.major, minor: v.minor + 1, patch: 0, prerelease: None, build: None)
}

/// Increment the patch version.
pub fn increment_patch(v: Version) -> Version {
  Version(
    major: v.major,
    minor: v.minor,
    patch: v.patch + 1,
    prerelease: None,
    build: None,
  )
}

/// Check if a version is a prerelease.
pub fn is_prerelease(v: Version) -> Bool {
  option.is_some(v.prerelease)
}

/// Check if a version is stable (1.0.0 or greater, no prerelease).
pub fn is_stable(v: Version) -> Bool {
  v.major >= 1 && option.is_none(v.prerelease)
}

/// Get the next stable version from a prerelease.
pub fn to_stable(v: Version) -> Version {
  Version(major: v.major, minor: v.minor, patch: v.patch, prerelease: None, build: None)
}
