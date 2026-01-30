-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeVersion operations
|||
||| This module exports semantic versioning operations to the C ABI
||| via Idris2's RefC backend. All functions are proven total and follow SemVer 2.0.
|||
||| Return conventions:
||| - Parsing → (Int, major, minor, patch, prerelease, build) where status 0 = success, 1 = error
||| - Comparison → Int (-1 = less, 0 = equal, 1 = greater)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Semantic versioning (SemVer 2.0) rules:
|||           - Breaking changes: increment major, reset minor/patch to 0
|||           - New features (backward compatible): increment minor, reset patch to 0
|||           - Bug fixes (backward compatible): increment patch
|||           - Prerelease: 1.0.0-alpha < 1.0.0
|||           - Build metadata: ignored in precedence (1.0.0+build1 == 1.0.0+build2)
|||
||| Version format: major.minor.patch[-prerelease][+build]
||| Example: 1.2.3-alpha.1+build.456
module Proven.FFI.SafeVersion

import Proven.SafeVersion
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Ordering as Int
encodeOrdering : Ordering -> Int
encodeOrdering LT = (-1)
encodeOrdering EQ = 0
encodeOrdering GT = 1

||| Join list with separator
joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith sep [x] = x
joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

%export
proven_idris_version_parse : String -> (Int, Int, Int, Int, String, String)
proven_idris_version_parse s =
  case parse s of
    Nothing => (1, 0, 0, 0, "", "")  -- Error
    Just v => (0, cast v.major, cast v.minor, cast v.patch,
               joinWith "." v.prerelease, joinWith "." v.build)

%export
proven_idris_version_is_valid : String -> Int
proven_idris_version_is_valid s = encodeBool (isValid s)

%export
proven_idris_version_has_prerelease : String -> Int
proven_idris_version_has_prerelease s =
  encodeBool (isInfixOf "-" s && not (isInfixOf "+" (takeWhile (/= '+') s)))

%export
proven_idris_version_has_build : String -> Int
proven_idris_version_has_build s =
  encodeBool (isInfixOf "+" s)

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

%export
proven_idris_version_format : Int -> Int -> Int -> String -> String -> String
proven_idris_version_format major minor patch prerelease build =
  let pre = if prerelease == "" then [] else split (== '.') prerelease
      bld = if build == "" then [] else split (== '.') build
      v = MkSemVer (cast major) (cast minor) (cast patch) pre bld
  in format v

%export
proven_idris_version_format_core : Int -> Int -> Int -> String
proven_idris_version_format_core major minor patch =
  show major ++ "." ++ show minor ++ "." ++ show patch

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

%export
proven_idris_version_compare : String -> String -> Int
proven_idris_version_compare s1 s2 =
  case (parse s1, parse s2) of
    (Just v1, Just v2) => encodeOrdering (compare v1 v2)
    (Just _, Nothing) => 1   -- Valid > Invalid
    (Nothing, Just _) => (-1)  -- Invalid < Valid
    (Nothing, Nothing) => 0

%export
proven_idris_version_equal : String -> String -> Int
proven_idris_version_equal s1 s2 =
  case (parse s1, parse s2) of
    (Just v1, Just v2) => encodeBool (v1 == v2)
    _ => 0

%export
proven_idris_version_less_than : String -> String -> Int
proven_idris_version_less_than s1 s2 =
  encodeBool (proven_idris_version_compare s1 s2 == (-1))

%export
proven_idris_version_less_than_or_equal : String -> String -> Int
proven_idris_version_less_than_or_equal s1 s2 =
  let cmp = proven_idris_version_compare s1 s2
  in encodeBool (cmp == (-1) || cmp == 0)

%export
proven_idris_version_greater_than : String -> String -> Int
proven_idris_version_greater_than s1 s2 =
  encodeBool (proven_idris_version_compare s1 s2 == 1)

%export
proven_idris_version_greater_than_or_equal : String -> String -> Int
proven_idris_version_greater_than_or_equal s1 s2 =
  let cmp = proven_idris_version_compare s1 s2
  in encodeBool (cmp == 1 || cmp == 0)

--------------------------------------------------------------------------------
-- Version Operations
--------------------------------------------------------------------------------

%export
proven_idris_version_inc_major : String -> String
proven_idris_version_inc_major s =
  case parse s of
    Nothing => s
    Just v => format (incMajor v)

%export
proven_idris_version_inc_minor : String -> String
proven_idris_version_inc_minor s =
  case parse s of
    Nothing => s
    Just v => format (incMinor v)

%export
proven_idris_version_inc_patch : String -> String
proven_idris_version_inc_patch s =
  case parse s of
    Nothing => s
    Just v => format (incPatch v)

%export
proven_idris_version_with_prerelease : String -> String -> String
proven_idris_version_with_prerelease versionStr prerelease =
  case parse versionStr of
    Nothing => versionStr
    Just v =>
      let pre = if prerelease == "" then [] else split (== '.') prerelease
      in format (withPrerelease pre v)

%export
proven_idris_version_with_build : String -> String -> String
proven_idris_version_with_build versionStr build =
  case parse versionStr of
    Nothing => versionStr
    Just v =>
      let bld = if build == "" then [] else split (== '.') build
      in format (withBuild bld v)

%export
proven_idris_version_remove_prerelease : String -> String
proven_idris_version_remove_prerelease s =
  case parse s of
    Nothing => s
    Just v => format (withPrerelease [] v)

%export
proven_idris_version_remove_build : String -> String
proven_idris_version_remove_build s =
  case parse s of
    Nothing => s
    Just v => format (withBuild [] v)

--------------------------------------------------------------------------------
-- Version Classification
--------------------------------------------------------------------------------

%export
proven_idris_version_is_stable : String -> Int
proven_idris_version_is_stable s =
  case parse s of
    Nothing => 0
    Just v => encodeBool (isStable v)

%export
proven_idris_version_is_prerelease : String -> Int
proven_idris_version_is_prerelease s =
  case parse s of
    Nothing => 0
    Just v => encodeBool (isPrerelease v)

%export
proven_idris_version_is_initial_development : String -> Int
proven_idris_version_is_initial_development s =
  case parse s of
    Nothing => 0
    Just v => encodeBool (v.major == 0)

%export
proven_idris_version_is_first_stable : String -> Int
proven_idris_version_is_first_stable s =
  case parse s of
    Nothing => 0
    Just v => encodeBool (v.major == 1 && v.minor == 0 && v.patch == 0 && isNil v.prerelease)

--------------------------------------------------------------------------------
-- Range Checking
--------------------------------------------------------------------------------

%export
proven_idris_version_satisfies_caret : String -> String -> Int
proven_idris_version_satisfies_caret rangeStr versionStr =
  case (parse rangeStr, parse versionStr) of
    (Just r, Just v) => encodeBool (satisfiesCaret r v)
    _ => 0

%export
proven_idris_version_satisfies_tilde : String -> String -> Int
proven_idris_version_satisfies_tilde rangeStr versionStr =
  case (parse rangeStr, parse versionStr) of
    (Just r, Just v) => encodeBool (satisfiesTilde r v)
    _ => 0

%export
proven_idris_version_in_range : String -> String -> String -> Int
proven_idris_version_in_range minStr maxStr versionStr =
  case (parse minStr, parse maxStr, parse versionStr) of
    (Just mn, Just mx, Just v) => encodeBool (inRange mn mx v)
    _ => 0

--------------------------------------------------------------------------------
-- Component Extraction
--------------------------------------------------------------------------------

%export
proven_idris_version_get_major : String -> Int
proven_idris_version_get_major s =
  case parse s of
    Nothing => (-1)
    Just v => cast v.major

%export
proven_idris_version_get_minor : String -> Int
proven_idris_version_get_minor s =
  case parse s of
    Nothing => (-1)
    Just v => cast v.minor

%export
proven_idris_version_get_patch : String -> Int
proven_idris_version_get_patch s =
  case parse s of
    Nothing => (-1)
    Just v => cast v.patch

%export
proven_idris_version_get_prerelease : String -> String
proven_idris_version_get_prerelease s =
  case parse s of
    Nothing => ""
    Just v => joinWith "." v.prerelease

%export
proven_idris_version_get_build : String -> String
proven_idris_version_get_build s =
  case parse s of
    Nothing => ""
    Just v => joinWith "." v.build

--------------------------------------------------------------------------------
-- Common Versions
--------------------------------------------------------------------------------

%export
proven_idris_version_zero : String
proven_idris_version_zero = format zero

%export
proven_idris_version_one : String
proven_idris_version_one = format one

%export
proven_idris_version_is_zero : String -> Int
proven_idris_version_is_zero s =
  case parse s of
    Nothing => 0
    Just v => encodeBool (v == zero)

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

%export
proven_idris_version_is_valid_major : Int -> Int
proven_idris_version_is_valid_major major =
  encodeBool (major >= 0)

%export
proven_idris_version_is_valid_minor : Int -> Int
proven_idris_version_is_valid_minor minor =
  encodeBool (minor >= 0)

%export
proven_idris_version_is_valid_patch : Int -> Int
proven_idris_version_is_valid_patch patch =
  encodeBool (patch >= 0)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_version_friendly_error : String -> String
proven_idris_version_friendly_error errorMsg =
  if isInfixOf "parse" (toLower errorMsg) || isInfixOf "invalid" (toLower errorMsg)
    then "Invalid semantic version format (expected major.minor.patch[-prerelease][+build])"
  else if isInfixOf "major" (toLower errorMsg)
    then "Invalid major version (must be non-negative integer)"
  else if isInfixOf "minor" (toLower errorMsg)
    then "Invalid minor version (must be non-negative integer)"
  else if isInfixOf "patch" (toLower errorMsg)
    then "Invalid patch version (must be non-negative integer)"
  else if isInfixOf "prerelease" (toLower errorMsg)
    then "Invalid prerelease identifier"
  else if isInfixOf "build" (toLower errorMsg)
    then "Invalid build metadata"
  else if isInfixOf "range" (toLower errorMsg)
    then "Version does not satisfy range constraint"
  else
    "Semantic version error"
