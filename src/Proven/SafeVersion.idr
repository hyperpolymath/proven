-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeVersion - Safe semantic version parsing and comparison
|||
||| This module provides safe operations for semantic versioning
||| with proper parsing, comparison, and range checking.
module Proven.SafeVersion

import public Proven.Core
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Version Types
--------------------------------------------------------------------------------

||| A semantic version (major.minor.patch with optional prerelease and build)
public export
record SemVer where
  constructor MkSemVer
  major : Nat
  minor : Nat
  patch : Nat
  prerelease : List String
  build : List String

public export
Eq SemVer where
  a == b = a.major == b.major
        && a.minor == b.minor
        && a.patch == b.patch
        && a.prerelease == b.prerelease

||| Compare prerelease identifiers
comparePrerelease : List String -> List String -> Ordering
comparePrerelease [] [] = EQ
comparePrerelease [] _ = GT
comparePrerelease _ [] = LT
comparePrerelease (a :: as) (b :: bs) =
  case (parseNum a, parseNum b) of
    (Just na, Just nb) => case compare na nb of
                            EQ => comparePrerelease as bs
                            o => o
    (Just _, Nothing) => LT
    (Nothing, Just _) => GT
    (Nothing, Nothing) => case compare a b of
                            EQ => comparePrerelease as bs
                            o => o
  where
    parseNum : String -> Maybe Nat
    parseNum s = if all isDigit (unpack s) && s /= ""
                   then Just (cast (parseInteger s))
                   else Nothing

public export
Ord SemVer where
  compare a b =
    case compare a.major b.major of
      EQ => case compare a.minor b.minor of
              EQ => case compare a.patch b.patch of
                      EQ => comparePrerelease a.prerelease b.prerelease
                      o => o
              o => o
      o => o

--------------------------------------------------------------------------------
-- Version Parsing
--------------------------------------------------------------------------------

||| Parse a semantic version string
public export
parse : String -> Maybe SemVer
parse s =
  let (main, bld) = splitBuild s
      (core, pre) = splitPrerelease main
  in do
    (major, minor, patch) <- parseCore core
    Just (MkSemVer major minor patch (parseDotted pre) (parseDotted bld))
  where
    splitBuild : String -> (String, String)
    splitBuild str = case break (== '+') (unpack str) of
      (before, '+' :: after) => (pack before, pack after)
      _ => (str, "")

    splitPrerelease : String -> (String, String)
    splitPrerelease str = case break (== '-') (unpack str) of
      (before, '-' :: after) => (pack before, pack after)
      _ => (str, "")

    parseDotted : String -> List String
    parseDotted "" = []
    parseDotted str = split (== '.') str

    parseCore : String -> Maybe (Nat, Nat, Nat)
    parseCore str =
      case split (== '.') str of
        [maj, min, pat] => do
          major <- parseN maj
          minor <- parseN min
          patch <- parseN pat
          Just (major, minor, patch)
        _ => Nothing

    parseN : String -> Maybe Nat
    parseN str =
      if all isDigit (unpack str) && str /= ""
        then Just (cast (parseInteger str))
        else Nothing

||| Check if a string is a valid semantic version
public export
isValid : String -> Bool
isValid = isJust . parse

--------------------------------------------------------------------------------
-- Version Formatting
--------------------------------------------------------------------------------

||| Format a version to string
public export
format : SemVer -> String
format v =
  let core = show v.major ++ "." ++ show v.minor ++ "." ++ show v.patch
      pre = case v.prerelease of
              [] => ""
              ps => "-" ++ joinBy "." ps
      bld = case v.build of
              [] => ""
              bs => "+" ++ joinBy "." bs
  in core ++ pre ++ bld
  where
    joinBy : String -> List String -> String
    joinBy sep [] = ""
    joinBy sep [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

public export
Show SemVer where
  show = format

--------------------------------------------------------------------------------
-- Version Operations
--------------------------------------------------------------------------------

||| Increment major version (resets minor and patch)
public export
incMajor : SemVer -> SemVer
incMajor v = MkSemVer (S v.major) 0 0 [] []

||| Increment minor version (resets patch)
public export
incMinor : SemVer -> SemVer
incMinor v = MkSemVer v.major (S v.minor) 0 [] []

||| Increment patch version
public export
incPatch : SemVer -> SemVer
incPatch v = MkSemVer v.major v.minor (S v.patch) [] []

||| Set prerelease identifiers
public export
withPrerelease : List String -> SemVer -> SemVer
withPrerelease pre v = MkSemVer v.major v.minor v.patch pre v.build

||| Set build metadata
public export
withBuild : List String -> SemVer -> SemVer
withBuild bld v = MkSemVer v.major v.minor v.patch v.prerelease bld

||| Check if version is stable (no prerelease and major >= 1)
public export
isStable : SemVer -> Bool
isStable v = v.major >= 1 && isNil v.prerelease

||| Check if version is a prerelease
public export
isPrerelease : SemVer -> Bool
isPrerelease v = not (isNil v.prerelease)

--------------------------------------------------------------------------------
-- Version Ranges
--------------------------------------------------------------------------------

||| Check if version satisfies a caret range (^x.y.z)
public export
satisfiesCaret : (range : SemVer) -> (version : SemVer) -> Bool
satisfiesCaret r v =
  if r.major /= 0
    then v.major == r.major && v >= r
    else if r.minor /= 0
      then v.major == 0 && v.minor == r.minor && v >= r
      else v.major == 0 && v.minor == 0 && v.patch == r.patch

||| Check if version satisfies a tilde range (~x.y.z)
public export
satisfiesTilde : (range : SemVer) -> (version : SemVer) -> Bool
satisfiesTilde r v = v.major == r.major && v.minor == r.minor && v.patch >= r.patch

||| Check if version is in range [min, max)
public export
inRange : (min : SemVer) -> (max : SemVer) -> (version : SemVer) -> Bool
inRange mn mx v = v >= mn && v < mx

--------------------------------------------------------------------------------
-- Common Versions
--------------------------------------------------------------------------------

||| Version 0.0.0
public export
zero : SemVer
zero = MkSemVer 0 0 0 [] []

||| Version 1.0.0
public export
one : SemVer
one = MkSemVer 1 0 0 [] []
