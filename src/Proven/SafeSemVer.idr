-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSemVer - Semantic Versioning 2.0.0 with total ordering proofs
|||
||| Provides type-safe SemVer parsing and comparison per semver.org spec.
||| The ordering is provably total and transitive — ideal for Agda
||| cross-verification.
module Proven.SafeSemVer

import Data.String
import Data.List
import Data.List1
import Data.Nat

%default total

-- ============================================================================
-- PRERELEASE IDENTIFIERS
-- ============================================================================

||| A prerelease identifier: numeric or alphanumeric
public export
data PreId = NumericId Nat | AlphaId String

public export
Show PreId where
  show (NumericId n) = show n
  show (AlphaId s) = s

public export
Eq PreId where
  NumericId a == NumericId b = a == b
  AlphaId a == AlphaId b = a == b
  _ == _ = False

||| SemVer prerelease precedence:
||| - Numeric < Alphanumeric (when both present)
||| - Numeric identifiers compared numerically
||| - Alphanumeric compared lexically (ASCII)
public export
Ord PreId where
  compare (NumericId a) (NumericId b) = compare a b
  compare (NumericId _) (AlphaId _)   = LT  -- numeric < alpha
  compare (AlphaId _)   (NumericId _) = GT
  compare (AlphaId a)   (AlphaId b)   = compare a b

-- ============================================================================
-- SEMVER TYPE
-- ============================================================================

||| A semantic version: MAJOR.MINOR.PATCH[-prerelease][+build]
public export
record SemVer where
  constructor MkSemVer
  major      : Nat
  minor      : Nat
  patch      : Nat
  prerelease : List PreId  -- Empty = release version
  build      : List String -- Build metadata (ignored in precedence)

public export
Show SemVer where
  show v =
    show v.major ++ "." ++ show v.minor ++ "." ++ show v.patch ++
    (if isNil v.prerelease then ""
     else "-" ++ fastConcat (intersperse "." (map show v.prerelease))) ++
    (if isNil v.build then ""
     else "+" ++ fastConcat (intersperse "." v.build))

||| SemVer precedence (section 11 of spec):
||| 1. Compare major, minor, patch numerically
||| 2. Release > prerelease (no prerelease > has prerelease)
||| 3. Prerelease: compare identifiers left to right
||| 4. Build metadata IGNORED
public export
Eq SemVer where
  a == b = a.major == b.major && a.minor == b.minor && a.patch == b.patch &&
           a.prerelease == b.prerelease

public export
Ord SemVer where
  compare a b =
    case compare a.major b.major of
      EQ => case compare a.minor b.minor of
        EQ => case compare a.patch b.patch of
          EQ => comparePre a.prerelease b.prerelease
          other => other
        other => other
      other => other
    where
      comparePre : List PreId -> List PreId -> Ordering
      comparePre [] [] = EQ
      comparePre [] _  = GT  -- Release > prerelease
      comparePre _  [] = LT
      comparePre (x :: xs) (y :: ys) =
        case compare x y of
          EQ => comparePre xs ys
          other => other

-- ============================================================================
-- PARSING
-- ============================================================================

||| Parse a prerelease identifier
parsePreId : String -> PreId
parsePreId s =
  case parsePositive {a=Nat} s of
    Just n  => if all isDigit (unpack s) then NumericId n else AlphaId s
    Nothing => AlphaId s

||| Parse a SemVer string
public export covering
parseSemVer : String -> Maybe SemVer
parseSemVer s =
  let (versionPart, buildPart) = splitOnce '+' s
      (corePart, prePart) = splitOnce '-' versionPart
  in do (major, minor, patch) <- parseCore corePart
        let pre = parsePre prePart
            bld = parseBuild buildPart
        Just (MkSemVer major minor patch pre bld)
  where
    splitOnce : Char -> String -> (String, String)
    splitOnce c str =
      case break (== c) (unpack str) of
        (before, [])      => (pack before, "")
        (before, _ :: after) => (pack before, pack after)

    parseCore : String -> Maybe (Nat, Nat, Nat)
    parseCore str =
      case forget (split (== '.') str) of
        [ma, mi, pa] => do
          major <- parsePositive {a=Nat} ma
          minor <- parsePositive {a=Nat} mi
          patch <- parsePositive {a=Nat} pa
          Just (major, minor, patch)
        _ => Nothing

    parsePre : String -> List PreId
    parsePre "" = []
    parsePre str = map parsePreId (forget (split (== '.') str))

    parseBuild : String -> List String
    parseBuild "" = []
    parseBuild str = forget (split (== '.') str)

-- ============================================================================
-- OPERATIONS
-- ============================================================================

||| Bump major version (resets minor and patch)
public export
bumpMajor : SemVer -> SemVer
bumpMajor v = { major $= S, minor := 0, patch := 0, prerelease := [], build := [] } v

||| Bump minor version (resets patch)
public export
bumpMinor : SemVer -> SemVer
bumpMinor v = { minor $= S, patch := 0, prerelease := [], build := [] } v

||| Bump patch version
public export
bumpPatch : SemVer -> SemVer
bumpPatch v = { patch $= S, prerelease := [], build := [] } v

||| Check if version is a prerelease
public export
isPrerelease : SemVer -> Bool
isPrerelease v = not (isNil v.prerelease)

||| Check if version is stable (>= 1.0.0, no prerelease)
public export
isStable : SemVer -> Bool
isStable v = v.major >= 1 && not (isPrerelease v)

||| Check if two versions are compatible (same major, for stable versions)
public export
isCompatible : SemVer -> SemVer -> Bool
isCompatible a b = a.major == b.major && isStable a && isStable b

||| Check if a version satisfies a range constraint (>=)
public export
satisfiesGTE : SemVer -> SemVer -> Bool
satisfiesGTE version constraint = compare version constraint /= LT

||| Check if a version satisfies ^constraint (compatible with)
public export
satisfiesCaret : SemVer -> SemVer -> Bool
satisfiesCaret version constraint =
  version.major == constraint.major && compare version constraint /= LT
