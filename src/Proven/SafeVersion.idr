-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeVersion - Verified semantic versioning
|||
||| Type-safe semantic version parsing, comparison, and manipulation.
||| Follows SemVer 2.0.0 specification.
|||
||| Guarantees version components are non-negative and comparisons are correct.
module Proven.SafeVersion

import Proven.Core
import Data.So
import Data.List
import Data.String

%default total

-- ============================================================================
-- SEMANTIC VERSION
-- ============================================================================

||| Pre-release identifier (alphanumeric or numeric)
public export
data PreReleaseId : Type where
  ||| Numeric identifier (compared numerically)
  Numeric : Nat -> PreReleaseId
  ||| Alphanumeric identifier (compared lexically)
  Alphanumeric : String -> PreReleaseId

||| Compare pre-release identifiers per SemVer spec
comparePreReleaseId : PreReleaseId -> PreReleaseId -> Ordering
comparePreReleaseId (Numeric n1) (Numeric n2) = compare n1 n2
comparePreReleaseId (Alphanumeric s1) (Alphanumeric s2) = compare s1 s2
comparePreReleaseId (Numeric _) (Alphanumeric _) = LT  -- Numeric has lower precedence
comparePreReleaseId (Alphanumeric _) (Numeric _) = GT

export
Eq PreReleaseId where
  (Numeric n1) == (Numeric n2) = n1 == n2
  (Alphanumeric s1) == (Alphanumeric s2) = s1 == s2
  _ == _ = False

export
Ord PreReleaseId where
  compare = comparePreReleaseId

||| Semantic version (SemVer 2.0.0)
public export
record SemVer where
  constructor MkSemVer
  major : Nat
  minor : Nat
  patch : Nat
  prerelease : List PreReleaseId
  buildMetadata : Maybe String

||| Create a simple version (no prerelease or build metadata)
export
semver : Nat -> Nat -> Nat -> SemVer
semver maj min pat = MkSemVer maj min pat [] Nothing

||| Create version with prerelease
export
semverPre : Nat -> Nat -> Nat -> List PreReleaseId -> SemVer
semverPre maj min pat pre = MkSemVer maj min pat pre Nothing

||| Create full version
export
semverFull : Nat -> Nat -> Nat -> List PreReleaseId -> Maybe String -> SemVer
semverFull = MkSemVer

-- ============================================================================
-- PARSING
-- ============================================================================

||| Check if string is all digits
isAllDigits : String -> Bool
isAllDigits s = all isDigit (unpack s)

||| Parse a natural number from string
parseNat : String -> Maybe Nat
parseNat s =
  if null s || not (isAllDigits s) then Nothing
  else if length s > 1 && strIndex s 0 == Just '0' then Nothing  -- No leading zeros
  else Just (cast (parseInteger s))
  where
    parseInteger : String -> Integer
    parseInteger = foldl (\acc, c => acc * 10 + cast (ord c - ord '0')) 0 . unpack

||| Parse a pre-release identifier
parsePreReleaseId : String -> Maybe PreReleaseId
parsePreReleaseId s =
  if null s then Nothing
  else if isAllDigits s
       then if length s > 1 && strIndex s 0 == Just '0'
            then Nothing  -- No leading zeros in numeric
            else Just (Numeric (cast (foldl (\acc, c => acc * 10 + cast (ord c - ord '0')) 0 (unpack s))))
       else if all isAlphanumericOrHyphen (unpack s)
            then Just (Alphanumeric s)
            else Nothing
  where
    isAlphanumericOrHyphen : Char -> Bool
    isAlphanumericOrHyphen c = isAlphaNum c || c == '-'

||| Split string by character
splitBy : Char -> String -> List String
splitBy c s = go (unpack s) [] []
  where
    go : List Char -> List Char -> List String -> List String
    go [] acc result = reverse (pack (reverse acc) :: result)
    go (x :: xs) acc result =
      if x == c then go xs [] (pack (reverse acc) :: result)
      else go xs (x :: acc) result

||| Parse pre-release identifiers
parsePrerelease : String -> Maybe (List PreReleaseId)
parsePrerelease s =
  if null s then Just []
  else traverse parsePreReleaseId (splitBy '.' s)

||| Validate build metadata (alphanumeric + dots + hyphens)
validateBuildMetadata : String -> Bool
validateBuildMetadata s = all isValidBuildChar (unpack s)
  where
    isValidBuildChar : Char -> Bool
    isValidBuildChar c = isAlphaNum c || c == '.' || c == '-'

||| Parse a semantic version string
||| Format: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
export
parseSemVer : String -> Maybe SemVer
parseSemVer s =
  -- Split off build metadata
  let (versionPart, buildMeta) = splitBuild s
      -- Split off prerelease
      (corePart, prereleasePart) = splitPrerelease versionPart
      -- Parse core version
      parts = splitBy '.' corePart
  in case parts of
       [majS, minS, patS] =>
         case (parseNat majS, parseNat minS, parseNat patS) of
           (Just maj, Just min, Just pat) =>
             case parsePrerelease prereleasePart of
               Just pre => Just (MkSemVer maj min pat pre buildMeta)
               Nothing => Nothing
           _ => Nothing
       _ => Nothing
  where
    splitBuild : String -> (String, Maybe String)
    splitBuild str =
      case break (== '+') (unpack str) of
        (v, []) => (pack v, Nothing)
        (v, _ :: b) => if validateBuildMetadata (pack b)
                       then (pack v, Just (pack b))
                       else (pack v, Nothing)

    splitPrerelease : String -> (String, String)
    splitPrerelease str =
      case break (== '-') (unpack str) of
        (v, []) => (pack v, "")
        (v, _ :: p) => (pack v, pack p)

-- ============================================================================
-- FORMATTING
-- ============================================================================

||| Format pre-release identifier
formatPreReleaseId : PreReleaseId -> String
formatPreReleaseId (Numeric n) = show n
formatPreReleaseId (Alphanumeric s) = s

||| Format semantic version to string
export
formatSemVer : SemVer -> String
formatSemVer v =
  let core = show v.major ++ "." ++ show v.minor ++ "." ++ show v.patch
      pre = case v.prerelease of
              [] => ""
              ids => "-" ++ joinBy "." (map formatPreReleaseId ids)
      build = case v.buildMetadata of
                Nothing => ""
                Just b => "+" ++ b
  in core ++ pre ++ build
  where
    joinBy : String -> List String -> String
    joinBy _ [] = ""
    joinBy _ [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

export
Show SemVer where
  show = formatSemVer

-- ============================================================================
-- COMPARISON
-- ============================================================================

||| Compare pre-release lists per SemVer spec
comparePrereleases : List PreReleaseId -> List PreReleaseId -> Ordering
comparePrereleases [] [] = EQ
comparePrereleases [] _ = GT  -- No prerelease > prerelease
comparePrereleases _ [] = LT  -- Prerelease < no prerelease
comparePrereleases (x :: xs) (y :: ys) =
  case compare x y of
    EQ => comparePrereleases xs ys
    other => other

||| Compare two semantic versions (ignores build metadata per SemVer spec)
export
compareSemVer : SemVer -> SemVer -> Ordering
compareSemVer v1 v2 =
  case compare v1.major v2.major of
    EQ => case compare v1.minor v2.minor of
            EQ => case compare v1.patch v2.patch of
                    EQ => comparePrereleases v1.prerelease v2.prerelease
                    other => other
            other => other
    other => other

export
Eq SemVer where
  v1 == v2 = compareSemVer v1 v2 == EQ

export
Ord SemVer where
  compare = compareSemVer

-- ============================================================================
-- VERSION BUMPING
-- ============================================================================

||| Bump major version (resets minor and patch)
export
bumpMajor : SemVer -> SemVer
bumpMajor v = MkSemVer (S v.major) 0 0 [] Nothing

||| Bump minor version (resets patch)
export
bumpMinor : SemVer -> SemVer
bumpMinor v = MkSemVer v.major (S v.minor) 0 [] Nothing

||| Bump patch version
export
bumpPatch : SemVer -> SemVer
bumpPatch v = MkSemVer v.major v.minor (S v.patch) [] Nothing

||| Set prerelease identifiers
export
setPrerelease : SemVer -> List PreReleaseId -> SemVer
setPrerelease v pre = { prerelease := pre } v

||| Clear prerelease (make release version)
export
clearPrerelease : SemVer -> SemVer
clearPrerelease v = { prerelease := [] } v

||| Set build metadata
export
setBuildMetadata : SemVer -> String -> SemVer
setBuildMetadata v build =
  if validateBuildMetadata build
  then { buildMetadata := Just build } v
  else v

-- ============================================================================
-- VERSION RANGES
-- ============================================================================

||| Version constraint operator
public export
data VersionOp : Type where
  ||| Exact version (=)
  Exact : VersionOp
  ||| Greater than (>)
  GT : VersionOp
  ||| Greater or equal (>=)
  GTE : VersionOp
  ||| Less than (<)
  LT : VersionOp
  ||| Less or equal (<=)
  LTE : VersionOp
  ||| Compatible with (^) - same major, >= version
  Caret : VersionOp
  ||| Approximately (~) - same major.minor, >= version
  Tilde : VersionOp

||| A version constraint
public export
record VersionConstraint where
  constructor MkConstraint
  op : VersionOp
  version : SemVer

||| Check if version satisfies a constraint
export
satisfies : SemVer -> VersionConstraint -> Bool
satisfies v c =
  case c.op of
    Exact => v == c.version
    GT => v > c.version
    GTE => v >= c.version
    LT => v < c.version
    LTE => v <= c.version
    Caret => v.major == c.version.major && v >= c.version
    Tilde => v.major == c.version.major && v.minor == c.version.minor && v >= c.version

||| Version range (conjunction of constraints)
public export
record VersionRange where
  constructor MkRange
  constraints : List VersionConstraint

||| Check if version satisfies all constraints in range
export
satisfiesRange : SemVer -> VersionRange -> Bool
satisfiesRange v range = all (satisfies v) range.constraints

||| Create a caret range (^1.2.3 -> >=1.2.3 <2.0.0)
export
caretRange : SemVer -> VersionRange
caretRange v = MkRange [MkConstraint GTE v, MkConstraint LT (bumpMajor v)]

||| Create a tilde range (~1.2.3 -> >=1.2.3 <1.3.0)
export
tildeRange : SemVer -> VersionRange
tildeRange v = MkRange [MkConstraint GTE v, MkConstraint LT (bumpMinor v)]

-- ============================================================================
-- CALVER (Calendar Versioning)
-- ============================================================================

||| Calendar version
public export
record CalVer where
  constructor MkCalVer
  year : Nat      -- Full year (e.g., 2024)
  month : Nat     -- 1-12
  day : Nat       -- 1-31 (optional, 0 if not used)
  micro : Nat     -- Micro version within the day
  modifier : Maybe String

||| Create calendar version
export
calver : Nat -> Nat -> CalVer
calver year month = MkCalVer year (min 12 (max 1 month)) 0 0 Nothing

||| Create full calendar version
export
calverFull : Nat -> Nat -> Nat -> Nat -> Maybe String -> CalVer
calverFull year month day micro mod =
  MkCalVer year (min 12 (max 1 month)) (min 31 day) micro mod

||| Parse CalVer string (YYYY.MM.DD or YYYY.MM)
export
parseCalVer : String -> Maybe CalVer
parseCalVer s =
  let parts = splitBy '.' s
  in case parts of
       [yearS, monthS] =>
         case (parseNat' yearS, parseNat' monthS) of
           (Just y, Just m) => if m >= 1 && m <= 12
                               then Just (MkCalVer y m 0 0 Nothing)
                               else Nothing
           _ => Nothing
       [yearS, monthS, dayS] =>
         case (parseNat' yearS, parseNat' monthS, parseNat' dayS) of
           (Just y, Just m, Just d) => if m >= 1 && m <= 12 && d >= 1 && d <= 31
                                       then Just (MkCalVer y m d 0 Nothing)
                                       else Nothing
           _ => Nothing
       _ => Nothing
  where
    parseNat' : String -> Maybe Nat
    parseNat' str =
      if null str || not (isAllDigits str) then Nothing
      else Just (cast (foldl (\acc, c => acc * 10 + cast (ord c - ord '0')) 0 (unpack str)))

||| Format CalVer to string
export
formatCalVer : CalVer -> String
formatCalVer v =
  let base = show v.year ++ "." ++ padZero v.month
      withDay = if v.day > 0 then base ++ "." ++ padZero v.day else base
      withMicro = if v.micro > 0 then withDay ++ "." ++ show v.micro else withDay
  in case v.modifier of
       Nothing => withMicro
       Just m => withMicro ++ "-" ++ m
  where
    padZero : Nat -> String
    padZero n = if n < 10 then "0" ++ show n else show n

-- ============================================================================
-- VERSION COMPARISON UTILITIES
-- ============================================================================

||| Check if version is prerelease
export
isPrerelease : SemVer -> Bool
isPrerelease v = not (null v.prerelease)

||| Check if version is stable (>= 1.0.0 and no prerelease)
export
isStable : SemVer -> Bool
isStable v = v.major >= 1 && null v.prerelease

||| Check if two versions are compatible (same major, v2 >= v1)
export
isCompatible : SemVer -> SemVer -> Bool
isCompatible v1 v2 = v1.major == v2.major && v2 >= v1

||| Get next prerelease version
export
nextPrerelease : SemVer -> String -> SemVer
nextPrerelease v tag =
  let newPre = case v.prerelease of
                 [] => [Alphanumeric tag, Numeric 1]
                 [Alphanumeric t, Numeric n] => if t == tag
                                                then [Alphanumeric tag, Numeric (S n)]
                                                else [Alphanumeric tag, Numeric 1]
                 _ => [Alphanumeric tag, Numeric 1]
  in { prerelease := newPre } v

-- ============================================================================
-- COMMON VERSIONS
-- ============================================================================

export v0_0_0 : SemVer
v0_0_0 = semver 0 0 0

export v0_0_1 : SemVer
v0_0_1 = semver 0 0 1

export v0_1_0 : SemVer
v0_1_0 = semver 0 1 0

export v1_0_0 : SemVer
v1_0_0 = semver 1 0 0

