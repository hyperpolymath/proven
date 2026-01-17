{- SPDX-License-Identifier: PMPL-1.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe semantic versioning operations.
--
-- Provides semantic version parsing, comparison, and manipulation
-- following the SemVer 2.0.0 specification.
module Proven.SafeVersion
  ( -- * Types
    SemVer(..)
  , VersionRange(..)
    -- * Construction
  , makeSemVer
  , parseSemVer
  , parseVersionRange
    -- * Validation
  , isValidSemVer
  , isStable
  , isPrerelease
    -- * Comparison
  , compareSemVer
  , semVerEq
  , semVerLt
  , semVerLte
  , semVerGt
  , semVerGte
    -- * Range Checking
  , satisfiesRange
  , satisfiesExact
  , satisfiesMinor
  , satisfiesMajor
    -- * Manipulation
  , bumpMajor
  , bumpMinor
  , bumpPatch
  , setPrerelease
  , setBuild
  , clearPrerelease
  , clearBuild
    -- * Rendering
  , renderSemVer
  , renderVersionRange
    -- * Constants
  , initialVersion
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Proven.Core (ProvenError(..), Result)

-- | Semantic version structure.
data SemVer = SemVer
  { svMajor      :: !Int
  , svMinor      :: !Int
  , svPatch      :: !Int
  , svPrerelease :: ![Text]
  , svBuild      :: ![Text]
  } deriving (Eq, Show)

-- | Version range specification.
data VersionRange
  = Exact !SemVer         -- ^ Exact version match
  | Range !SemVer !SemVer -- ^ Version range (inclusive)
  | Caret !SemVer         -- ^ Caret range (^1.2.3)
  | Tilde !SemVer         -- ^ Tilde range (~1.2.3)
  | GreaterThan !SemVer   -- ^ Greater than
  | LessThan !SemVer      -- ^ Less than
  | GreaterEq !SemVer     -- ^ Greater or equal
  | LessEq !SemVer        -- ^ Less or equal
  | Any                   -- ^ Any version (*)
  deriving (Eq, Show)

-- | Create a semantic version.
makeSemVer :: Int -> Int -> Int -> Result SemVer
makeSemVer major minor patch
  | major < 0 = Left (OutOfRange "Major version must be non-negative")
  | minor < 0 = Left (OutOfRange "Minor version must be non-negative")
  | patch < 0 = Left (OutOfRange "Patch version must be non-negative")
  | otherwise = Right SemVer
      { svMajor = major
      , svMinor = minor
      , svPatch = patch
      , svPrerelease = []
      , svBuild = []
      }

-- | Parse a semantic version string.
parseSemVer :: Text -> Result SemVer
parseSemVer input = do
  let trimmed = T.strip input
  when (T.null trimmed) $ Left (InvalidInput "Empty version string")
  let (versionPart, rest) = T.breakOn "+" trimmed
  let buildMeta = if T.null rest then [] else T.splitOn "." (T.drop 1 rest)
  let (corePart, prereleaseRest) = T.breakOn "-" versionPart
  let prerelease = if T.null prereleaseRest then [] else T.splitOn "." (T.drop 1 prereleaseRest)
  case T.splitOn "." corePart of
    [majT, minT, patT] -> do
      major <- parseVersion majT
      minor <- parseVersion minT
      patch <- parseVersion patT
      Right SemVer
        { svMajor = major
        , svMinor = minor
        , svPatch = patch
        , svPrerelease = prerelease
        , svBuild = buildMeta
        }
    _ -> Left (InvalidFormat "Version must be in X.Y.Z format")
  where
    when False _ = Right ()
    when True e = e

parseVersion :: Text -> Result Int
parseVersion t = case reads (T.unpack t) of
  [(n, "")] | n >= 0 -> Right n
  _ -> Left (InvalidFormat "Invalid version number")

-- | Parse a version range.
parseVersionRange :: Text -> Result VersionRange
parseVersionRange input = do
  let trimmed = T.strip input
  case T.uncons trimmed of
    Nothing -> Left (InvalidInput "Empty range")
    Just ('*', _) -> Right Any
    Just ('^', rest) -> Caret <$> parseSemVer rest
    Just ('~', rest) -> Tilde <$> parseSemVer rest
    Just ('>', rest) ->
      case T.uncons rest of
        Just ('=', r) -> GreaterEq <$> parseSemVer (T.strip r)
        _ -> GreaterThan <$> parseSemVer (T.strip rest)
    Just ('<', rest) ->
      case T.uncons rest of
        Just ('=', r) -> LessEq <$> parseSemVer (T.strip r)
        _ -> LessThan <$> parseSemVer (T.strip rest)
    _ -> Exact <$> parseSemVer trimmed

-- | Check if version string is valid.
isValidSemVer :: Text -> Bool
isValidSemVer input = case parseSemVer input of
  Right _ -> True
  Left _ -> False

-- | Check if version is stable (no prerelease).
isStable :: SemVer -> Bool
isStable sv = null (svPrerelease sv)

-- | Check if version is prerelease.
isPrerelease :: SemVer -> Bool
isPrerelease = not . isStable

-- | Compare two semantic versions.
compareSemVer :: SemVer -> SemVer -> Ordering
compareSemVer a b =
  case compare (svMajor a) (svMajor b) of
    EQ -> case compare (svMinor a) (svMinor b) of
      EQ -> case compare (svPatch a) (svPatch b) of
        EQ -> comparePrerelease (svPrerelease a) (svPrerelease b)
        other -> other
      other -> other
    other -> other

comparePrerelease :: [Text] -> [Text] -> Ordering
comparePrerelease [] [] = EQ
comparePrerelease [] _ = GT  -- Release > prerelease
comparePrerelease _ [] = LT
comparePrerelease (a:as) (b:bs) =
  case compareIdentifier a b of
    EQ -> comparePrerelease as bs
    other -> other

compareIdentifier :: Text -> Text -> Ordering
compareIdentifier a b =
  case (reads (T.unpack a) :: [(Int, String)], reads (T.unpack b)) of
    ([(na, "")], [(nb, "")]) -> compare na nb
    _ -> compare a b

-- | Check if two versions are equal.
semVerEq :: SemVer -> SemVer -> Bool
semVerEq a b = compareSemVer a b == EQ

-- | Check if first version is less than second.
semVerLt :: SemVer -> SemVer -> Bool
semVerLt a b = compareSemVer a b == LT

-- | Check if first version is less than or equal to second.
semVerLte :: SemVer -> SemVer -> Bool
semVerLte a b = compareSemVer a b /= GT

-- | Check if first version is greater than second.
semVerGt :: SemVer -> SemVer -> Bool
semVerGt a b = compareSemVer a b == GT

-- | Check if first version is greater than or equal to second.
semVerGte :: SemVer -> SemVer -> Bool
semVerGte a b = compareSemVer a b /= LT

-- | Check if version satisfies range.
satisfiesRange :: VersionRange -> SemVer -> Bool
satisfiesRange Any _ = True
satisfiesRange (Exact v) sv = semVerEq v sv
satisfiesRange (Range lo hi) sv = semVerGte sv lo && semVerLte sv hi
satisfiesRange (Caret v) sv = satisfiesCaret v sv
satisfiesRange (Tilde v) sv = satisfiesTilde v sv
satisfiesRange (GreaterThan v) sv = semVerGt sv v
satisfiesRange (LessThan v) sv = semVerLt sv v
satisfiesRange (GreaterEq v) sv = semVerGte sv v
satisfiesRange (LessEq v) sv = semVerLte sv v

satisfiesCaret :: SemVer -> SemVer -> Bool
satisfiesCaret base sv
  | svMajor base > 0 = svMajor sv == svMajor base && semVerGte sv base
  | svMinor base > 0 = svMajor sv == 0 && svMinor sv == svMinor base && semVerGte sv base
  | otherwise = semVerEq sv base

satisfiesTilde :: SemVer -> SemVer -> Bool
satisfiesTilde base sv =
  svMajor sv == svMajor base &&
  svMinor sv == svMinor base &&
  semVerGte sv base

-- | Check if version exactly matches.
satisfiesExact :: SemVer -> SemVer -> Bool
satisfiesExact = semVerEq

-- | Check if version satisfies minor constraint (same major.minor).
satisfiesMinor :: SemVer -> SemVer -> Bool
satisfiesMinor base sv =
  svMajor sv == svMajor base && svMinor sv == svMinor base

-- | Check if version satisfies major constraint (same major).
satisfiesMajor :: SemVer -> SemVer -> Bool
satisfiesMajor base sv = svMajor sv == svMajor base

-- | Bump major version.
bumpMajor :: SemVer -> SemVer
bumpMajor sv = sv
  { svMajor = svMajor sv + 1
  , svMinor = 0
  , svPatch = 0
  , svPrerelease = []
  }

-- | Bump minor version.
bumpMinor :: SemVer -> SemVer
bumpMinor sv = sv
  { svMinor = svMinor sv + 1
  , svPatch = 0
  , svPrerelease = []
  }

-- | Bump patch version.
bumpPatch :: SemVer -> SemVer
bumpPatch sv = sv
  { svPatch = svPatch sv + 1
  , svPrerelease = []
  }

-- | Set prerelease identifiers.
setPrerelease :: [Text] -> SemVer -> SemVer
setPrerelease pre sv = sv { svPrerelease = pre }

-- | Set build metadata.
setBuild :: [Text] -> SemVer -> SemVer
setBuild build sv = sv { svBuild = build }

-- | Clear prerelease.
clearPrerelease :: SemVer -> SemVer
clearPrerelease sv = sv { svPrerelease = [] }

-- | Clear build metadata.
clearBuild :: SemVer -> SemVer
clearBuild sv = sv { svBuild = [] }

-- | Render version to string.
renderSemVer :: SemVer -> Text
renderSemVer sv =
  T.pack (show (svMajor sv) ++ "." ++ show (svMinor sv) ++ "." ++ show (svPatch sv)) <>
  (if null (svPrerelease sv) then "" else "-" <> T.intercalate "." (svPrerelease sv)) <>
  (if null (svBuild sv) then "" else "+" <> T.intercalate "." (svBuild sv))

-- | Render version range to string.
renderVersionRange :: VersionRange -> Text
renderVersionRange Any = "*"
renderVersionRange (Exact v) = renderSemVer v
renderVersionRange (Range lo hi) = renderSemVer lo <> " - " <> renderSemVer hi
renderVersionRange (Caret v) = "^" <> renderSemVer v
renderVersionRange (Tilde v) = "~" <> renderSemVer v
renderVersionRange (GreaterThan v) = ">" <> renderSemVer v
renderVersionRange (LessThan v) = "<" <> renderSemVer v
renderVersionRange (GreaterEq v) = ">=" <> renderSemVer v
renderVersionRange (LessEq v) = "<=" <> renderSemVer v

-- | Initial development version.
initialVersion :: SemVer
initialVersion = SemVer 0 1 0 [] []
