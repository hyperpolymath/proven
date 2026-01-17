-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe semantic version handling.
-- |
-- | Provides parsing and comparison of semantic versions (SemVer)
-- | according to the Semantic Versioning 2.0.0 specification.

module Proven.SafeVersion
  ( SafeVersion
  , Version(..)
  , parse
  , isValid
  , compare
  , major
  , minor
  , patch
  , prerelease
  , buildMetadata
  , increment
  , satisfies
  , toString
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, indexOf, take, drop, length)
import Data.Int (fromString)
import Proven.Result (Result(..), ProvenError(..))

-- | SafeVersion namespace marker (not instantiated).
data SafeVersion

-- | Semantic version with major.minor.patch and optional pre-release/build.
newtype Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  , prerelease :: Maybe String
  , buildMetadata :: Maybe String
  }

derive instance eqVersion :: Eq Version

instance showVersion :: Show Version where
  show = toString

instance ordVersion :: Ord Version where
  compare = compareVersions

-- | Parse a semantic version string.
parse :: String -> Result Version ProvenError
parse s =
  let result = parseVersionImpl s
  in if result.valid
     then Ok (Version
       { major: result.major
       , minor: result.minor
       , patch: result.patch
       , prerelease: if result.prerelease == "" then Nothing else Just result.prerelease
       , buildMetadata: if result.build == "" then Nothing else Just result.build
       })
     else Err InvalidVersion

foreign import parseVersionImpl :: String ->
  { valid :: Boolean
  , major :: Int
  , minor :: Int
  , patch :: Int
  , prerelease :: String
  , build :: String
  }

-- | Check if a string is a valid semantic version.
isValid :: String -> Boolean
isValid s = (parseVersionImpl s).valid

-- | Compare two versions.
compareVersions :: Version -> Version -> Ordering
compareVersions (Version a) (Version b) =
  case Prelude.compare a.major b.major of
    EQ -> case Prelude.compare a.minor b.minor of
      EQ -> case Prelude.compare a.patch b.patch of
        EQ -> comparePrerelease a.prerelease b.prerelease
        other -> other
      other -> other
    other -> other

comparePrerelease :: Maybe String -> Maybe String -> Ordering
comparePrerelease Nothing Nothing = EQ
comparePrerelease Nothing (Just _) = GT  -- No prerelease > prerelease
comparePrerelease (Just _) Nothing = LT  -- Prerelease < no prerelease
comparePrerelease (Just a) (Just b) = Prelude.compare a b

-- | Get the major version number.
major :: Version -> Int
major (Version v) = v.major

-- | Get the minor version number.
minor :: Version -> Int
minor (Version v) = v.minor

-- | Get the patch version number.
patch :: Version -> Int
patch (Version v) = v.patch

-- | Get the pre-release identifier if present.
prerelease :: Version -> Maybe String
prerelease (Version v) = v.prerelease

-- | Get the build metadata if present.
buildMetadata :: Version -> Maybe String
buildMetadata (Version v) = v.buildMetadata

-- | Increment type for version bumping.
data IncrementType = Major | Minor | Patch

-- | Increment a version component.
increment :: IncrementType -> Version -> Version
increment Major (Version v) = Version v
  { major = v.major + 1
  , minor = 0
  , patch = 0
  , prerelease = Nothing
  , buildMetadata = Nothing
  }
increment Minor (Version v) = Version v
  { minor = v.minor + 1
  , patch = 0
  , prerelease = Nothing
  , buildMetadata = Nothing
  }
increment Patch (Version v) = Version v
  { patch = v.patch + 1
  , prerelease = Nothing
  , buildMetadata = Nothing
  }

-- | Check if a version satisfies a version constraint.
-- | Supports simple operators: =, >, <, >=, <=, ^, ~
satisfies :: String -> Version -> Boolean
satisfies constraint version = satisfiesImpl constraint (toString version)

foreign import satisfiesImpl :: String -> String -> Boolean

-- | Convert version to string.
toString :: Version -> String
toString (Version v) =
  show v.major <> "." <> show v.minor <> "." <> show v.patch <>
  (case v.prerelease of
    Just pre -> "-" <> pre
    Nothing -> "") <>
  (case v.buildMetadata of
    Just build -> "+" <> build
    Nothing -> "")
