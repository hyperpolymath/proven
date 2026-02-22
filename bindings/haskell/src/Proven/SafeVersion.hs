{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe semantic versioning operations via libproven FFI.
--
-- Version parsing and comparison are performed by the Idris 2 verified core.
module Proven.SafeVersion
  ( SemVer(..)
  , parseSemVer
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Proven.FFI (c_proven_version_parse, c_proven_version_free)
import Proven.FFI.Types (VersionResult(..), FFI_SemanticVersion(..))
import Proven.Core (withCStringLen')

-- | Parsed semantic version.
data SemVer = SemVer
  { svMajor :: !Int
  , svMinor :: !Int
  , svPatch :: !Int
  , svPrerelease :: !String
  } deriving (Eq, Show)

-- | Parse a semantic version string.
-- Delegates to @proven_version_parse@ in libproven.
parseSemVer :: String -> IO (Maybe SemVer)
parseSemVer str = withCStringLen' str $ \ptr len -> do
  result <- c_proven_version_parse ptr len
  if vrStatusRaw result == 0
    then do
      let ver = vrVersion result
      prerelease <- if svPrereleasePtr ver /= nullPtr && svPrereleaseLen ver > 0
                    then peekCStringLen (svPrereleasePtr ver,
                                        fromIntegral (svPrereleaseLen ver))
                    else return ""
      return (Just (SemVer
        (fromIntegral (svMajorRaw ver))
        (fromIntegral (svMinorRaw ver))
        (fromIntegral (svPatchRaw ver))
        prerelease))
    else return Nothing
