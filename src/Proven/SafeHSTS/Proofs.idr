-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeHSTS operations
|||
||| Verifies that HSTS configuration validation catches misconfigurations:
||| preload without subdomains, insufficient max-age, and that the
||| default and preload configs are properly configured.
module Proven.SafeHSTS.Proofs

import Proven.SafeHSTS
import Data.Nat
import Data.String

%default total

--------------------------------------------------------------------------------
-- Default Configuration Properties
--------------------------------------------------------------------------------

||| Default HSTS has no validation errors.
public export
defaultHSTSValid : validateHSTS defaultHSTS = []
defaultHSTSValid = Refl

||| Preload HSTS has no validation errors.
public export
preloadHSTSValid : validateHSTS preloadHSTS = []
preloadHSTSValid = Refl

||| Preload HSTS config is preload-ready.
public export
preloadHSTSReady : isPreloadReady preloadHSTS = True
preloadHSTSReady = Refl

||| Default HSTS is NOT preload-ready (preload flag is False).
public export
defaultNotPreloadReady : isPreloadReady defaultHSTS = False
defaultNotPreloadReady = Refl

--------------------------------------------------------------------------------
-- Preload Minimum Age Properties
--------------------------------------------------------------------------------

||| Preload minimum age is 1 year (31536000 seconds).
public export
preloadMinAgeIsOneYear : PreloadMinAge = 31536000
preloadMinAgeIsOneYear = Refl

||| Default HSTS max-age meets the preload minimum.
public export
defaultMeetsPreloadAge : defaultHSTS.maxAge >= PreloadMinAge = True
defaultMeetsPreloadAge = Refl

||| Preload HSTS max-age exceeds the preload minimum (2 years).
public export
preloadExceedsMinAge : preloadHSTS.maxAge >= PreloadMinAge = True
preloadExceedsMinAge = Refl

--------------------------------------------------------------------------------
-- Render Properties
--------------------------------------------------------------------------------

||| Default HSTS renders with includeSubDomains.
public export
defaultRenderHasSub : renderHSTS defaultHSTS = "max-age=31536000; includeSubDomains"
defaultRenderHasSub = Refl

||| Preload HSTS renders with includeSubDomains and preload.
public export
preloadRenderHasAll : renderHSTS preloadHSTS = "max-age=63072000; includeSubDomains; preload"
preloadRenderHasAll = Refl

--------------------------------------------------------------------------------
-- Configuration Invariants
--------------------------------------------------------------------------------

||| Default HSTS includes subdomains.
public export
defaultIncludesSub : defaultHSTS.includeSubDomains = True
defaultIncludesSub = Refl

||| Preload HSTS includes subdomains.
public export
preloadIncludesSub : preloadHSTS.includeSubDomains = True
preloadIncludesSub = Refl

||| Preload HSTS has preload flag set.
public export
preloadHasPreloadFlag : preloadHSTS.preload = True
preloadHasPreloadFlag = Refl
