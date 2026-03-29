-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeHSTS - HTTP Strict Transport Security
|||
||| Provides type-safe HSTS header construction per RFC 6797.
||| Prevents: SSL stripping, downgrade attacks, mixed content.
module Proven.SafeHSTS

import Data.String
import Data.Nat

%default total

||| Minimum max-age for HSTS preload (1 year = 31536000 seconds)
public export
PreloadMinAge : Nat
PreloadMinAge = 31536000

||| HSTS configuration
public export
record HSTSConfig where
  constructor MkHSTSConfig
  maxAge            : Nat   -- Seconds
  includeSubDomains : Bool
  preload           : Bool

||| Default HSTS (1 year, subdomains, no preload)
public export
defaultHSTS : HSTSConfig
defaultHSTS = MkHSTSConfig 31536000 True False

||| Preload-ready HSTS (2 years, subdomains, preload)
public export
preloadHSTS : HSTSConfig
preloadHSTS = MkHSTSConfig 63072000 True True

||| Render HSTS header value
public export
renderHSTS : HSTSConfig -> String
renderHSTS cfg =
  "max-age=" ++ show cfg.maxAge ++
  (if cfg.includeSubDomains then "; includeSubDomains" else "") ++
  (if cfg.preload then "; preload" else "")

||| Validation errors
public export
data HSTSError =
    MaxAgeTooLow Nat        -- max-age below recommended minimum
  | PreloadMissingSubDomains -- preload requires includeSubDomains
  | PreloadMaxAgeTooLow Nat  -- preload requires >= 1 year

public export
Show HSTSError where
  show (MaxAgeTooLow n) = "max-age " ++ show n ++ " is below recommended 1 year"
  show PreloadMissingSubDomains = "preload requires includeSubDomains"
  show (PreloadMaxAgeTooLow n) = "preload requires max-age >= " ++ show PreloadMinAge ++ ", got " ++ show n

||| Validate HSTS configuration
public export
validateHSTS : HSTSConfig -> List HSTSError
validateHSTS cfg =
  let ageWarn = if cfg.maxAge < 86400 then [MaxAgeTooLow cfg.maxAge] else []
      preloadSub = if cfg.preload && not cfg.includeSubDomains then [PreloadMissingSubDomains] else []
      preloadAge = if cfg.preload && cfg.maxAge < PreloadMinAge then [PreloadMaxAgeTooLow cfg.maxAge] else []
  in ageWarn ++ preloadSub ++ preloadAge

||| Check if configuration is valid for HSTS preload list submission
public export
isPreloadReady : HSTSConfig -> Bool
isPreloadReady cfg =
  cfg.maxAge >= PreloadMinAge && cfg.includeSubDomains && cfg.preload &&
  isNil (validateHSTS cfg)
