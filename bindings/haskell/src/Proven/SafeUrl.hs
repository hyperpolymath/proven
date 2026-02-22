{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe URL operations via libproven FFI.
--
-- URL parsing and validation are performed by the Idris 2 verified core.
-- The UrlResult is an opaque struct managed by libproven; callers must
-- use @proven_url_free@ to release resources.
module Proven.SafeUrl
  ( -- Note: URL parsing returns an opaque result from libproven.
    -- High-level wrappers for individual URL components can be
    -- built atop the raw FFI if needed. For now, this module
    -- re-exports the raw FFI for direct use.
  ) where

-- URL operations require working with UrlResult/UrlComponents structs
-- which contain multiple allocated strings. The raw FFI functions
-- (c_proven_url_parse, c_proven_url_free) are available from Proven.FFI.
-- A full high-level wrapper would manage the lifecycle of the opaque
-- UrlResult, but that is left to application code to avoid imposing
-- a specific resource management pattern.
