-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeUrl - FFI bindings to libproven URL parsing and validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeUrl
  ( parseUrl
  , UrlComponents
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parsed URL components returned by the FFI.
type UrlComponents =
  { scheme :: String
  , host :: String
  , port :: Int
  , path :: String
  , query :: String
  , fragment :: String
  }

-- | Parse a URL string into components (delegates to Idris 2).
foreign import parseUrlImpl :: String ->
  { status :: Int
  , scheme :: String
  , host :: String
  , port :: Int
  , path :: String
  , query :: String
  , fragment :: String
  }

parseUrl :: String -> Result UrlComponents ProvenError
parseUrl s =
  let r = parseUrlImpl s
  in if r.status == 0
     then Ok
       { scheme: r.scheme
       , host: r.host
       , port: r.port
       , path: r.path
       , query: r.query
       , fragment: r.fragment
       }
     else Err InvalidUrl
