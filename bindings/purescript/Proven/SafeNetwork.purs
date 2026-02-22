-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeNetwork - FFI bindings to libproven network address validation
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeNetwork
  ( parseIpv4
  , ipv4IsPrivate
  , ipv4IsLoopback
  ) where

import Prelude

import Proven.Result (Result(..), ProvenError(..))

-- | Parse an IPv4 address string into octets (delegates to Idris 2).
foreign import parseIpv4Impl :: String -> { status :: Int, octets :: Array Int }

parseIpv4 :: String -> Result (Array Int) ProvenError
parseIpv4 s =
  let r = parseIpv4Impl s
  in if r.status == 0 then Ok r.octets else Err (InvalidFormat "Invalid IPv4 address")

-- | Check if IPv4 octets represent a private address (delegates to Idris 2).
foreign import ipv4IsPrivateImpl :: Array Int -> Boolean

ipv4IsPrivate :: Array Int -> Boolean
ipv4IsPrivate = ipv4IsPrivateImpl

-- | Check if IPv4 octets represent a loopback address (delegates to Idris 2).
foreign import ipv4IsLoopbackImpl :: Array Int -> Boolean

ipv4IsLoopback :: Array Int -> Boolean
ipv4IsLoopback = ipv4IsLoopbackImpl
