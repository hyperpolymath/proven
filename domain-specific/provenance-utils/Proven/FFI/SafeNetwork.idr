-- SPDX-License-Identifier: PMPL-1.0-or-later
||| FFI exports for SafeNetwork operations (Idris-only logic)
module Proven.FFI.SafeNetwork

import Proven.SafeNetwork.IPv4
import Data.Bits

%default total

%export
proven_idris_network_parse_ipv4 : String -> Int
proven_idris_network_parse_ipv4 s = case parseIPv4 s of
  Just ip => cast (ipv4ToInteger ip)
  Nothing => -1

%export
proven_idris_network_ipv4_is_private : Int -> Bool
proven_idris_network_ipv4_is_private n =
  if n < 0
    then False
    else isPrivateIPv4 (integerToIPv4 (cast n))

%export
proven_idris_network_ipv4_is_loopback : Int -> Bool
proven_idris_network_ipv4_is_loopback n =
  if n < 0
    then False
    else isLoopbackIPv4 (integerToIPv4 (cast n))
