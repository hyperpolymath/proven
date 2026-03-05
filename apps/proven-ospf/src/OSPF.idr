-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-ospf skeleton.
-- | Re-exports OSPF.Types and defines protocol constants for
-- | RFC 2328 OSPF v2.

module OSPF

import public OSPF.Types

%default total

||| IP protocol number for OSPF per IANA.
public export
ospfProtocol : Nat
ospfProtocol = 89

||| Default Hello interval in seconds per RFC 2328 Section C.3.
public export
helloInterval : Nat
helloInterval = 10

||| Default RouterDead interval in seconds per RFC 2328 Section C.3.
public export
deadInterval : Nat
deadInterval = 40

||| Multicast address for all OSPF routers (AllSPFRouters).
public export
allSPFRouters : String
allSPFRouters = "224.0.0.5"

||| Multicast address for all OSPF designated routers (AllDRouters).
public export
allDRouters : String
allDRouters = "224.0.0.6"
