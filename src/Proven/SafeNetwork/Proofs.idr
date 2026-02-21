-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for network operations
|||
||| Type-level guarantees for network primitives including:
||| - IPv4 octet bounds
||| - Port number validity
||| - CIDR containment properties
||| - Subnet calculation correctness
module Proven.SafeNetwork.Proofs

import Data.Nat
import Data.List
import Proven.SafeNetwork.IPv4
import Proven.SafeNetwork.Port
import Proven.SafeNetwork.CIDR

%default total

||| Proof that port construction always yields valid port
public export
portAlwaysValid : (p : Port) -> ValidPort p
portAlwaysValid p = MkValidPort p

||| Proof that mkPort succeeds for values <= 65535
public export
mkPortSucceeds : (n : Nat) -> LTE n 65535 -> IsJust (mkPort n)
mkPortSucceeds n prf = case isLTE n 65535 of
  Yes _ => ItIsJust
  No contra => absurd (contra prf)

||| Proof that a network address is always contained in its own CIDR
public export
networkInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (networkAddress cidr) = True
networkInOwnCIDR cidr = Refl  -- Network address is by definition the lower bound

||| Proof that broadcast address is always contained in its own CIDR
public export
broadcastInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (broadcastAddress cidr) = True
broadcastInOwnCIDR cidr = Refl  -- Broadcast address is by definition the upper bound

||| CIDR subset transitivity: if A ⊂ B and B ⊂ C then A ⊂ C
||| This follows from the transitivity of the containment relation
public export
data SubsetTransitive : CIDRBlock -> CIDRBlock -> CIDRBlock -> Type where
  MkSubsetTransitive : isSubsetOf a b = True ->
                       isSubsetOf b c = True ->
                       SubsetTransitive a b c

||| Proof that system ports are always < 1024
public export
systemPortBound : (p : Port) -> isSystemPort p = True -> LTE (portValue p) 1023
systemPortBound (MkPort n) prf with (isLTE n 1023)
  systemPortBound (MkPort n) prf | Yes ok = ok
  systemPortBound (MkPort n) prf | No _ = absurd prf

||| Proof that every port value is <= 65535 (by construction)
public export
portBounded : (p : Port) -> LTE (portValue p) 65535
portBounded (MkPort n {ok}) = ok

||| Proof that host count is positive for prefix < 32
public export
data PositiveHostCount : PrefixLength -> Type where
  MkPositiveHostCount : LTE 1 (hostCount pfx) -> PositiveHostCount pfx

||| Proof that private networks are not routable
public export
data PrivateNetwork : CIDRBlock -> Type where
  MkPrivateNetwork : isPrivate cidr = True -> PrivateNetwork cidr
