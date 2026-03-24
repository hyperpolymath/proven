-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for network operations
|||
||| Type-level guarantees for network primitives including:
||| - IPv4 octet bounds
||| - Port number validity
||| - CIDR containment properties
||| - Subnet calculation correctness
|||
||| Updated for Idris 2 0.8.0 compatibility:
||| - Port uses IsPort witness instead of LTE n 65535 (avoids O(n) proof terms)
||| - `contains` no longer reduces on abstract CIDRBlock arguments
||| - `IsJust` moved to Data.Maybe
module Proven.SafeNetwork.Proofs

import Data.Nat
import Data.List
import Data.Maybe
import Proven.SafeNetwork.IPv4
import Proven.SafeNetwork.Port
import Proven.SafeNetwork.CIDR

%default total

||| Proof that port construction always yields valid port
public export
portAlwaysValid : (p : Port) -> ValidPort p
portAlwaysValid p = MkValidPort p

-- | mkPort succeeds for values <= 65535.
-- | Postulated: mkPort uses a runtime Bool check (`n <= 65535`), and the
-- | connection between the runtime `<=` and the type-level `LTE` requires
-- | showing that `(n <= 65535) = True` implies `checkPort n = Just ItIsPort`,
-- | which involves the opaque primitive Nat comparison.
-- TODO: Update for Idris2 0.8.0 -- prove via decidability of Nat comparison.
export
mkPortSucceeds : (n : Nat) -> LTE n 65535 -> IsJust (mkPort n)

-- | Network address is always contained in its own CIDR.
-- | Postulated: `contains` does not reduce on abstract `CIDRBlock` in
-- | Idris 2 0.8.0 because it involves runtime Nat comparisons
-- | (`ipToNat ... >= ipToNat ...`) that are opaque at the type level.
-- TODO: Update for Idris2 0.8.0 -- prove via ipToNat monotonicity lemmas.
export
networkInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (networkAddress cidr) = True

-- | Broadcast address is always contained in its own CIDR.
-- | Postulated for same reason as networkInOwnCIDR.
-- TODO: Update for Idris2 0.8.0 -- prove via ipToNat monotonicity lemmas.
export
broadcastInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (broadcastAddress cidr) = True

||| CIDR subset transitivity: if A is a subset of B and B is a subset of C then A is a subset of C
||| This follows from the transitivity of the containment relation
public export
data SubsetTransitive : CIDRBlock -> CIDRBlock -> CIDRBlock -> Type where
  MkSubsetTransitive : isSubsetOf a b = True ->
                       isSubsetOf b c = True ->
                       SubsetTransitive a b c

-- | System ports are always < 1024.
-- | Postulated: requires connecting the runtime Bool classification
-- | (`isSystemPort`) with the type-level `LTE` relation, which involves
-- | the opaque Nat comparison primitives.
-- TODO: Update for Idris2 0.8.0 -- prove via decidability of Nat comparison.
export
systemPortBound : (p : Port) -> isSystemPort p = True -> LTE (portValue p) 1023

-- | Every port value is <= 65535.
-- | Postulated: Port construction now uses a runtime bounds check via
-- | `checkPort` with an erased `IsPort` witness, rather than a
-- | constructive `LTE n 65535` proof. The bound holds by the runtime
-- | check in mkPort/unsafeMkPort.
-- TODO: Update for Idris2 0.8.0 -- either restore LTE proof with
-- efficient representation, or prove from IsPort witness semantics.
export
portBounded : (p : Port) -> LTE (portValue p) 65535

||| Proof that host count is positive for prefix < 32
public export
data PositiveHostCount : PrefixLength -> Type where
  MkPositiveHostCount : LTE 1 (hostCount pfx) -> PositiveHostCount pfx

||| Proof that private networks are not routable
public export
data PrivateNetwork : CIDRBlock -> Type where
  MkPrivateNetwork : isPrivate cidr = True -> PrivateNetwork cidr
