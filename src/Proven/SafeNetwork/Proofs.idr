-- SPDX-License-Identifier: MPL-2.0
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

||| OWED: `mkPort n` returns `Just _` whenever `n <= 65535`. Held back
||| by Idris2 0.8.0 not bridging the runtime `Bool` test
||| `n <= 65535 = True` (used by `checkPort`) to the type-level `LTE n
||| 65535` witness: `mkPort` branches on `checkPort n`, which evaluates
||| the opaque primitive Nat `<=`, so `IsJust (mkPort n)` does not
||| reduce by Refl from an `LTE` premise. Discharge once a Nat
||| decidability bridge (`lteReflectsLTE : (n <= m) = True -> LTE n m`)
||| is exposed by `Data.Nat`, or `mkPort` is refactored to take the
||| `LTE` proof directly.
public export
0 mkPortSucceeds : (n : Nat) -> LTE n 65535 -> IsJust (mkPort n)

||| OWED: every CIDR block contains its own network address. Held back
||| by Idris2 0.8.0 not reducing `contains` on an abstract `CIDRBlock`:
||| `contains` is defined via `ipToNat ... >= ipToNat ...`, and those
||| primitive Nat `>=` comparisons stay opaque at the type level when
||| the `CIDRBlock` is a free variable rather than a concrete literal.
||| Discharge once `ipToNat` monotonicity lemmas are proven and a
||| reflective bridge from primitive `>=` to `GTE` is available.
public export
0 networkInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (networkAddress cidr) = True

||| OWED: every CIDR block contains its own broadcast address. Same
||| blocker family as `networkInOwnCIDR` — `contains` does not reduce
||| on an abstract `CIDRBlock` argument because of opaque primitive
||| Nat `>=`. Discharge once `ipToNat` monotonicity + a `>=`-to-`GTE`
||| reflective bridge are in place.
public export
0 broadcastInOwnCIDR : (cidr : CIDRBlock) -> contains cidr (broadcastAddress cidr) = True

||| CIDR subset transitivity: if A is a subset of B and B is a subset of C then A is a subset of C
||| This follows from the transitivity of the containment relation
public export
data SubsetTransitive : CIDRBlock -> CIDRBlock -> CIDRBlock -> Type where
  MkSubsetTransitive : isSubsetOf a b = True ->
                       isSubsetOf b c = True ->
                       SubsetTransitive a b c

||| OWED: a port classified as a system port has value `<= 1023`.
||| Held back by Idris2 0.8.0's Bool-to-LTE bridge: `isSystemPort` is a
||| runtime classification that pattern-matches on the result of
||| `classify`, and the `True` outcome does not type-level reduce to a
||| `LTE` witness without an opaque primitive Nat comparison.
||| Discharge once `classify` is reformulated to return a dependent
||| witness (e.g. `(c : Class ** ClassifiedAs p c)`) carrying the
||| `LTE` proof, or once a primitive-Nat reflective bridge lands.
public export
0 systemPortBound : (p : Port) -> isSystemPort p = True -> LTE (portValue p) 1023

||| OWED: every well-typed `Port` has `portValue p <= 65535`. Held
||| back by the move to runtime bounds checking: `mkPort` / `unsafeMkPort`
||| now carry an erased `IsPort` witness (`data IsPort : Nat -> Type
||| where ItIsPort : IsPort n`) rather than a constructive `LTE n
||| 65535` proof, so the bound is enforced dynamically by `checkPort`
||| but not recoverable at the type level by Refl. Discharge once
||| `IsPort` is refined to a non-trivial witness carrying the `LTE`
||| (i.e. `IsPort n := LTE n 65535`), or `Port` is rebuilt to store
||| the `LTE` proof directly.
public export
0 portBounded : (p : Port) -> LTE (portValue p) 65535

||| Proof that host count is positive for prefix < 32
public export
data PositiveHostCount : PrefixLength -> Type where
  MkPositiveHostCount : LTE 1 (hostCount pfx) -> PositiveHostCount pfx

||| Proof that private networks are not routable
public export
data PrivateNetwork : CIDRBlock -> Type where
  MkPrivateNetwork : isPrivate cidr = True -> PrivateNetwork cidr
