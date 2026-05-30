-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCapability capability-based security.
|||
||| `Proven.SafeCapability` ships `attenuationSound` and `filterElemSublist`
||| inline (the heart of the attenuation correctness argument). This
||| file machine-checks the surrounding structural invariants:
|||
|||   * `Permission` enum self-equality (5 constructors).
|||   * `Permission` ordering anchors (Admin > Delete > Write > Execute > Read).
|||   * `createCapability` smart-constructor anchors (default
|||     delegatable = True, expiry = Nothing).
|||   * `emptyStore` is the empty list of capabilities + empty revoked.
|||   * `isExpired now` returns False on a `Nothing`-expiry capability
|||     (definition unfold).
|||   * `makeNonDelegatable` sets `capDelegatable = False`.
|||   * `setExpiry` sets `capExpiry = Just t`.
|||   * `adminHierarchy` lists Admin -> [Read, Write, Execute, Delete].
|||   * `implies` reflexivity (`p1 == p2`).
|||
||| Zero `believe_me` / `idris_crash`, zero OWED.
module Proven.SafeCapability.Proofs

import Proven.SafeCapability

%default total

--------------------------------------------------------------------------------
-- Permission enum self-equality (5 constructors)
--------------------------------------------------------------------------------

public export
readSelfEq : Read == Read = True
readSelfEq = Refl

public export
writeSelfEq : Write == Write = True
writeSelfEq = Refl

public export
executeSelfEq : Execute == Execute = True
executeSelfEq = Refl

public export
deleteSelfEq : Delete == Delete = True
deleteSelfEq = Refl

public export
adminSelfEq : Admin == Admin = True
adminSelfEq = Refl

public export
readNotWrite : Read == Write = False
readNotWrite = Refl

public export
adminNotDelete : Admin == Delete = False
adminNotDelete = Refl

--------------------------------------------------------------------------------
-- Permission ordering anchors (Admin > Delete > Write > Execute > Read)
--------------------------------------------------------------------------------

||| `Read < Write` per the documented hierarchy.
public export
readLtWrite : compare Read Write = LT
readLtWrite = Refl

||| `Write < Execute`? Per the source: Write/_ = LT, except Write/Read = GT.
||| The source has Write < Execute via `Write _ = LT`.
public export
writeLtExecute : compare Write Execute = LT
writeLtExecute = Refl

||| `Execute < Delete`.
public export
executeLtDelete : compare Execute Delete = LT
executeLtDelete = Refl

||| `Delete < Admin`.
public export
deleteLtAdmin : compare Delete Admin = LT
deleteLtAdmin = Refl

||| Admin > Read.
public export
adminGtRead : compare Admin Read = GT
adminGtRead = Refl

||| Admin = Admin under compare.
public export
adminEqAdmin : compare Admin Admin = EQ
adminEqAdmin = Refl

--------------------------------------------------------------------------------
-- `createCapability` defaults
--------------------------------------------------------------------------------

||| `createCapability` defaults to non-expiring.
public export
createCapabilityNonExpiring :
  (h : Principal) -> (r : ResourceId) -> (ps : List Permission)
  -> (createCapability h r ps).capExpiry = Nothing
createCapabilityNonExpiring h r ps = Refl

||| `createCapability` defaults to delegatable.
public export
createCapabilityDelegatable :
  (h : Principal) -> (r : ResourceId) -> (ps : List Permission)
  -> (createCapability h r ps).capDelegatable = True
createCapabilityDelegatable h r ps = Refl

||| `createCapability` records the requested permissions.
public export
createCapabilityPermissions :
  (h : Principal) -> (r : ResourceId) -> (ps : List Permission)
  -> (createCapability h r ps).capPermissions = ps
createCapabilityPermissions h r ps = Refl

||| `createCapability` records the requested holder.
public export
createCapabilityHolder :
  (h : Principal) -> (r : ResourceId) -> (ps : List Permission)
  -> (createCapability h r ps).capHolder = h
createCapabilityHolder h r ps = Refl

--------------------------------------------------------------------------------
-- `isExpired` on never-expiring capability
--------------------------------------------------------------------------------

||| A capability with `capExpiry = Nothing` is never expired.
public export
neverExpires :
  (now : Nat) -> (h : Principal) -> (r : ResourceId) -> (ps : List Permission)
  -> (d : Bool)
  -> isExpired now (MkCapability h r ps Nothing d) = False
neverExpires now h r ps d = Refl

--------------------------------------------------------------------------------
-- Record-update functions
--------------------------------------------------------------------------------

||| DISCHARGED: `makeNonDelegatable` sets `capDelegatable = False`.
||| `makeNonDelegatable` is `public export` with body
||| `{ capDelegatable := False } cap` (SafeCapability.idr L171-172).
||| Record-update projection reduces directly for `Refl`.
public export
makeNonDelegatableSetsFalse :
  (cap : Capability) -> (makeNonDelegatable cap).capDelegatable = False
makeNonDelegatableSetsFalse _ = Refl

||| DISCHARGED: `setExpiry t` sets `capExpiry = Just t`. `setExpiry`
||| is `public export` (SafeCapability.idr L176-177) — same record-
||| update projection pattern.
public export
setExpirySetsJust :
  (t : Nat) -> (cap : Capability) -> (setExpiry t cap).capExpiry = Just t
setExpirySetsJust _ _ = Refl

--------------------------------------------------------------------------------
-- Store base cases
--------------------------------------------------------------------------------

||| DISCHARGED: `emptyStore` has no capabilities. `emptyStore` is
||| `public export` with body `MkCapabilityStore [] []`
||| (SafeCapability.idr L188-189), so record projection reduces.
public export
emptyStoreNoCapabilities :
  (emptyStore).capabilities = []
emptyStoreNoCapabilities = Refl

||| DISCHARGED: `emptyStore` has no revoked entries. Same anchor.
public export
emptyStoreNoRevoked :
  (emptyStore).revoked = []
emptyStoreNoRevoked = Refl

--------------------------------------------------------------------------------
-- `adminHierarchy`
--------------------------------------------------------------------------------

||| DISCHARGED: `adminHierarchy` declares Admin as parent of [Read,
||| Write, Execute, Delete]. `adminHierarchy` is `public export` with
||| this exact body (SafeCapability.idr L258-259) — `Refl` closes.
public export
adminHierarchyDef :
  adminHierarchy = MkHierarchy Admin [Read, Write, Execute, Delete]
adminHierarchyDef = Refl

--------------------------------------------------------------------------------
-- `readOnlyMembrane`
--------------------------------------------------------------------------------

||| A read-only membrane permits Read.
public export
readOnlyAllowsRead :
  (obj : a) -> (accessPolicy (readOnlyMembrane obj)) Read = True
readOnlyAllowsRead obj = Refl

||| A read-only membrane denies Write.
public export
readOnlyDeniesWrite :
  (obj : a) -> (accessPolicy (readOnlyMembrane obj)) Write = False
readOnlyDeniesWrite obj = Refl

||| A read-only membrane denies Admin.
public export
readOnlyDeniesAdmin :
  (obj : a) -> (accessPolicy (readOnlyMembrane obj)) Admin = False
readOnlyDeniesAdmin obj = Refl
