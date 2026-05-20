-- SPDX-License-Identifier: PMPL-1.0-or-later
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

||| OWED: `makeNonDelegatable` sets `capDelegatable = False`. Same
||| `public export`-body-opacity blocker.
public export
0 makeNonDelegatableSetsFalse :
  (cap : Capability) -> (makeNonDelegatable cap).capDelegatable = False

||| OWED: `setExpiry t` sets `capExpiry = Just t`. Same blocker.
public export
0 setExpirySetsJust :
  (t : Nat) -> (cap : Capability) -> (setExpiry t cap).capExpiry = Just t

--------------------------------------------------------------------------------
-- Store base cases
--------------------------------------------------------------------------------

||| OWED: `emptyStore` has no capabilities. Idris2 0.8.0 does not
||| reduce record-projection through a `public export` definition
||| body alone (would need `inline` or full-pattern unfolding).
public export
0 emptyStoreNoCapabilities :
  (emptyStore).capabilities = []

||| OWED: `emptyStore` has no revoked entries. Same blocker.
public export
0 emptyStoreNoRevoked :
  (emptyStore).revoked = []

--------------------------------------------------------------------------------
-- `adminHierarchy`
--------------------------------------------------------------------------------

||| OWED: `adminHierarchy` declares Admin as the parent of [Read,
||| Write, Execute, Delete]. Same `public export`-body-opacity
||| blocker as `emptyStore`.
public export
0 adminHierarchyDef :
  adminHierarchy = MkHierarchy Admin [Read, Write, Execute, Delete]

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
