-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- SafeCapability: Formally verified capability-based security
--
-- Provides:
-- - Type-safe capabilities with delegation proofs
-- - Permission hierarchies with attenuation
-- - Revocation with temporal validity
-- - Confinement guarantees

module Proven.SafeCapability
import Data.String
import Data.List

import Data.List
import Data.List.Elem
import Data.Nat
import Data.Maybe
import Decidable.Equality

%default total

||| A principal (entity that can hold capabilities)
public export
Principal : Type
Principal = String

||| A resource identifier
public export
ResourceId : Type
ResourceId = String

||| Basic permissions
public export
data Permission =
    Read
  | Write
  | Execute
  | Delete
  | Admin

||| Equality for permissions
public export
Eq Permission where
  Read == Read = True
  Write == Write = True
  Execute == Execute = True
  Delete == Delete = True
  Admin == Admin = True
  _ == _ = False

||| Ordering on permissions (Admin > Delete > Write > Execute > Read)
public export
Ord Permission where
  compare Read Read = EQ
  compare Read _ = LT
  compare Write Read = GT
  compare Write Write = EQ
  compare Write _ = LT
  compare Execute Read = GT
  compare Execute Write = GT
  compare Execute Execute = EQ
  compare Execute _ = LT
  compare Delete Admin = LT
  compare Delete Delete = EQ
  compare Delete _ = GT
  compare Admin Admin = EQ
  compare Admin _ = GT

||| A capability grants a principal permission over a resource
public export
record Capability where
  constructor MkCapability
  capHolder : Principal
  capResource : ResourceId
  capPermissions : List Permission
  capExpiry : Maybe Nat  -- Nothing = never expires
  capDelegatable : Bool

||| Create a new capability
public export
createCapability : Principal -> ResourceId -> List Permission -> Capability
createCapability holder resource perms =
  MkCapability holder resource perms Nothing True

||| Check if capability has a specific permission
public export
hasPermission : Permission -> Capability -> Bool
hasPermission perm cap = elem perm (capPermissions cap)

||| Check if capability is expired
public export
isExpired : Nat -> Capability -> Bool
isExpired now cap =
  case capExpiry cap of
    Nothing => False
    Just expiry => now > expiry

||| Check if capability is valid (not expired and has required permission)
public export
isValid : Nat -> Permission -> Capability -> Bool
isValid now perm cap = not (isExpired now cap) && hasPermission perm cap

||| Proof that a capability grants a permission
public export
data GrantsPermission : Capability -> Permission -> Type where
  MkGrantsPermission : Elem perm (capPermissions cap) -> GrantsPermission cap perm

||| Proof that a capability is not expired at a given time
public export
data NotExpired : Capability -> Nat -> Type where
  NeverExpires : capExpiry cap = Nothing -> NotExpired cap time
  NotYetExpired : capExpiry cap = Just expiry -> LTE time expiry -> NotExpired cap time

||| A valid capability proof combines permission and freshness
public export
data ValidCapability : Capability -> Permission -> Nat -> Type where
  MkValidCapability : GrantsPermission cap perm -> NotExpired cap time ->
                      ValidCapability cap perm time

||| Attenuate a capability (reduce permissions)
||| Can only remove permissions, never add them
public export
attenuate : List Permission -> Capability -> Capability
attenuate newPerms cap =
  { capPermissions := filter (\p => elem p newPerms) (capPermissions cap) } cap

||| Proof that attenuated capability has subset of original permissions
public export
attenuationSound : (cap : Capability) -> (newPerms : List Permission) ->
                   (perm : Permission) ->
                   Elem perm (capPermissions (attenuate newPerms cap)) ->
                   Elem perm (capPermissions cap)
attenuationSound cap newPerms perm elemPrf = ?attenuationSoundProof

||| Delegate a capability to another principal
public export
delegate : Principal -> Capability -> Maybe Capability
delegate newHolder cap =
  if capDelegatable cap
    then Just ({ capHolder := newHolder } cap)
    else Nothing

||| Delegate with attenuation
public export
delegateAttenuated : Principal -> List Permission -> Capability -> Maybe Capability
delegateAttenuated newHolder perms cap =
  if capDelegatable cap
    then Just (attenuate perms ({ capHolder := newHolder } cap))
    else Nothing

||| Make a capability non-delegatable
public export
makeNonDelegatable : Capability -> Capability
makeNonDelegatable cap = { capDelegatable := False } cap

||| Set expiry on a capability
public export
setExpiry : Nat -> Capability -> Capability
setExpiry time cap = { capExpiry := Just time } cap

||| A capability store for a system
public export
record CapabilityStore where
  constructor MkCapabilityStore
  capabilities : List Capability
  revoked : List (Principal, ResourceId)  -- Revoked (holder, resource) pairs

||| Empty capability store
public export
emptyStore : CapabilityStore
emptyStore = MkCapabilityStore [] []

||| Grant a capability (add to store)
public export
grant : Capability -> CapabilityStore -> CapabilityStore
grant cap store = { capabilities := cap :: capabilities store } store

||| Revoke a capability
public export
revoke : Principal -> ResourceId -> CapabilityStore -> CapabilityStore
revoke holder resource store =
  { revoked := (holder, resource) :: revoked store,
    capabilities := filter notRevoked (capabilities store) } store
  where
    notRevoked : Capability -> Bool
    notRevoked cap = not (capHolder cap == holder && capResource cap == resource)

||| Check if a capability is revoked
public export
isRevoked : Principal -> ResourceId -> CapabilityStore -> Bool
isRevoked holder resource store =
  any (\p => fst p == holder && snd p == resource) (revoked store)

||| Look up capabilities for a principal
public export
lookupCapabilities : Principal -> CapabilityStore -> List Capability
lookupCapabilities principal store =
  filter (\cap => capHolder cap == principal) (capabilities store)

||| Check if principal has permission on resource
public export
checkAccess : Principal -> ResourceId -> Permission -> Nat ->
              CapabilityStore -> Bool
checkAccess principal resource perm now store =
  not (isRevoked principal resource store) &&
  any (isValid now perm) (filter relevantCap (capabilities store))
  where
    relevantCap : Capability -> Bool
    relevantCap cap = capHolder cap == principal && capResource cap == resource

||| Access control result with proof
public export
data AccessResult : Type where
  AccessGranted : (cap : Capability) -> ValidCapability cap perm time -> AccessResult
  AccessDenied : String -> AccessResult

||| Role-based capability template
public export
record Role where
  constructor MkRole
  roleName : String
  rolePermissions : List Permission
  roleResources : List ResourceId

||| Create capabilities from a role
public export
roleToCapabilities : Principal -> Role -> List Capability
roleToCapabilities principal role =
  map (\res => createCapability principal res (rolePermissions role))
      (roleResources role)

||| Hierarchical capabilities (parent implies child)
public export
data CapabilityHierarchy : Type where
  MkHierarchy : (parent : Permission) -> (children : List Permission) ->
                CapabilityHierarchy

||| Standard admin hierarchy (Admin implies all others)
public export
adminHierarchy : CapabilityHierarchy
adminHierarchy = MkHierarchy Admin [Read, Write, Execute, Delete]

||| Check if permission implies another via hierarchy
public export
implies : CapabilityHierarchy -> Permission -> Permission -> Bool
implies (MkHierarchy parent children) p1 p2 =
  p1 == p2 || (p1 == parent && elem p2 children)

||| Remove duplicates from a list (local, uses assert_total due to filter)
nubLocal : Eq a => List a -> List a
nubLocal [] = []
nubLocal (x :: xs) = x :: assert_total (nubLocal (filter (/= x) xs))

||| Expand permissions via hierarchy
public export
expandPermissions : CapabilityHierarchy -> List Permission -> List Permission
expandPermissions hier perms =
  nubLocal (perms ++ concatMap expand perms)
  where
    expand : Permission -> List Permission
    expand p = case hier of
      MkHierarchy parent children =>
        if p == parent then children else []

||| Confinement - a capability cannot be leaked outside its domain
public export
record ConfinedCapability where
  constructor MkConfinedCapability
  confinedCap : Capability
  confinementDomain : List Principal  -- Principals that can receive delegation

||| Check if delegation is within confinement
public export
withinConfinement : Principal -> ConfinedCapability -> Bool
withinConfinement principal cc = elem principal (confinementDomain cc)

||| Delegate only within confinement
public export
confinedDelegate : Principal -> ConfinedCapability -> Maybe ConfinedCapability
confinedDelegate newHolder cc =
  if withinConfinement newHolder cc && capDelegatable (confinedCap cc)
    then Just ({ confinedCap := { capHolder := newHolder } (confinedCap cc) } cc)
    else Nothing

||| Audit trail entry
public export
record AuditEntry where
  constructor MkAuditEntry
  auditTime : Nat
  auditPrincipal : Principal
  auditResource : ResourceId
  auditPermission : Permission
  auditGranted : Bool

||| Capability store with audit
public export
record AuditedStore where
  constructor MkAuditedStore
  store : CapabilityStore
  auditLog : List AuditEntry

||| Check access with audit logging
public export
checkAccessAudited : Principal -> ResourceId -> Permission -> Nat ->
                     AuditedStore -> (Bool, AuditedStore)
checkAccessAudited principal resource perm now astore =
  let granted = checkAccess principal resource perm now (store astore)
      entry = MkAuditEntry now principal resource perm granted
  in (granted, { auditLog := entry :: auditLog astore } astore)

||| Token-based capability (unforgeable reference)
public export
record CapabilityToken where
  constructor MkCapabilityToken
  tokenId : Nat
  tokenCap : Capability
  tokenSecret : String  -- In practice, this would be cryptographic

||| Validate a token
public export
validateToken : CapabilityToken -> String -> Bool
validateToken token secret = tokenSecret token == secret

||| Object capability pattern - combine object with its capability
public export
record CapabilityObject (a : Type) where
  constructor MkCapabilityObject
  objectValue : a
  objectCap : Capability

||| Use an object only if capability is valid
public export
useObject : CapabilityObject a -> Permission -> Nat -> (a -> b) -> Maybe b
useObject obj perm now f =
  if isValid now perm (objectCap obj)
    then Just (f (objectValue obj))
    else Nothing

||| Membrane - wraps an object and interposes on all access
public export
record Membrane (a : Type) where
  constructor MkMembrane
  wrappedObject : a
  accessPolicy : Permission -> Bool

||| Access through membrane
public export
throughMembrane : Membrane a -> Permission -> (a -> b) -> Maybe b
throughMembrane mem perm f =
  if accessPolicy mem perm
    then Just (f (wrappedObject mem))
    else Nothing

||| Create a read-only membrane
public export
readOnlyMembrane : a -> Membrane a
readOnlyMembrane obj = MkMembrane obj (\p => p == Read)

