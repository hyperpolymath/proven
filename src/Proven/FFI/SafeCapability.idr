-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCapability operations
|||
||| This module exports capability-based security helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - Permission → Int (Read=0, Write=1, Execute=2, Delete=3, Admin=4)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Count → Int (capabilities, permissions)
|||
||| CRITICAL: Capability-based security prevents confused deputy attacks
|||           by bundling authority with references (unforgeable).
|||
||| Capability Model:
||| - Capability = (Principal, Resource, Permissions, Expiry, Delegatable)
||| - Principal: Entity that holds the capability
||| - Resource: Object being accessed
||| - Permissions: List of allowed operations
||| - Expiry: Optional time limit (Nothing = never expires)
||| - Delegatable: Can be transferred to another principal
|||
||| Permission Hierarchy:
||| - Admin (4): Highest privilege, implies all others
||| - Delete (3): Can delete resources
||| - Write (2): Can modify resources
||| - Execute (1): Can execute/run resources
||| - Read (0): Can read resources
||| - Admin > Delete > Write > Execute > Read
|||
||| Core Operations:
||| - hasPermission: Check if capability grants permission
||| - isExpired: Check if capability expired
||| - isValid: Not expired AND has permission
||| - attenuate: Reduce permissions (can only remove, not add)
||| - delegate: Transfer to another principal
||| - revoke: Invalidate capability
|||
||| Security Properties:
||| - Attenuation: Derived capabilities have ≤ permissions
||| - Confinement: Capabilities can't escape their domain
||| - Revocation: Can be invalidated at any time
||| - Expiry: Temporal validity limits
|||
||| Advanced Features:
||| - Role-based templates (Role → Capabilities)
||| - Hierarchical permissions (Admin implies all)
||| - Confined delegation (within domain only)
||| - Audit logging (track all access)
||| - Tokens (unforgeable references)
||| - Membranes (access interposition)
module Proven.FFI.SafeCapability

import Proven.SafeCapability
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Permission Encoding
--------------------------------------------------------------------------------

export
proven_idris_cap_perm_read : Int
proven_idris_cap_perm_read = 0

export
proven_idris_cap_perm_write : Int
proven_idris_cap_perm_write = 1

export
proven_idris_cap_perm_execute : Int
proven_idris_cap_perm_execute = 2

export
proven_idris_cap_perm_delete : Int
proven_idris_cap_perm_delete = 3

export
proven_idris_cap_perm_admin : Int
proven_idris_cap_perm_admin = 4

export
proven_idris_cap_is_valid_permission : Int -> Int
proven_idris_cap_is_valid_permission perm =
  encodeBool (perm >= 0 && perm <= 4)

--------------------------------------------------------------------------------
-- Permission Hierarchy
--------------------------------------------------------------------------------

export
proven_idris_cap_perm_implies : Int -> Int -> Int
proven_idris_cap_perm_implies perm1 perm2 =
  -- Admin implies all others
  if perm1 == 4 then 1  -- Admin implies everything
  else if perm1 == perm2 then 1  -- Same permission
  else if perm1 > perm2 then 1  -- Higher permission implies lower
  else 0

export
proven_idris_cap_perm_compare : Int -> Int -> Int
proven_idris_cap_perm_compare perm1 perm2 =
  if perm1 < perm2 then (-1)
  else if perm1 == perm2 then 0
  else 1

export
proven_idris_cap_perm_max : Int -> Int -> Int
proven_idris_cap_perm_max perm1 perm2 =
  if perm1 > perm2 then perm1 else perm2

export
proven_idris_cap_perm_min : Int -> Int -> Int
proven_idris_cap_perm_min perm1 perm2 =
  if perm1 < perm2 then perm1 else perm2

export
proven_idris_cap_is_admin : Int -> Int
proven_idris_cap_is_admin perm =
  encodeBool (perm == 4)

export
proven_idris_cap_requires_admin : Int -> Int
proven_idris_cap_requires_admin perm =
  encodeBool (perm == 4 || perm == 3)  -- Admin or Delete

--------------------------------------------------------------------------------
-- Capability Validation
--------------------------------------------------------------------------------

export
proven_idris_cap_has_permission : Int -> Int
proven_idris_cap_has_permission hasIt = hasIt

export
proven_idris_cap_is_expired : Int -> Int -> Int
proven_idris_cap_is_expired currentTime expiryTime =
  encodeBool (currentTime > expiryTime)

export
proven_idris_cap_never_expires : Int -> Int
proven_idris_cap_never_expires hasExpiry =
  encodeBool (hasExpiry == 0)

export
proven_idris_cap_is_valid : Int -> Int -> Int -> Int
proven_idris_cap_is_valid hasPermission isExpired neverExpires =
  encodeBool (hasPermission == 1 && (isExpired == 0 || neverExpires == 1))

export
proven_idris_cap_remaining_time : Int -> Int -> Int
proven_idris_cap_remaining_time currentTime expiryTime =
  if currentTime >= expiryTime then 0
  else expiryTime - currentTime

export
proven_idris_cap_is_nearly_expired : Int -> Int -> Int -> Int
proven_idris_cap_is_nearly_expired currentTime expiryTime threshold =
  let remaining = proven_idris_cap_remaining_time currentTime expiryTime
  in encodeBool (remaining <= threshold && remaining > 0)

--------------------------------------------------------------------------------
-- Delegation
--------------------------------------------------------------------------------

export
proven_idris_cap_is_delegatable : Int -> Int
proven_idris_cap_is_delegatable delegatable = delegatable

export
proven_idris_cap_can_delegate : Int -> Int -> Int
proven_idris_cap_can_delegate delegatable isExpired =
  encodeBool (delegatable == 1 && isExpired == 0)

export
proven_idris_cap_delegation_depth : Int -> Int
proven_idris_cap_delegation_depth depth = depth

export
proven_idris_cap_max_delegation_depth : Int
proven_idris_cap_max_delegation_depth = 10

export
proven_idris_cap_can_delegate_further : Int -> Int -> Int
proven_idris_cap_can_delegate_further currentDepth maxDepth =
  encodeBool (currentDepth < maxDepth)

--------------------------------------------------------------------------------
-- Attenuation
--------------------------------------------------------------------------------

export
proven_idris_cap_can_attenuate_to : Int -> Int -> Int
proven_idris_cap_can_attenuate_to currentPerm newPerm =
  -- Can only reduce permissions (newPerm ≤ currentPerm)
  encodeBool (newPerm <= currentPerm)

export
proven_idris_cap_attenuation_valid : Int -> Int -> Int
proven_idris_cap_attenuation_valid originalPerms attenuatedPerms =
  -- Attenuated permissions must be subset of original
  encodeBool (attenuatedPerms <= originalPerms)

export
proven_idris_cap_permissions_removed : Int -> Int -> Int
proven_idris_cap_permissions_removed originalCount attenuatedCount =
  if originalCount >= attenuatedCount
    then originalCount - attenuatedCount
    else 0

--------------------------------------------------------------------------------
-- Revocation
--------------------------------------------------------------------------------

export
proven_idris_cap_is_revoked : Int -> Int
proven_idris_cap_is_revoked revoked = revoked

export
proven_idris_cap_can_revoke : Int -> Int
proven_idris_cap_can_revoke isAdmin = isAdmin

export
proven_idris_cap_revocation_count : Int -> Int
proven_idris_cap_revocation_count count = count

--------------------------------------------------------------------------------
-- Confinement
--------------------------------------------------------------------------------

export
proven_idris_cap_within_confinement : Int -> Int
proven_idris_cap_within_confinement withinDomain = withinDomain

export
proven_idris_cap_can_confine : Int -> Int
proven_idris_cap_can_confine canConfine = canConfine

export
proven_idris_cap_confinement_domain_size : Int -> Int
proven_idris_cap_confinement_domain_size size = size

export
proven_idris_cap_is_confined : Int -> Int
proven_idris_cap_is_confined hasConfinement = hasConfinement

--------------------------------------------------------------------------------
-- Store Operations
--------------------------------------------------------------------------------

export
proven_idris_cap_store_size : Int -> Int
proven_idris_cap_store_size count = count

export
proven_idris_cap_store_capacity : Int -> Int
proven_idris_cap_store_capacity maxSize = maxSize

export
proven_idris_cap_store_is_full : Int -> Int -> Int
proven_idris_cap_store_is_full currentSize maxSize =
  encodeBool (currentSize >= maxSize)

export
proven_idris_cap_store_remaining_capacity : Int -> Int -> Int
proven_idris_cap_store_remaining_capacity currentSize maxSize =
  if currentSize >= maxSize then 0
  else maxSize - currentSize

export
proven_idris_cap_lookup_count : Int -> Int
proven_idris_cap_lookup_count count = count

--------------------------------------------------------------------------------
-- Access Control
--------------------------------------------------------------------------------

export
proven_idris_cap_access_granted : Int -> Int
proven_idris_cap_access_granted granted = granted

export
proven_idris_cap_access_denied : Int -> Int
proven_idris_cap_access_denied denied = denied

export
proven_idris_cap_check_access : Int -> Int -> Int -> Int
proven_idris_cap_check_access hasPermission notExpired notRevoked =
  encodeBool (hasPermission == 1 && notExpired == 1 && notRevoked == 1)

export
proven_idris_cap_denial_reason : Int -> String
proven_idris_cap_denial_reason reason =
  if reason == 0 then "No permission"
  else if reason == 1 then "Expired"
  else if reason == 2 then "Revoked"
  else if reason == 3 then "Not delegatable"
  else if reason == 4 then "Outside confinement"
  else "Unknown denial reason"

--------------------------------------------------------------------------------
-- Audit Logging
--------------------------------------------------------------------------------

export
proven_idris_cap_audit_entry_count : Int -> Int
proven_idris_cap_audit_entry_count count = count

export
proven_idris_cap_access_success_rate : Int -> Int -> Double
proven_idris_cap_access_success_rate granted total =
  if total == 0 then 0.0
  else cast granted / cast total

export
proven_idris_cap_access_success_rate_percent : Int -> Int -> Double
proven_idris_cap_access_success_rate_percent granted total =
  proven_idris_cap_access_success_rate granted total * 100.0

export
proven_idris_cap_denial_rate : Int -> Int -> Double
proven_idris_cap_denial_rate denied total =
  if total == 0 then 0.0
  else cast denied / cast total

export
proven_idris_cap_denial_rate_percent : Int -> Int -> Double
proven_idris_cap_denial_rate_percent denied total =
  proven_idris_cap_denial_rate denied total * 100.0

--------------------------------------------------------------------------------
-- Token Validation
--------------------------------------------------------------------------------

export
proven_idris_cap_token_is_valid : Int -> Int
proven_idris_cap_token_is_valid valid = valid

export
proven_idris_cap_token_secret_matches : Int -> Int
proven_idris_cap_token_secret_matches matches = matches

export
proven_idris_cap_generate_token_id : Int -> Int
proven_idris_cap_generate_token_id currentId = currentId + 1

--------------------------------------------------------------------------------
-- Object Capabilities
--------------------------------------------------------------------------------

export
proven_idris_cap_object_has_access : Int -> Int -> Int -> Int
proven_idris_cap_object_has_access hasPermission notExpired notRevoked =
  proven_idris_cap_check_access hasPermission notExpired notRevoked

export
proven_idris_cap_membrane_allows : Int -> Int
proven_idris_cap_membrane_allows allows = allows

export
proven_idris_cap_membrane_is_readonly : Int -> Int -> Int
proven_idris_cap_membrane_is_readonly requestedPerm _ =
  encodeBool (requestedPerm == 0)  -- Read only

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

export
proven_idris_cap_delegation_count : Int -> Int
proven_idris_cap_delegation_count count = count

export
proven_idris_cap_attenuation_count : Int -> Int
proven_idris_cap_attenuation_count count = count

export
proven_idris_cap_average_lifetime : Int -> Int -> Double
proven_idris_cap_average_lifetime totalLifetime capCount =
  if capCount == 0 then 0.0
  else cast totalLifetime / cast capCount

export
proven_idris_cap_average_delegation_depth : Int -> Int -> Double
proven_idris_cap_average_delegation_depth totalDepth capCount =
  if capCount == 0 then 0.0
  else cast totalDepth / cast capCount

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_cap_default_expiry : Int
proven_idris_cap_default_expiry = 3600  -- 1 hour

export
proven_idris_cap_max_store_size : Int
proven_idris_cap_max_store_size = 100000

export
proven_idris_cap_expiry_warning_threshold : Int
proven_idris_cap_expiry_warning_threshold = 300  -- 5 minutes

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_cap_friendly_error : String -> String
proven_idris_cap_friendly_error errorMsg =
  if isInfixOf "permission" (toLower errorMsg) || isInfixOf "access" (toLower errorMsg)
    then "Access denied (insufficient permissions)"
  else if isInfixOf "expired" (toLower errorMsg) || isInfixOf "expiry" (toLower errorMsg)
    then "Capability expired (no longer valid)"
  else if isInfixOf "revoked" (toLower errorMsg) || isInfixOf "revocation" (toLower errorMsg)
    then "Capability revoked (access denied)"
  else if isInfixOf "delegate" (toLower errorMsg) || isInfixOf "delegation" (toLower errorMsg)
    then "Delegation failed (not delegatable or expired)"
  else if isInfixOf "confinement" (toLower errorMsg) || isInfixOf "domain" (toLower errorMsg)
    then "Confinement violation (outside authorized domain)"
  else if isInfixOf "attenuation" (toLower errorMsg)
    then "Attenuation failed (cannot add permissions)"
  else
    "Capability error"

export
proven_idris_cap_permission_description : Int -> String
proven_idris_cap_permission_description perm =
  if perm == 0 then "Read (view resources)"
  else if perm == 1 then "Write (modify resources)"
  else if perm == 2 then "Execute (run resources)"
  else if perm == 3 then "Delete (remove resources)"
  else if perm == 4 then "Admin (full control, implies all permissions)"
  else "Unknown permission"
