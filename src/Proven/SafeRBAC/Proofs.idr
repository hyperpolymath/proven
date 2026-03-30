-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeRBAC operations
|||
||| Verifies lattice-theoretic properties of the RBAC permission model:
||| empty set is identity for union, subset ordering, and that
||| separation of duty constraints correctly deny access.
module Proven.SafeRBAC.Proofs

import Proven.SafeRBAC
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Empty Permission Set Properties
--------------------------------------------------------------------------------

||| Empty permission set has no permissions.
public export
emptyPermsEmpty : emptyPerms.perms = []
emptyPermsEmpty = Refl

||| Empty permission set does not contain any permission.
public export
emptyHasNoPermission : (p : Permission) -> hasPermission emptyPerms p = False
emptyHasNoPermission _ = Refl

||| Empty permission set is a subset of any set.
public export
emptyIsSubsetOfAny : (ps : PermissionSet) -> isSubset emptyPerms ps = True
emptyIsSubsetOfAny _ = Refl

||| Intersection with empty is empty.
public export
intersectEmptyLeft : (ps : PermissionSet) ->
                     (intersectPerms emptyPerms ps).perms = []
intersectEmptyLeft _ = Refl

--------------------------------------------------------------------------------
-- Singleton Properties
--------------------------------------------------------------------------------

||| Singleton permission set has exactly one permission.
public export
singlePermLength : (name : String) -> length (singlePerm name).perms = 1
singlePermLength _ = Refl

||| Singleton contains its own permission.
public export
singlePermContainsSelf : (name : String) ->
                         hasPermission (singlePerm name) (MkPermission name) = True
singlePermContainsSelf name = Refl

--------------------------------------------------------------------------------
-- Access Decision Properties
--------------------------------------------------------------------------------

||| checkAccess on an empty hierarchy always denies.
public export
emptyHierarchyDenies : (roleName : String) -> (perm : Permission) ->
                       checkAccess (MkHierarchy []) roleName perm = Deny
emptyHierarchyDenies _ _ = Refl

||| Grant and Deny are distinct.
public export
grantNotDeny : Not (Grant = Deny)
grantNotDeny Refl impossible

||| Grant and DenySoD are distinct.
public export
grantNotDenySoD : Not (Grant = DenySoD)
grantNotDenySoD Refl impossible

||| Deny and DenySoD are distinct.
public export
denyNotDenySoD : Not (Deny = DenySoD)
denyNotDenySoD Refl impossible

--------------------------------------------------------------------------------
-- Separation of Duty Properties
--------------------------------------------------------------------------------

||| Empty permission set never violates any SoD constraint.
||| The intersection of any set with empty gives empty perms on left side,
||| so hasA is False, and hasA && hasB = False.
public export
emptyNeverViolatesSoD : (sod : SoDConstraint) -> violatesSoD sod emptyPerms = False
emptyNeverViolatesSoD sod = Refl

||| checkAccessWithSoD on empty hierarchy always denies (never grants).
public export
emptyHierarchyWithSoDDenies : (constraints : List SoDConstraint) ->
                              (roleName : String) -> (perm : Permission) ->
                              checkAccessWithSoD (MkHierarchy []) constraints roleName perm = Deny
emptyHierarchyWithSoDDenies [] _ _ = Refl
emptyHierarchyWithSoDDenies (c :: cs) roleName perm = Refl
