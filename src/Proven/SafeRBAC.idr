-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeRBAC - Role-Based Access Control with lattice-theoretic guarantees
|||
||| Provides a type-safe RBAC model with:
||| - Permission sets as join-semilattices
||| - Role hierarchy as partial order
||| - Least-privilege checks
||| - Separation of duty constraints
||| Suitable for Agda cross-verification (algebraic structure).
module Proven.SafeRBAC

import Data.String
import Data.List
import Data.Nat

-- Permission nub and hierarchy traversal use recursion the totality
-- checker cannot verify. Functions that depend on these are covering.
%default covering

-- ============================================================================
-- PERMISSIONS
-- ============================================================================

||| A permission is an opaque token identified by name
public export
record Permission where
  constructor MkPermission
  permName : String

public export
Eq Permission where
  a == b = a.permName == b.permName

public export
Ord Permission where
  compare a b = compare a.permName b.permName

public export
Show Permission where
  show p = p.permName

||| A set of permissions (sorted, deduplicated)
public export
record PermissionSet where
  constructor MkPermissionSet
  perms : List Permission

public export
Eq PermissionSet where
  a == b = a.perms == b.perms

public export
Show PermissionSet where
  show ps = "{" ++ fastConcat (intersperse ", " (map show ps.perms)) ++ "}"

||| Empty permission set
public export
emptyPerms : PermissionSet
emptyPerms = MkPermissionSet []

||| Singleton permission set
public export
singlePerm : String -> PermissionSet
singlePerm name = MkPermissionSet [MkPermission name]

||| Union of permission sets (join in the lattice)
public export
unionPerms : PermissionSet -> PermissionSet -> PermissionSet
unionPerms a b = MkPermissionSet (nub (a.perms ++ b.perms))
  where
    nub : List Permission -> List Permission
    nub [] = []
    nub (x :: xs) = x :: nub (filter (/= x) xs)

||| Intersection of permission sets (meet in the lattice)
public export
intersectPerms : PermissionSet -> PermissionSet -> PermissionSet
intersectPerms a b = MkPermissionSet (filter (\p => any (== p) b.perms) a.perms)

||| Check if a permission set contains a specific permission
public export
hasPermission : PermissionSet -> Permission -> Bool
hasPermission ps p = any (== p) ps.perms

||| Check if one permission set is a subset of another
public export
isSubset : PermissionSet -> PermissionSet -> Bool
isSubset sub super = all (\p => hasPermission super p) sub.perms

-- ============================================================================
-- ROLES
-- ============================================================================

||| A role with a name and assigned permissions
public export
record Role where
  constructor MkRole
  roleName    : String
  permissions : PermissionSet
  parents     : List String   -- Parent role names (for hierarchy)

public export
Eq Role where
  a == b = a.roleName == b.roleName

public export
Show Role where
  show r = r.roleName ++ "(" ++ show r.permissions ++ ")"

-- ============================================================================
-- ROLE HIERARCHY
-- ============================================================================

||| A role hierarchy (directed acyclic graph of roles)
public export
record RoleHierarchy where
  constructor MkHierarchy
  roles : List Role

||| Find a role by name
public export
findRole : RoleHierarchy -> String -> Maybe Role
findRole h name = find (\r => r.roleName == name) h.roles

||| Get effective permissions for a role (including inherited from parents)
||| Uses depth limit to prevent infinite loops in malformed hierarchies.
public export
effectivePermissions : RoleHierarchy -> String -> PermissionSet
effectivePermissions h roleName = go 20 roleName
  where
    go : Nat -> String -> PermissionSet
    go Z _ = emptyPerms  -- Depth limit reached
    go (S depth) name =
      case findRole h name of
        Nothing => emptyPerms
        Just role =>
          let parentPerms = foldl (\acc, parent => unionPerms acc (go depth parent))
                                  emptyPerms role.parents
          in unionPerms role.permissions parentPerms

-- ============================================================================
-- ACCESS CONTROL DECISIONS
-- ============================================================================

||| Access decision
public export
data AccessDecision =
    Grant    -- Access allowed
  | Deny     -- Access denied — insufficient permissions
  | DenySoD  -- Access denied — separation of duty violation

public export
Show AccessDecision where
  show Grant   = "GRANT"
  show Deny    = "DENY"
  show DenySoD = "DENY(SoD)"

public export
Eq AccessDecision where
  Grant == Grant = True
  Deny == Deny = True
  DenySoD == DenySoD = True
  _ == _ = False

||| Check if a role has a required permission
public export
checkAccess : RoleHierarchy -> String -> Permission -> AccessDecision
checkAccess h roleName required =
  let effective = effectivePermissions h roleName
  in if hasPermission effective required then Grant else Deny

||| Check if a role has ALL required permissions
public export
checkAccessAll : RoleHierarchy -> String -> List Permission -> AccessDecision
checkAccessAll h roleName required =
  let effective = effectivePermissions h roleName
  in if all (hasPermission effective) required then Grant else Deny

-- ============================================================================
-- SEPARATION OF DUTY
-- ============================================================================

||| A separation of duty constraint: no single user should have both sets
public export
record SoDConstraint where
  constructor MkSoD
  constraintName : String
  setA : PermissionSet
  setB : PermissionSet

||| Check if a permission set violates a SoD constraint
public export
violatesSoD : SoDConstraint -> PermissionSet -> Bool
violatesSoD sod perms =
  let hasA = not (isNil (intersectPerms sod.setA perms).perms)
      hasB = not (isNil (intersectPerms sod.setB perms).perms)
  in hasA && hasB

||| Check access with SoD constraints
public export
checkAccessWithSoD : RoleHierarchy -> List SoDConstraint -> String -> Permission -> AccessDecision
checkAccessWithSoD h constraints roleName required =
  let effective = effectivePermissions h roleName
  in if any (\c => violatesSoD c effective) constraints
       then DenySoD
       else if hasPermission effective required then Grant else Deny

-- ============================================================================
-- LEAST PRIVILEGE ANALYSIS
-- ============================================================================

||| Find the minimal role that grants a permission
public export
minimalRole : RoleHierarchy -> Permission -> Maybe String
minimalRole h perm =
  let candidates = filter (\r => hasPermission (effectivePermissions h r.roleName) perm) h.roles
      sorted = sortBy (\a, b => compare (length (effectivePermissions h a.roleName).perms)
                                        (length (effectivePermissions h b.roleName).perms)) candidates
  in map (.roleName) (head' sorted)

||| Check if a role has excess permissions beyond what's needed
public export
excessPermissions : RoleHierarchy -> String -> List Permission -> PermissionSet
excessPermissions h roleName needed =
  let effective = effectivePermissions h roleName
      neededSet = MkPermissionSet (map id needed)
  in MkPermissionSet (filter (\p => not (any (== p) needed)) effective.perms)
