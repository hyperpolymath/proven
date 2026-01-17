// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCapability - Capability-based security tokens that cannot crash.
 *
 * Implements capability-based security tokens following the principle of
 * least privilege. Capabilities are unforgeable tokens that grant specific
 * permissions to resources. All operations fail safely rather than expose
 * security vulnerabilities.
 */

/** Capability error types */
type capabilityError =
  | InvalidToken
  | ExpiredCapability
  | InsufficientPermissions
  | InvalidResource
  | InvalidSignature
  | TokenTooLong
  | TokenTooShort
  | MalformedToken
  | CapabilityRevoked
  | DelegationNotAllowed
  | MaxDelegationDepthExceeded

/** Permission flags for capabilities */
type permission = {
  read: bool,
  write: bool,
  execute: bool,
  delete: bool,
  admin: bool,
  delegate: bool,
  revoke: bool,
}

/** No permissions */
let noPermission: permission = {
  read: false,
  write: false,
  execute: false,
  delete: false,
  admin: false,
  delegate: false,
  revoke: false,
}

/** Read-only permission */
let readOnlyPermission: permission = {
  ...noPermission,
  read: true,
}

/** Read and write permissions */
let readWritePermission: permission = {
  ...noPermission,
  read: true,
  write: true,
}

/** Full permissions including admin */
let fullPermission: permission = {
  read: true,
  write: true,
  execute: true,
  delete: true,
  admin: true,
  delegate: true,
  revoke: true,
}

/** Check if one permission set includes another */
let permissionIncludes = (self: permission, other: permission): bool => {
  (!other.read || self.read) &&
  (!other.write || self.write) &&
  (!other.execute || self.execute) &&
  (!other.delete || self.delete) &&
  (!other.admin || self.admin) &&
  (!other.delegate || self.delegate) &&
  (!other.revoke || self.revoke)
}

/** Combine two permission sets (union) */
let permissionCombine = (a: permission, b: permission): permission => {
  read: a.read || b.read,
  write: a.write || b.write,
  execute: a.execute || b.execute,
  delete: a.delete || b.delete,
  admin: a.admin || b.admin,
  delegate: a.delegate || b.delegate,
  revoke: a.revoke || b.revoke,
}

/** Intersect two permission sets */
let permissionIntersect = (a: permission, b: permission): permission => {
  read: a.read && b.read,
  write: a.write && b.write,
  execute: a.execute && b.execute,
  delete: a.delete && b.delete,
  admin: a.admin && b.admin,
  delegate: a.delegate && b.delegate,
  revoke: a.revoke && b.revoke,
}

/** Convert permission to byte representation */
let permissionToByte = (p: permission): int => {
  let result = ref(0)
  if p.read {
    result := result.contents lor 1
  }
  if p.write {
    result := result.contents lor 2
  }
  if p.execute {
    result := result.contents lor 4
  }
  if p.delete {
    result := result.contents lor 8
  }
  if p.admin {
    result := result.contents lor 16
  }
  if p.delegate {
    result := result.contents lor 32
  }
  if p.revoke {
    result := result.contents lor 64
  }
  result.contents
}

/** Create permission from byte representation */
let permissionFromByte = (byte: int): permission => {
  read: byte land 1 != 0,
  write: byte land 2 != 0,
  execute: byte land 4 != 0,
  delete: byte land 8 != 0,
  admin: byte land 16 != 0,
  delegate: byte land 32 != 0,
  revoke: byte land 64 != 0,
}

/** Check if any permission is set */
let hasAnyPermission = (p: permission): bool => {
  p.read || p.write || p.execute || p.delete || p.admin || p.delegate || p.revoke
}

/** Check if no permissions are set */
let hasNoPermission = (p: permission): bool => {
  !hasAnyPermission(p)
}

/** Resource identifier for capabilities */
type resourceId = {
  namespace: string,
  identifier: string,
}

/** Create a resource ID from namespace and identifier strings */
let makeResourceId = (namespace: string, identifier: string): resourceId => {
  // Truncate to max lengths
  let ns = Js.String2.slice(namespace, ~from=0, ~to_=16)
  let id = Js.String2.slice(identifier, ~from=0, ~to_=32)
  {namespace: ns, identifier: id}
}

/** Create a wildcard resource matching all in a namespace */
let makeWildcardResource = (namespace: string): resourceId => {
  makeResourceId(namespace, "*")
}

/** Check if a resource matches another (with wildcard support) */
let resourceMatches = (pattern: resourceId, target: resourceId): bool => {
  if pattern.namespace != target.namespace {
    false
  } else if pattern.identifier == "*" {
    true
  } else {
    pattern.identifier == target.identifier
  }
}

/** Check if two resources are equal */
let resourceEquals = (a: resourceId, b: resourceId): bool => {
  a.namespace == b.namespace && a.identifier == b.identifier
}

/** A capability token granting permissions on a resource */
type capability = {
  id: string,
  resource: resourceId,
  permissions: permission,
  createdAt: float,
  expiresAt: float,
  delegationDepth: int,
  maxDelegationDepth: int,
  revoked: bool,
  signature: string,
}

/** Check if capability is expired */
let isExpired = (cap: capability): bool => {
  if cap.expiresAt == 0.0 {
    false
  } else {
    Js.Date.now() > cap.expiresAt *. 1000.0
  }
}

/** Check if capability is valid (not expired or revoked) */
let isValid = (cap: capability): bool => {
  !cap.revoked && !isExpired(cap)
}

/** Check if capability grants a specific permission */
let hasPermission = (cap: capability, perm: permission): bool => {
  permissionIncludes(cap.permissions, perm)
}

/** Check if capability can be delegated */
let canDelegate = (cap: capability): bool => {
  cap.permissions.delegate && cap.delegationDepth < cap.maxDelegationDepth
}

/** Get remaining time in seconds (0 if expired or no expiry) */
let remainingTime = (cap: capability): float => {
  if cap.expiresAt == 0.0 {
    0.0
  } else {
    let now = Js.Date.now() /. 1000.0
    let remaining = cap.expiresAt -. now
    if remaining > 0.0 {
      remaining
    } else {
      0.0
    }
  }
}

/** Generate a random hex string */
let generateRandomId = (): string => {
  let chars = "0123456789abcdef"
  let result = ref("")
  for _ in 0 to 31 {
    let idx = Js.Math.floor_int(Js.Math.random() *. 16.0)
    result := result.contents ++ Js.String2.charAt(chars, idx)
  }
  result.contents
}

/** Generate a random signature placeholder */
let generateSignature = (): string => {
  let chars = "0123456789abcdef"
  let result = ref("")
  for _ in 0 to 63 {
    let idx = Js.Math.floor_int(Js.Math.random() *. 16.0)
    result := result.contents ++ Js.String2.charAt(chars, idx)
  }
  result.contents
}

/** Capability factory configuration */
type factoryConfig = {
  defaultExpirySeconds: float,
  defaultMaxDelegation: int,
}

/** Default factory configuration */
let defaultFactoryConfig: factoryConfig = {
  defaultExpirySeconds: 3600.0,
  defaultMaxDelegation: 3,
}

/** Create a new capability */
let createCapability = (
  ~resource: resourceId,
  ~permissions: permission,
  ~expirySeconds: float=3600.0,
  ~maxDelegation: int=3,
): capability => {
  let now = Js.Date.now() /. 1000.0
  {
    id: generateRandomId(),
    resource,
    permissions,
    createdAt: now,
    expiresAt: if expirySeconds > 0.0 {
      now +. expirySeconds
    } else {
      0.0
    },
    delegationDepth: 0,
    maxDelegationDepth: maxDelegation,
    revoked: false,
    signature: generateSignature(),
  }
}

/** Delegate a capability with reduced permissions */
let delegateCapability = (
  parent: capability,
  newPermissions: permission,
): result<capability, capabilityError> => {
  if !isValid(parent) {
    Error(ExpiredCapability)
  } else if !canDelegate(parent) {
    Error(DelegationNotAllowed)
  } else if !permissionIncludes(parent.permissions, newPermissions) {
    Error(InsufficientPermissions)
  } else {
    let now = Js.Date.now() /. 1000.0
    Ok({
      id: generateRandomId(),
      resource: parent.resource,
      permissions: permissionIntersect(parent.permissions, newPermissions),
      createdAt: now,
      expiresAt: parent.expiresAt,
      delegationDepth: parent.delegationDepth + 1,
      maxDelegationDepth: parent.maxDelegationDepth,
      revoked: false,
      signature: generateSignature(),
    })
  }
}

/** Validate a capability for a specific resource and permission */
let validateCapability = (
  cap: capability,
  resource: resourceId,
  requiredPermission: permission,
): result<unit, capabilityError> => {
  if cap.revoked {
    Error(CapabilityRevoked)
  } else if isExpired(cap) {
    Error(ExpiredCapability)
  } else if !resourceMatches(cap.resource, resource) {
    Error(InvalidResource)
  } else if !hasPermission(cap, requiredPermission) {
    Error(InsufficientPermissions)
  } else {
    Ok()
  }
}

/** Check access (convenience function that returns bool) */
let checkAccess = (
  cap: capability,
  resource: resourceId,
  requiredPermission: permission,
): bool => {
  switch validateCapability(cap, resource, requiredPermission) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Format capability ID as a short display string */
let formatCapabilityId = (cap: capability): string => {
  Js.String2.slice(cap.id, ~from=0, ~to_=8) ++ "..."
}

/** Serialize capability to JSON string */
let serializeCapability = (cap: capability): string => {
  let json = Js.Dict.empty()
  Js.Dict.set(json, "id", Js.Json.string(cap.id))
  Js.Dict.set(json, "namespace", Js.Json.string(cap.resource.namespace))
  Js.Dict.set(json, "identifier", Js.Json.string(cap.resource.identifier))
  Js.Dict.set(json, "permissions", Js.Json.number(Belt.Int.toFloat(permissionToByte(cap.permissions))))
  Js.Dict.set(json, "createdAt", Js.Json.number(cap.createdAt))
  Js.Dict.set(json, "expiresAt", Js.Json.number(cap.expiresAt))
  Js.Dict.set(json, "delegationDepth", Js.Json.number(Belt.Int.toFloat(cap.delegationDepth)))
  Js.Dict.set(json, "maxDelegationDepth", Js.Json.number(Belt.Int.toFloat(cap.maxDelegationDepth)))
  Js.Dict.set(json, "revoked", Js.Json.boolean(cap.revoked))
  Js.Dict.set(json, "signature", Js.Json.string(cap.signature))
  Js.Json.stringify(Js.Json.object_(json))
}

/** Deserialize capability from JSON string */
let deserializeCapability = (jsonString: string): result<capability, capabilityError> => {
  try {
    let json = Js.Json.parseExn(jsonString)
    switch Js.Json.classify(json) {
    | Js.Json.JSONObject(obj) =>
      let getString = key =>
        switch Js.Dict.get(obj, key) {
        | Some(v) =>
          switch Js.Json.classify(v) {
          | Js.Json.JSONString(s) => Some(s)
          | _ => None
          }
        | None => None
        }
      let getNumber = key =>
        switch Js.Dict.get(obj, key) {
        | Some(v) =>
          switch Js.Json.classify(v) {
          | Js.Json.JSONNumber(n) => Some(n)
          | _ => None
          }
        | None => None
        }
      let getBool = key =>
        switch Js.Dict.get(obj, key) {
        | Some(v) =>
          switch Js.Json.classify(v) {
          | Js.Json.JSONTrue => Some(true)
          | Js.Json.JSONFalse => Some(false)
          | _ => None
          }
        | None => None
        }

      switch (
        getString("id"),
        getString("namespace"),
        getString("identifier"),
        getNumber("permissions"),
        getNumber("createdAt"),
        getNumber("expiresAt"),
        getNumber("delegationDepth"),
        getNumber("maxDelegationDepth"),
        getBool("revoked"),
        getString("signature"),
      ) {
      | (
          Some(id),
          Some(ns),
          Some(ident),
          Some(perms),
          Some(created),
          Some(expires),
          Some(depth),
          Some(maxDepth),
          Some(revoked),
          Some(sig),
        ) =>
        Ok({
          id,
          resource: {namespace: ns, identifier: ident},
          permissions: permissionFromByte(Belt.Float.toInt(perms)),
          createdAt: created,
          expiresAt: expires,
          delegationDepth: Belt.Float.toInt(depth),
          maxDelegationDepth: Belt.Float.toInt(maxDepth),
          revoked,
          signature: sig,
        })
      | _ => Error(MalformedToken)
      }
    | _ => Error(MalformedToken)
    }
  } catch {
  | _ => Error(MalformedToken)
  }
}

/** Permission set builder */
module PermissionBuilder = {
  let empty = noPermission
  let withRead = (p: permission): permission => {...p, read: true}
  let withWrite = (p: permission): permission => {...p, write: true}
  let withExecute = (p: permission): permission => {...p, execute: true}
  let withDelete = (p: permission): permission => {...p, delete: true}
  let withAdmin = (p: permission): permission => {...p, admin: true}
  let withDelegate = (p: permission): permission => {...p, delegate: true}
  let withRevoke = (p: permission): permission => {...p, revoke: true}
}
