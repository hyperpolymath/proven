-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 4918 WebDAV.
-- | Defines WebDAV methods, status codes, lock scopes, lock types,
-- | depth values, and property operations as closed sum types with
-- | Show instances.

module WebDAV.Types

%default total

||| WebDAV-specific HTTP methods per RFC 4918 Section 9.
public export
data Method : Type where
  Propfind  : Method
  Proppatch : Method
  Mkcol     : Method
  Copy      : Method
  Move      : Method
  Lock      : Method
  Unlock    : Method

public export
Show Method where
  show Propfind  = "Propfind"
  show Proppatch = "Proppatch"
  show Mkcol     = "Mkcol"
  show Copy      = "Copy"
  show Move      = "Move"
  show Lock      = "Lock"
  show Unlock    = "Unlock"

||| WebDAV-specific HTTP status codes per RFC 4918 Section 11.
public export
data StatusCode : Type where
  MultiStatus            : StatusCode
  UnprocessableEntity    : StatusCode
  Locked                 : StatusCode
  FailedDependency       : StatusCode
  InsufficientStorage    : StatusCode

public export
Show StatusCode where
  show MultiStatus         = "MultiStatus"
  show UnprocessableEntity = "UnprocessableEntity"
  show Locked              = "Locked"
  show FailedDependency    = "FailedDependency"
  show InsufficientStorage = "InsufficientStorage"

||| WebDAV lock scope per RFC 4918 Section 14.13.
public export
data LockScope : Type where
  Exclusive : LockScope
  Shared    : LockScope

public export
Show LockScope where
  show Exclusive = "Exclusive"
  show Shared    = "Shared"

||| WebDAV lock type per RFC 4918 Section 14.15.
public export
data LockType : Type where
  Write : LockType

public export
Show LockType where
  show Write = "Write"

||| WebDAV Depth header values per RFC 4918 Section 10.2.
public export
data Depth : Type where
  Zero     : Depth
  One      : Depth
  Infinity : Depth

public export
Show Depth where
  show Zero     = "Zero"
  show One      = "One"
  show Infinity = "Infinity"

||| WebDAV property update operations per RFC 4918 Section 14.23.
public export
data PropertyOp : Type where
  Set    : PropertyOp
  Remove : PropertyOp

public export
Show PropertyOp where
  show Set    = "Set"
  show Remove = "Remove"
