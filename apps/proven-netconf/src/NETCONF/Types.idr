-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for NETCONF (RFC 6241).
||| All types are closed sum types with Show instances.
module NETCONF.Types

%default total

---------------------------------------------------------------------------
-- NETCONF Operation (RFC 6241 Section 7)
---------------------------------------------------------------------------

||| NETCONF RPC operations.
public export
data Operation : Type where
  Get            : Operation
  GetConfig      : Operation
  EditConfig     : Operation
  CopyConfig     : Operation
  DeleteConfig   : Operation
  Lock           : Operation
  Unlock         : Operation
  CloseSession   : Operation
  KillSession    : Operation
  Commit         : Operation
  Validate       : Operation
  DiscardChanges : Operation

public export
Show Operation where
  show Get            = "get"
  show GetConfig      = "get-config"
  show EditConfig     = "edit-config"
  show CopyConfig     = "copy-config"
  show DeleteConfig   = "delete-config"
  show Lock           = "lock"
  show Unlock         = "unlock"
  show CloseSession   = "close-session"
  show KillSession    = "kill-session"
  show Commit         = "commit"
  show Validate       = "validate"
  show DiscardChanges = "discard-changes"

---------------------------------------------------------------------------
-- Datastore (RFC 6241 Section 5.1)
---------------------------------------------------------------------------

||| NETCONF configuration datastores.
public export
data Datastore : Type where
  Running   : Datastore
  Startup   : Datastore
  Candidate : Datastore

public export
Show Datastore where
  show Running   = "running"
  show Startup   = "startup"
  show Candidate = "candidate"

---------------------------------------------------------------------------
-- Edit Operation (RFC 6241 Section 7.2)
---------------------------------------------------------------------------

||| Operations for edit-config.
public export
data EditOperation : Type where
  Merge   : EditOperation
  Replace : EditOperation
  Create  : EditOperation
  Delete  : EditOperation
  Remove  : EditOperation

public export
Show EditOperation where
  show Merge   = "merge"
  show Replace = "replace"
  show Create  = "create"
  show Delete  = "delete"
  show Remove  = "remove"

---------------------------------------------------------------------------
-- Error Type (RFC 6241 Section 4.3)
---------------------------------------------------------------------------

||| NETCONF error type layer.
public export
data ErrorType : Type where
  Transport   : ErrorType
  RPC         : ErrorType
  Protocol    : ErrorType
  Application : ErrorType

public export
Show ErrorType where
  show Transport   = "transport"
  show RPC         = "rpc"
  show Protocol    = "protocol"
  show Application = "application"

---------------------------------------------------------------------------
-- Error Severity (RFC 6241 Section 4.3)
---------------------------------------------------------------------------

||| NETCONF error severity level.
public export
data ErrorSeverity : Type where
  Error   : ErrorSeverity
  Warning : ErrorSeverity

public export
Show ErrorSeverity where
  show Error   = "error"
  show Warning = "warning"
