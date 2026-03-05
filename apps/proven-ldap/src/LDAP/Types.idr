-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- LDAP.Types: Core protocol types for LDAPv3 (RFC 4511).
--
-- Defines closed sum types for LDAP operations (the 10 protocol operations
-- from RFC 4511 Section 4), search scopes, and result codes. Result codes
-- cover the most common responses from RFC 4511 Section 4.1.9.

module LDAP.Types

%default total

-- ============================================================================
-- LDAP operations (RFC 4511 Section 4)
-- ============================================================================

||| LDAPv3 protocol operations from RFC 4511 Section 4.
||| Each operation corresponds to a distinct PDU type in the LDAP message
||| envelope.
public export
data Operation : Type where
  ||| Authenticate to the directory (Section 4.2).
  Bind     : Operation
  ||| Terminate the LDAP session (Section 4.3).
  Unbind   : Operation
  ||| Search the directory tree (Section 4.5).
  Search   : Operation
  ||| Modify attributes of an existing entry (Section 4.6).
  Modify   : Operation
  ||| Add a new entry to the directory (Section 4.7).
  Add      : Operation
  ||| Remove an entry from the directory (Section 4.8).
  Delete   : Operation
  ||| Modify the DN (distinguished name) of an entry (Section 4.9).
  ModDN    : Operation
  ||| Compare an assertion value against an entry attribute (Section 4.10).
  Compare  : Operation
  ||| Request the server to abandon an outstanding operation (Section 4.11).
  Abandon  : Operation
  ||| Extended operation for protocol extensions (Section 4.12).
  Extended : Operation

public export
Eq Operation where
  Bind     == Bind     = True
  Unbind   == Unbind   = True
  Search   == Search   = True
  Modify   == Modify   = True
  Add      == Add      = True
  Delete   == Delete   = True
  ModDN    == ModDN    = True
  Compare  == Compare  = True
  Abandon  == Abandon  = True
  Extended == Extended = True
  _        == _        = False

public export
Show Operation where
  show Bind     = "BindRequest"
  show Unbind   = "UnbindRequest"
  show Search   = "SearchRequest"
  show Modify   = "ModifyRequest"
  show Add      = "AddRequest"
  show Delete   = "DelRequest"
  show ModDN    = "ModDNRequest"
  show Compare  = "CompareRequest"
  show Abandon  = "AbandonRequest"
  show Extended = "ExtendedRequest"

-- ============================================================================
-- Search scope (RFC 4511 Section 4.5.1)
-- ============================================================================

||| Search scope values from RFC 4511 Section 4.5.1.
||| Determines how deep into the DIT (Directory Information Tree) a search
||| operation will traverse.
public export
data SearchScope : Type where
  ||| Search only the base object itself (scope 0).
  BaseObject   : SearchScope
  ||| Search only the immediate children of the base (scope 1).
  SingleLevel  : SearchScope
  ||| Search the entire subtree rooted at the base (scope 2).
  WholeSubtree : SearchScope

public export
Eq SearchScope where
  BaseObject   == BaseObject   = True
  SingleLevel  == SingleLevel  = True
  WholeSubtree == WholeSubtree = True
  _            == _            = False

public export
Show SearchScope where
  show BaseObject   = "baseObject"
  show SingleLevel  = "singleLevel"
  show WholeSubtree = "wholeSubtree"

-- ============================================================================
-- Result codes (RFC 4511 Section 4.1.9)
-- ============================================================================

||| Common LDAP result codes from RFC 4511 Section 4.1.9.
||| These are the most frequently encountered result codes; the full
||| IANA registry contains additional values.
public export
data ResultCode : Type where
  ||| Operation completed successfully (0).
  Success                  : ResultCode
  ||| Server encountered an internal error (1).
  OperationsError          : ResultCode
  ||| Request does not comply with the protocol (2).
  ProtocolError            : ResultCode
  ||| Search exceeded the server's time limit (3).
  TimeLimitExceeded        : ResultCode
  ||| Search exceeded the server's size limit (4).
  SizeLimitExceeded        : ResultCode
  ||| Requested authentication method is not supported (7).
  AuthMethodNotSupported   : ResultCode
  ||| The target entry does not exist in the DIT (32).
  NoSuchObject             : ResultCode
  ||| Bind credentials are incorrect (49).
  InvalidCredentials       : ResultCode
  ||| Client lacks permission for the requested operation (50).
  InsufficientAccessRights : ResultCode
  ||| Server is too busy to process the request (51).
  Busy                     : ResultCode
  ||| Server is shutting down or otherwise unavailable (52).
  Unavailable              : ResultCode

public export
Eq ResultCode where
  Success                  == Success                  = True
  OperationsError          == OperationsError          = True
  ProtocolError            == ProtocolError            = True
  TimeLimitExceeded        == TimeLimitExceeded        = True
  SizeLimitExceeded        == SizeLimitExceeded        = True
  AuthMethodNotSupported   == AuthMethodNotSupported   = True
  NoSuchObject             == NoSuchObject             = True
  InvalidCredentials       == InvalidCredentials       = True
  InsufficientAccessRights == InsufficientAccessRights = True
  Busy                     == Busy                     = True
  Unavailable              == Unavailable              = True
  _                        == _                        = False

public export
Show ResultCode where
  show Success                  = "success(0)"
  show OperationsError          = "operationsError(1)"
  show ProtocolError            = "protocolError(2)"
  show TimeLimitExceeded        = "timeLimitExceeded(3)"
  show SizeLimitExceeded        = "sizeLimitExceeded(4)"
  show AuthMethodNotSupported   = "authMethodNotSupported(7)"
  show NoSuchObject             = "noSuchObject(32)"
  show InvalidCredentials       = "invalidCredentials(49)"
  show InsufficientAccessRights = "insufficientAccessRights(50)"
  show Busy                     = "busy(51)"
  show Unavailable              = "unavailable(52)"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All LDAP operations.
public export
allOperations : List Operation
allOperations = [Bind, Unbind, Search, Modify, Add, Delete,
                 ModDN, Compare, Abandon, Extended]

||| All search scopes.
public export
allSearchScopes : List SearchScope
allSearchScopes = [BaseObject, SingleLevel, WholeSubtree]

||| All result codes.
public export
allResultCodes : List ResultCode
allResultCodes = [Success, OperationsError, ProtocolError, TimeLimitExceeded,
                  SizeLimitExceeded, AuthMethodNotSupported, NoSuchObject,
                  InvalidCredentials, InsufficientAccessRights, Busy, Unavailable]
