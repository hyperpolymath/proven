-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SNMP.Types: Core protocol types for SNMP (RFC 3411 architecture).
--
-- Defines closed sum types for SNMP protocol versions, PDU types (the 7
-- PDU types across SNMPv1/v2c/v3), and error status codes from the
-- Protocol Operations specification (RFC 3416).

module SNMP.Types

%default total

-- ============================================================================
-- SNMP versions
-- ============================================================================

||| SNMP protocol versions.
||| Each version has distinct security and PDU capabilities.
public export
data Version : Type where
  ||| SNMPv1: community-based, limited error reporting (RFC 1157).
  V1  : Version
  ||| SNMPv2c: community-based with enhanced PDU types (RFC 3584).
  V2c : Version
  ||| SNMPv3: user-based security model with encryption (RFC 3414).
  V3  : Version

public export
Eq Version where
  V1  == V1  = True
  V2c == V2c = True
  V3  == V3  = True
  _   == _   = False

public export
Show Version where
  show V1  = "SNMPv1"
  show V2c = "SNMPv2c"
  show V3  = "SNMPv3"

-- ============================================================================
-- SNMP PDU types (RFC 3416)
-- ============================================================================

||| SNMP Protocol Data Unit types.
||| GetRequest, GetNextRequest, GetResponse, and SetRequest are available in
||| all versions. GetBulkRequest, InformRequest, and SNMPv2Trap are v2c/v3 only.
public export
data PDUType : Type where
  ||| Retrieve the value of one or more OIDs (all versions).
  GetRequest     : PDUType
  ||| Retrieve the next OID in lexicographic order (all versions).
  GetNextRequest : PDUType
  ||| Response to a Get/Set/GetNext/GetBulk request (all versions).
  GetResponse    : PDUType
  ||| Set the value of one or more OIDs (all versions).
  SetRequest     : PDUType
  ||| Retrieve a large table efficiently (v2c/v3 only).
  GetBulkRequest : PDUType
  ||| Manager-to-manager notification with acknowledgement (v2c/v3 only).
  InformRequest  : PDUType
  ||| Asynchronous notification from agent to manager (v2c/v3 only).
  SNMPv2Trap     : PDUType

public export
Eq PDUType where
  GetRequest     == GetRequest     = True
  GetNextRequest == GetNextRequest = True
  GetResponse    == GetResponse    = True
  SetRequest     == SetRequest     = True
  GetBulkRequest == GetBulkRequest = True
  InformRequest  == InformRequest  = True
  SNMPv2Trap     == SNMPv2Trap     = True
  _              == _              = False

public export
Show PDUType where
  show GetRequest     = "GetRequest"
  show GetNextRequest = "GetNextRequest"
  show GetResponse    = "GetResponse"
  show SetRequest     = "SetRequest"
  show GetBulkRequest = "GetBulkRequest"
  show InformRequest  = "InformRequest"
  show SNMPv2Trap     = "SNMPv2-Trap"

-- ============================================================================
-- SNMP error status codes (RFC 3416 Section 3)
-- ============================================================================

||| Error status codes from RFC 3416 Section 3.
||| Returned in GetResponse PDUs to indicate the outcome of a request.
||| Codes 0-5 are available in all versions; codes 6-18 are v2c/v3 only.
public export
data ErrorStatus : Type where
  ||| No error occurred (0).
  NoError              : ErrorStatus
  ||| Response would be too large to transport (1).
  TooBig               : ErrorStatus
  ||| Requested OID does not exist (2, v1 only; replaced by exceptions in v2c).
  NoSuchName           : ErrorStatus
  ||| Value has incorrect type or is out of range (3).
  BadValue             : ErrorStatus
  ||| Variable is read-only and cannot be set (4).
  ReadOnly             : ErrorStatus
  ||| An unspecified error occurred (5).
  GenErr               : ErrorStatus
  ||| Access denied for the requested operation (6, v2c/v3).
  NoAccess             : ErrorStatus
  ||| Value is of the wrong ASN.1 type (7, v2c/v3).
  WrongType            : ErrorStatus
  ||| Value has incorrect length (8, v2c/v3).
  WrongLength          : ErrorStatus
  ||| Value is outside the acceptable range (10, v2c/v3).
  WrongValue           : ErrorStatus
  ||| Row creation is not permitted (11, v2c/v3).
  NoCreation           : ErrorStatus
  ||| Value is inconsistent with other managed object state (12, v2c/v3).
  InconsistentValue    : ErrorStatus
  ||| Resource is temporarily unavailable (13, v2c/v3).
  ResourceUnavailable  : ErrorStatus
  ||| Set operation could not be committed (14, v2c/v3).
  CommitFailed         : ErrorStatus
  ||| Set operation could not be rolled back (15, v2c/v3).
  UndoFailed           : ErrorStatus
  ||| SNMP entity lacks authorization (16, v2c/v3).
  AuthorizationError   : ErrorStatus

public export
Eq ErrorStatus where
  NoError              == NoError              = True
  TooBig               == TooBig               = True
  NoSuchName           == NoSuchName           = True
  BadValue             == BadValue             = True
  ReadOnly             == ReadOnly             = True
  GenErr               == GenErr               = True
  NoAccess             == NoAccess             = True
  WrongType            == WrongType            = True
  WrongLength          == WrongLength          = True
  WrongValue           == WrongValue           = True
  NoCreation           == NoCreation           = True
  InconsistentValue    == InconsistentValue    = True
  ResourceUnavailable  == ResourceUnavailable  = True
  CommitFailed         == CommitFailed         = True
  UndoFailed           == UndoFailed           = True
  AuthorizationError   == AuthorizationError   = True
  _                    == _                    = False

public export
Show ErrorStatus where
  show NoError              = "noError(0)"
  show TooBig               = "tooBig(1)"
  show NoSuchName           = "noSuchName(2)"
  show BadValue             = "badValue(3)"
  show ReadOnly             = "readOnly(4)"
  show GenErr               = "genErr(5)"
  show NoAccess             = "noAccess(6)"
  show WrongType            = "wrongType(7)"
  show WrongLength          = "wrongLength(8)"
  show WrongValue           = "wrongValue(10)"
  show NoCreation           = "noCreation(11)"
  show InconsistentValue    = "inconsistentValue(12)"
  show ResourceUnavailable  = "resourceUnavailable(13)"
  show CommitFailed         = "commitFailed(14)"
  show UndoFailed           = "undoFailed(15)"
  show AuthorizationError   = "authorizationError(16)"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All SNMP versions.
public export
allVersions : List Version
allVersions = [V1, V2c, V3]

||| All PDU types.
public export
allPDUTypes : List PDUType
allPDUTypes = [GetRequest, GetNextRequest, GetResponse, SetRequest,
               GetBulkRequest, InformRequest, SNMPv2Trap]

||| All error status codes.
public export
allErrorStatuses : List ErrorStatus
allErrorStatuses = [NoError, TooBig, NoSuchName, BadValue, ReadOnly, GenErr,
                    NoAccess, WrongType, WrongLength, WrongValue, NoCreation,
                    InconsistentValue, ResourceUnavailable, CommitFailed,
                    UndoFailed, AuthorizationError]
