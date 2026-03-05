-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core mDNS/DNS-SD protocol types as closed sum types.
-- | Models DNS record types, query modes (RFC 6762 Section 5),
-- | conflict resolution actions (Section 9), and service flags.
module MDNS.Types

%default total

-------------------------------------------------------------------------------
-- DNS Record Types
-------------------------------------------------------------------------------

||| DNS record types relevant to mDNS/DNS-SD.
public export
data RecordType : Type where
  A    : RecordType
  AAAA : RecordType
  PTR  : RecordType
  SRV  : RecordType
  TXT  : RecordType

||| Show instance for RecordType.
export
Show RecordType where
  show A    = "A"
  show AAAA = "AAAA"
  show PTR  = "PTR"
  show SRV  = "SRV"
  show TXT  = "TXT"

-------------------------------------------------------------------------------
-- Query Types
-------------------------------------------------------------------------------

||| mDNS query modes as described in RFC 6762 Section 5.
public export
data QueryType : Type where
  Standard   : QueryType
  OneShot    : QueryType
  Continuous : QueryType

||| Show instance for QueryType.
export
Show QueryType where
  show Standard   = "Standard"
  show OneShot    = "OneShot"
  show Continuous = "Continuous"

-------------------------------------------------------------------------------
-- Conflict Actions
-------------------------------------------------------------------------------

||| Conflict resolution actions for mDNS name conflicts (RFC 6762 Section 9).
public export
data ConflictAction : Type where
  Probe    : ConflictAction
  Defend   : ConflictAction
  Withdraw : ConflictAction

||| Show instance for ConflictAction.
export
Show ConflictAction where
  show Probe    = "Probe"
  show Defend   = "Defend"
  show Withdraw = "Withdraw"

-------------------------------------------------------------------------------
-- Service Flags
-------------------------------------------------------------------------------

||| Service registration flags for mDNS records.
||| Unique records require probing; Shared records do not.
public export
data ServiceFlag : Type where
  Unique : ServiceFlag
  Shared : ServiceFlag

||| Show instance for ServiceFlag.
export
Show ServiceFlag where
  show Unique = "Unique"
  show Shared = "Shared"
