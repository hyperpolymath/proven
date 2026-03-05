-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core Oblivious DNS types as closed sum types.
-- | Models participant roles, message types, error reasons,
-- | and encapsulation formats per draft-pauly-dprive-oblivious-doh.
module ODNS.Types

%default total

-------------------------------------------------------------------------------
-- Roles
-------------------------------------------------------------------------------

||| Participant roles in the Oblivious DNS architecture.
||| Client sends queries through Proxy to Target, ensuring neither
||| Proxy nor Target can correlate identity with query content.
public export
data Role : Type where
  Client : Role
  Proxy  : Role
  Target : Role

||| Show instance for Role.
export
Show Role where
  show Client = "Client"
  show Proxy  = "Proxy"
  show Target = "Target"

-------------------------------------------------------------------------------
-- Message Types
-------------------------------------------------------------------------------

||| Oblivious DNS message types.
public export
data MessageType : Type where
  Query    : MessageType
  Response : MessageType

||| Show instance for MessageType.
export
Show MessageType where
  show Query    = "Query"
  show Response = "Response"

-------------------------------------------------------------------------------
-- Error Reasons
-------------------------------------------------------------------------------

||| Error reasons specific to Oblivious DNS processing.
public export
data ErrorReason : Type where
  ProxyError       : ErrorReason
  TargetError      : ErrorReason
  DecryptionFailed : ErrorReason
  InvalidConfig    : ErrorReason
  PayloadTooLarge  : ErrorReason

||| Show instance for ErrorReason.
export
Show ErrorReason where
  show ProxyError       = "ProxyError"
  show TargetError      = "TargetError"
  show DecryptionFailed = "DecryptionFailed"
  show InvalidConfig    = "InvalidConfig"
  show PayloadTooLarge  = "PayloadTooLarge"

-------------------------------------------------------------------------------
-- Encapsulation Formats
-------------------------------------------------------------------------------

||| Cryptographic encapsulation format for Oblivious DNS.
||| HPKE (Hybrid Public Key Encryption) is the sole format.
public export
data EncapsulationFormat : Type where
  HPKE : EncapsulationFormat

||| Show instance for EncapsulationFormat.
export
Show EncapsulationFormat where
  show HPKE = "HPKE"
