-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 8915 Network Time Security.
-- | Defines NTS-KE record types, error codes, AEAD algorithms,
-- | and handshake states as closed sum types with Show instances.

module NTS.Types

%default total

||| NTS Key Establishment record types per RFC 8915 Section 4.
public export
data RecordType : Type where
  EndOfMessage      : RecordType
  NextProtocol      : RecordType
  Error             : RecordType
  Warning           : RecordType
  AEADAlgorithm     : RecordType
  Cookie            : RecordType
  CookiePlaceholder : RecordType
  NTSKEServer       : RecordType
  NTSKEPort         : RecordType

public export
Show RecordType where
  show EndOfMessage      = "EndOfMessage"
  show NextProtocol      = "NextProtocol"
  show Error             = "Error"
  show Warning           = "Warning"
  show AEADAlgorithm     = "AEADAlgorithm"
  show Cookie            = "Cookie"
  show CookiePlaceholder = "CookiePlaceholder"
  show NTSKEServer       = "NTSKEServer"
  show NTSKEPort         = "NTSKEPort"

||| NTS-KE error codes per RFC 8915 Section 4.1.3.
public export
data ErrorCode : Type where
  UnrecognizedCritical : ErrorCode
  BadRequest           : ErrorCode
  InternalError        : ErrorCode

public export
Show ErrorCode where
  show UnrecognizedCritical = "UnrecognizedCritical"
  show BadRequest           = "BadRequest"
  show InternalError        = "InternalError"

||| AEAD algorithms supported by NTS per RFC 8915 Section 4.1.5.
public export
data AEADAlgorithm : Type where
  AEAD_AES_128_GCM       : AEADAlgorithm
  AEAD_AES_256_GCM       : AEADAlgorithm
  AEAD_AES_SIV_CMAC_256  : AEADAlgorithm

public export
Show AEADAlgorithm where
  show AEAD_AES_128_GCM      = "AEAD_AES_128_GCM"
  show AEAD_AES_256_GCM      = "AEAD_AES_256_GCM"
  show AEAD_AES_SIV_CMAC_256 = "AEAD_AES_SIV_CMAC_256"

||| NTS-KE handshake state machine.
public export
data HandshakeState : Type where
  Initial      : HandshakeState
  Negotiating  : HandshakeState
  Established  : HandshakeState
  Failed       : HandshakeState

public export
Show HandshakeState where
  show Initial     = "Initial"
  show Negotiating = "Negotiating"
  show Established = "Established"
  show Failed      = "Failed"
