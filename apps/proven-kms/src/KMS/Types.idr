-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- KMS.Types : Core types for the Key Management Server.
-- Defines managed object types, KMIP-style operations,
-- key lifecycle states, and cryptographic algorithms.

module KMS.Types

%default total

---------------------------------------------------------------------------
-- ObjectType : Types of cryptographic objects managed by the KMS.
---------------------------------------------------------------------------

||| Cryptographic objects the KMS can store and manage.
public export
data ObjectType : Type where
  SymmetricKey : ObjectType
  PublicKey    : ObjectType
  PrivateKey   : ObjectType
  SecretData   : ObjectType
  Certificate  : ObjectType
  OpaqueData   : ObjectType

export
Show ObjectType where
  show SymmetricKey = "SymmetricKey"
  show PublicKey    = "PublicKey"
  show PrivateKey   = "PrivateKey"
  show SecretData   = "SecretData"
  show Certificate  = "Certificate"
  show OpaqueData   = "OpaqueData"

---------------------------------------------------------------------------
-- Operation : KMIP-style operations the KMS can perform.
---------------------------------------------------------------------------

||| Operations supported by the key management protocol.
public export
data Operation : Type where
  Create   : Operation
  Get      : Operation
  Activate : Operation
  Revoke   : Operation
  Destroy  : Operation
  Locate   : Operation
  Register : Operation
  Rekey    : Operation
  Encrypt  : Operation
  Decrypt  : Operation
  Sign     : Operation
  Verify   : Operation
  Wrap     : Operation
  Unwrap   : Operation
  MAC      : Operation

export
Show Operation where
  show Create   = "Create"
  show Get      = "Get"
  show Activate = "Activate"
  show Revoke   = "Revoke"
  show Destroy  = "Destroy"
  show Locate   = "Locate"
  show Register = "Register"
  show Rekey    = "Rekey"
  show Encrypt  = "Encrypt"
  show Decrypt  = "Decrypt"
  show Sign     = "Sign"
  show Verify   = "Verify"
  show Wrap     = "Wrap"
  show Unwrap   = "Unwrap"
  show MAC      = "MAC"

---------------------------------------------------------------------------
-- KeyState : KMIP key lifecycle states.
---------------------------------------------------------------------------

||| Lifecycle state of a managed cryptographic key.
public export
data KeyState : Type where
  PreActive            : KeyState
  Active               : KeyState
  Deactivated          : KeyState
  Compromised          : KeyState
  Destroyed            : KeyState
  DestroyedCompromised : KeyState

export
Show KeyState where
  show PreActive            = "PreActive"
  show Active               = "Active"
  show Deactivated          = "Deactivated"
  show Compromised          = "Compromised"
  show Destroyed            = "Destroyed"
  show DestroyedCompromised = "DestroyedCompromised"

---------------------------------------------------------------------------
-- Algorithm : Cryptographic algorithms supported by the KMS.
---------------------------------------------------------------------------

||| Cryptographic algorithms available for key operations.
public export
data Algorithm : Type where
  AES128           : Algorithm
  AES256           : Algorithm
  RSA2048          : Algorithm
  RSA4096          : Algorithm
  ECDSA_P256       : Algorithm
  ECDSA_P384       : Algorithm
  Ed25519          : Algorithm
  ChaCha20Poly1305 : Algorithm
  HMAC_SHA256      : Algorithm

export
Show Algorithm where
  show AES128           = "AES-128"
  show AES256           = "AES-256"
  show RSA2048          = "RSA-2048"
  show RSA4096          = "RSA-4096"
  show ECDSA_P256       = "ECDSA-P256"
  show ECDSA_P384       = "ECDSA-P384"
  show Ed25519          = "Ed25519"
  show ChaCha20Poly1305 = "ChaCha20-Poly1305"
  show HMAC_SHA256      = "HMAC-SHA256"
