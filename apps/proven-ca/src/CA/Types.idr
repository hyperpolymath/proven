-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- CA.Types : Core types for the Certificate Authority server.
-- Defines certificate types, key algorithms, certificate statuses,
-- revocation reasons, and X.509 extensions.

module CA.Types

%default total

---------------------------------------------------------------------------
-- CertType : Types of X.509 certificates the CA can issue.
---------------------------------------------------------------------------

||| Certificate purpose classifications.
public export
data CertType : Type where
  Root            : CertType
  Intermediate    : CertType
  EndEntity       : CertType
  CodeSigning     : CertType
  EmailProtection : CertType
  OCSPSigning     : CertType

export
Show CertType where
  show Root            = "Root"
  show Intermediate    = "Intermediate"
  show EndEntity       = "EndEntity"
  show CodeSigning     = "CodeSigning"
  show EmailProtection = "EmailProtection"
  show OCSPSigning     = "OCSPSigning"

---------------------------------------------------------------------------
-- KeyAlgorithm : Cryptographic algorithms for key generation.
---------------------------------------------------------------------------

||| Supported key generation algorithms and their parameters.
public export
data KeyAlgorithm : Type where
  RSA2048    : KeyAlgorithm
  RSA4096    : KeyAlgorithm
  ECDSA_P256 : KeyAlgorithm
  ECDSA_P384 : KeyAlgorithm
  Ed25519    : KeyAlgorithm
  Ed448      : KeyAlgorithm

export
Show KeyAlgorithm where
  show RSA2048    = "RSA-2048"
  show RSA4096    = "RSA-4096"
  show ECDSA_P256 = "ECDSA-P256"
  show ECDSA_P384 = "ECDSA-P384"
  show Ed25519    = "Ed25519"
  show Ed448      = "Ed448"

---------------------------------------------------------------------------
-- CertStatus : Current state of a certificate in its lifecycle.
---------------------------------------------------------------------------

||| Lifecycle status of an issued certificate.
public export
data CertStatus : Type where
  Valid     : CertStatus
  Revoked   : CertStatus
  Expired   : CertStatus
  Suspended : CertStatus

export
Show CertStatus where
  show Valid     = "Valid"
  show Revoked   = "Revoked"
  show Expired   = "Expired"
  show Suspended = "Suspended"

---------------------------------------------------------------------------
-- RevocationReason : RFC 5280 CRL reason codes.
---------------------------------------------------------------------------

||| Reason for revoking a certificate, per RFC 5280 Section 5.3.1.
public export
data RevocationReason : Type where
  Unspecified          : RevocationReason
  KeyCompromise        : RevocationReason
  CACompromise         : RevocationReason
  AffiliationChanged   : RevocationReason
  Superseded           : RevocationReason
  CessationOfOperation : RevocationReason
  CertificateHold      : RevocationReason

export
Show RevocationReason where
  show Unspecified          = "Unspecified"
  show KeyCompromise        = "KeyCompromise"
  show CACompromise         = "CACompromise"
  show AffiliationChanged   = "AffiliationChanged"
  show Superseded           = "Superseded"
  show CessationOfOperation = "CessationOfOperation"
  show CertificateHold      = "CertificateHold"

---------------------------------------------------------------------------
-- Extension : X.509v3 certificate extensions.
---------------------------------------------------------------------------

||| X.509v3 extensions the CA can include in issued certificates.
public export
data Extension : Type where
  BasicConstraints        : Extension
  KeyUsage                : Extension
  ExtKeyUsage             : Extension
  SubjectAltName          : Extension
  AuthorityInfoAccess     : Extension
  CRLDistributionPoints   : Extension

export
Show Extension where
  show BasicConstraints      = "BasicConstraints"
  show KeyUsage              = "KeyUsage"
  show ExtKeyUsage           = "ExtKeyUsage"
  show SubjectAltName        = "SubjectAltName"
  show AuthorityInfoAccess   = "AuthorityInfoAccess"
  show CRLDistributionPoints = "CRLDistributionPoints"
