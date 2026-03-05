-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- OCSP.Types : Core types for the RFC 6960 OCSP responder.
-- Defines certificate status responses, OCSP response statuses,
-- and hash algorithm identifiers.

module OCSP.Types

%default total

---------------------------------------------------------------------------
-- CertStatus : OCSP certificate status (RFC 6960 Section 2.2).
---------------------------------------------------------------------------

||| Certificate status returned in an OCSP response.
public export
data CertStatus : Type where
  Good    : CertStatus
  Revoked : CertStatus
  Unknown : CertStatus

export
Show CertStatus where
  show Good    = "Good"
  show Revoked = "Revoked"
  show Unknown = "Unknown"

---------------------------------------------------------------------------
-- ResponseStatus : OCSP response-level status codes (RFC 6960 Section 2.3).
---------------------------------------------------------------------------

||| Status of the OCSP response itself (not the certificate).
public export
data ResponseStatus : Type where
  Successful       : ResponseStatus
  MalformedRequest : ResponseStatus
  InternalError    : ResponseStatus
  TryLater         : ResponseStatus
  SigRequired      : ResponseStatus
  Unauthorized     : ResponseStatus

export
Show ResponseStatus where
  show Successful       = "Successful"
  show MalformedRequest = "MalformedRequest"
  show InternalError    = "InternalError"
  show TryLater         = "TryLater"
  show SigRequired      = "SigRequired"
  show Unauthorized     = "Unauthorized"

---------------------------------------------------------------------------
-- HashAlgorithm : Hash algorithms for CertID computation.
---------------------------------------------------------------------------

||| Hash algorithms used in OCSP CertID construction.
public export
data HashAlgorithm : Type where
  SHA1   : HashAlgorithm
  SHA256 : HashAlgorithm
  SHA384 : HashAlgorithm
  SHA512 : HashAlgorithm

export
Show HashAlgorithm where
  show SHA1   = "SHA-1"
  show SHA256 = "SHA-256"
  show SHA384 = "SHA-384"
  show SHA512 = "SHA-512"
