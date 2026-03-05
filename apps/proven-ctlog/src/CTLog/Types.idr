-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for Certificate Transparency Log (RFC 6962).
||| All types are closed sum types with Show instances.
module CTLog.Types

%default total

---------------------------------------------------------------------------
-- Log Entry Type (RFC 6962 Section 3.1)
---------------------------------------------------------------------------

||| Types of entries that can appear in a CT log.
public export
data LogEntryType : Type where
  X509Entry    : LogEntryType
  PrecertEntry : LogEntryType

public export
Show LogEntryType where
  show X509Entry    = "X509Entry"
  show PrecertEntry = "PrecertEntry"

---------------------------------------------------------------------------
-- Signature Type (RFC 6962 Section 3.2)
---------------------------------------------------------------------------

||| Types of CT log signatures.
public export
data SignatureType : Type where
  CertificateTimestamp : SignatureType
  TreeHash             : SignatureType

public export
Show SignatureType where
  show CertificateTimestamp = "certificate_timestamp"
  show TreeHash             = "tree_hash"

---------------------------------------------------------------------------
-- Merkle Leaf Type
---------------------------------------------------------------------------

||| Merkle tree leaf types.
public export
data MerkleLeafType : Type where
  TimestampedEntry : MerkleLeafType

public export
Show MerkleLeafType where
  show TimestampedEntry = "timestamped_entry"

---------------------------------------------------------------------------
-- Submission Status
---------------------------------------------------------------------------

||| Status of a certificate submission to the CT log.
public export
data SubmissionStatus : Type where
  Accepted     : SubmissionStatus
  Duplicate    : SubmissionStatus
  RateLimited  : SubmissionStatus
  Rejected     : SubmissionStatus
  InvalidChain : SubmissionStatus
  UnknownAnchor : SubmissionStatus

public export
Show SubmissionStatus where
  show Accepted      = "Accepted"
  show Duplicate     = "Duplicate"
  show RateLimited   = "Rate Limited"
  show Rejected      = "Rejected"
  show InvalidChain  = "Invalid Chain"
  show UnknownAnchor = "Unknown Anchor"

---------------------------------------------------------------------------
-- Verification Result
---------------------------------------------------------------------------

||| Result of verifying a CT log proof or consistency check.
public export
data VerificationResult : Type where
  ValidProof       : VerificationResult
  InvalidProof     : VerificationResult
  InconsistentTree : VerificationResult
  StaleSTH         : VerificationResult

public export
Show VerificationResult where
  show ValidProof       = "Valid Proof"
  show InvalidProof     = "Invalid Proof"
  show InconsistentTree = "Inconsistent Tree"
  show StaleSTH         = "Stale STH"
