-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-diode: Core protocol types for data diode (unidirectional gateway).
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Diode.Types

%default total

-- ============================================================================
-- Direction
-- ============================================================================

||| The permitted direction of data flow through the diode.
||| A hardware data diode physically enforces one-way transfer.
public export
data Direction : Type where
  ||| Transfer from high-security to low-security network.
  HighToLow : Direction
  ||| Transfer from low-security to high-security network.
  LowToHigh : Direction

export
Show Direction where
  show HighToLow = "HighToLow"
  show LowToHigh = "LowToHigh"

-- ============================================================================
-- Protocol
-- ============================================================================

||| Network protocols supported for transit across the diode.
public export
data Protocol : Type where
  ||| User Datagram Protocol -- connectionless, natural fit for diodes.
  UDP          : Protocol
  ||| Transmission Control Protocol -- requires proxy reconstruction.
  TCP          : Protocol
  ||| Bulk file transfer with manifest and integrity verification.
  FileTransfer : Protocol
  ||| Syslog forwarding (RFC 5424).
  Syslog       : Protocol
  ||| Simple Network Management Protocol traps (read-only export).
  SNMP         : Protocol

export
Show Protocol where
  show UDP          = "UDP"
  show TCP          = "TCP"
  show FileTransfer = "FileTransfer"
  show Syslog       = "Syslog"
  show SNMP         = "SNMP"

-- ============================================================================
-- TransferState
-- ============================================================================

||| Lifecycle state of a single data segment traversing the diode.
public export
data TransferState : Type where
  ||| Segment is queued for transmission.
  Queued     : TransferState
  ||| Segment is actively being pushed through the diode.
  Sending    : TransferState
  ||| Segment sent; awaiting out-of-band confirmation (if available).
  Confirming : TransferState
  ||| Transfer completed successfully.
  Complete   : TransferState
  ||| Transfer failed -- segment must be re-queued or discarded.
  Failed     : TransferState

export
Show TransferState where
  show Queued     = "Queued"
  show Sending    = "Sending"
  show Confirming = "Confirming"
  show Complete   = "Complete"
  show Failed     = "Failed"

-- ============================================================================
-- ValidationResult
-- ============================================================================

||| Result of validating a data segment before diode transit.
public export
data ValidationResult : Type where
  ||| Segment passes all policy checks.
  Passed        : ValidationResult
  ||| Segment has an unrecognised or malformed format.
  FormatError   : ValidationResult
  ||| Segment exceeds the maximum allowed size.
  SizeExceeded  : ValidationResult
  ||| Segment blocked by security policy rules.
  PolicyBlocked : ValidationResult

export
Show ValidationResult where
  show Passed        = "Passed"
  show FormatError   = "FormatError"
  show SizeExceeded  = "SizeExceeded"
  show PolicyBlocked = "PolicyBlocked"

-- ============================================================================
-- IntegrityCheck
-- ============================================================================

||| Integrity verification algorithm applied to segments.
public export
data IntegrityCheck : Type where
  ||| CRC-32 checksum (fast, non-cryptographic).
  CRC32  : IntegrityCheck
  ||| SHA-256 hash (cryptographic).
  SHA256 : IntegrityCheck
  ||| HMAC with a pre-shared key (authenticated integrity).
  HMAC   : IntegrityCheck

export
Show IntegrityCheck where
  show CRC32  = "CRC32"
  show SHA256 = "SHA256"
  show HMAC   = "HMAC"
