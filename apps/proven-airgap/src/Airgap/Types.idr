-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Airgap.Types : Core types for the airgapped data transfer gateway.
-- Defines transfer directions, physical media types, scan results,
-- transfer lifecycle states, and validation check types.

module Airgap.Types

%default total

---------------------------------------------------------------------------
-- TransferDirection : Direction of data movement across the air gap.
---------------------------------------------------------------------------

||| Direction of data transfer relative to the airgapped network.
public export
data TransferDirection : Type where
  Import : TransferDirection
  Export : TransferDirection

export
Show TransferDirection where
  show Import = "Import"
  show Export = "Export"

---------------------------------------------------------------------------
-- MediaType : Physical media used for data transfer.
---------------------------------------------------------------------------

||| Physical media types supported for cross-boundary data transfer.
public export
data MediaType : Type where
  USB          : MediaType
  OpticalDisc  : MediaType
  TapeCartridge : MediaType
  DiodeLink    : MediaType

export
Show MediaType where
  show USB           = "USB"
  show OpticalDisc   = "OpticalDisc"
  show TapeCartridge = "TapeCartridge"
  show DiodeLink     = "DiodeLink"

---------------------------------------------------------------------------
-- ScanResult : Content scanning outcome classifications.
---------------------------------------------------------------------------

||| Result of scanning transferred content for threats.
public export
data ScanResult : Type where
  Clean       : ScanResult
  Suspicious  : ScanResult
  Malicious   : ScanResult
  Unscannable : ScanResult

export
Show ScanResult where
  show Clean       = "Clean"
  show Suspicious  = "Suspicious"
  show Malicious   = "Malicious"
  show Unscannable = "Unscannable"

---------------------------------------------------------------------------
-- TransferState : Lifecycle state of a data transfer.
---------------------------------------------------------------------------

||| Current state of a data transfer in its lifecycle.
public export
data TransferState : Type where
  Pending    : TransferState
  Scanning   : TransferState
  Approved   : TransferState
  Rejected   : TransferState
  InProgress : TransferState
  Complete   : TransferState
  Failed     : TransferState

export
Show TransferState where
  show Pending    = "Pending"
  show Scanning   = "Scanning"
  show Approved   = "Approved"
  show Rejected   = "Rejected"
  show InProgress = "InProgress"
  show Complete   = "Complete"
  show Failed     = "Failed"

---------------------------------------------------------------------------
-- ValidationCheck : Types of content validation performed.
---------------------------------------------------------------------------

||| Validation checks applied to transferred content.
public export
data ValidationCheck : Type where
  HashVerify        : ValidationCheck
  SignatureVerify   : ValidationCheck
  FormatCheck       : ValidationCheck
  ContentInspection : ValidationCheck
  MalwareScan       : ValidationCheck

export
Show ValidationCheck where
  show HashVerify        = "HashVerify"
  show SignatureVerify   = "SignatureVerify"
  show FormatCheck       = "FormatCheck"
  show ContentInspection = "ContentInspection"
  show MalwareScan       = "MalwareScan"
