-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-lpd: A Line Printer Daemon implementation that cannot crash.
--
-- Architecture:
--   - Command: 5 LPD commands as a closed sum type with byte codes
--   - Job: Print job records with validated IDs (000-999) and status tracking
--   - Queue: Bounded FIFO queue with capacity enforcement and CRUD operations
--   - Protocol: State machine (Idle -> ReceivingControlFile -> ReceivingDataFile)
--   - Spool: Spool directory management with RFC 1179 file naming conventions
--
-- This module defines core LPD constants and re-exports all submodules.

module LPD

import public LPD.Command
import public LPD.Job
import public LPD.Queue
import public LPD.Protocol
import public LPD.Spool

||| Standard LPD port (RFC 1179 Section 1).
public export
lpdPort : Bits16
lpdPort = 515

||| Maximum print job size in bytes (100 MiB default).
public export
maxJobSize : Nat
maxJobSize = 104857600

||| Maximum number of jobs in a single queue.
public export
maxQueueDepth : Nat
maxQueueDepth = 100

||| Default spool directory base path.
public export
defaultSpoolPath : String
defaultSpoolPath = "/var/spool/lpd"

||| Server identification string for proven-lpd.
public export
serverIdent : String
serverIdent = "proven-lpd/0.1.0"
