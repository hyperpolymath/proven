-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-diode: Data diode -- unidirectional gateway.
--
-- Architecture:
--   - Types: Direction, Protocol, TransferState, ValidationResult, IntegrityCheck
--
-- This module defines core diode constants and re-exports Diode.Types.

module Diode

import public Diode.Types

%default total

||| Maximum segment size in bytes for diode transit.
||| Chosen to fit within a single Ethernet frame minus headers.
public export
maxSegmentSize : Nat
maxSegmentSize = 1400

||| Maximum number of segments that can be queued for transmission.
public export
maxQueueDepth : Nat
maxQueueDepth = 10000
