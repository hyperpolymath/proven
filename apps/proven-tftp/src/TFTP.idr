-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-tftp: A TFTP (RFC 1350) implementation that cannot crash.
--
-- Architecture:
--   - Opcode: 5 operation codes with compile-time exhaustive matching
--   - Error: 8 error codes with standard messages and classification
--   - Packet: 5 packet variants with validated block numbers and payload sizes
--   - Transfer: 5-state transfer FSM with valid-transition-only types
--   - Mode: 3 transfer modes (NetASCII, Octet, Mail) with property functions
--
-- This module defines the core TFTP types and re-exports submodules.

module TFTP

import public TFTP.Opcode
import public TFTP.Error
import public TFTP.Packet
import public TFTP.Transfer
import public TFTP.Mode

||| TFTP default port (RFC 1350 Section 2).
public export
tftpPort : Bits16
tftpPort = 69

||| TFTP block size in bytes (RFC 1350 Section 2).
||| Each DATA packet carries at most 512 bytes of data.
||| The last block has fewer than 512 bytes (possibly 0).
public export
blockSize : Nat
blockSize = 512

||| Maximum number of retries before abandoning a transfer.
public export
maxRetriesVal : Nat
maxRetriesVal = 5

||| Timeout in seconds before retransmitting a packet.
public export
timeoutSecsVal : Nat
timeoutSecsVal = 5
