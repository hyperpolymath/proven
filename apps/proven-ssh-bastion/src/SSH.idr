-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ssh-bastion: An SSH bastion host that cannot crash.
--
-- Architecture:
--   - Transport: Version exchange, key exchange init, algorithm negotiation
--   - Auth: Authentication methods, request/response, attempt tracking
--   - Channel: Channel types, state machine, flow control
--   - Session: Session state machine (version → kex → auth → established)
--   - Config: Bastion configuration with validation
--
-- This module defines the core SSH constants and re-exports submodules.

module SSH

import public SSH.Transport
import public SSH.Auth
import public SSH.Channel
import public SSH.Session
import public SSH.Config

||| SSH default port (RFC 4253)
public export
sshPort : Bits16
sshPort = 22

||| Protocol version string sent during version exchange.
||| Format: SSH-protoversion-softwareversion (RFC 4253 Section 4.2)
public export
protocolVersion : String
protocolVersion = "SSH-2.0-proven_0.1"

||| Maximum SSH packet size in bytes (RFC 4253 Section 6.1).
||| The minimum maximum is 35000 bytes; we use that as our cap.
public export
maxPacketSize : Nat
maxPacketSize = 35000

||| Maximum number of channels per session.
||| This is our bastion-specific limit, not an SSH protocol requirement.
public export
maxChannels : Nat
maxChannels = 10

||| Default window size for channel flow control (2 MiB).
||| This determines how much data a peer can send before needing
||| a window adjustment message.
public export
defaultWindowSize : Nat
defaultWindowSize = 2097152
