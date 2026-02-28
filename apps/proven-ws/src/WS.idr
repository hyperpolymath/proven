-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ws: A WebSocket server that cannot crash on malformed frames.
--
-- Architecture:
--   - Opcode: 6 standard opcodes with classification (data/control)
--   - Frame: Frame structure, validation, construction
--   - Handshake: HTTP Upgrade handshake validation + response
--   - CloseCode: 11 standard close codes with classification
--   - Session: Session state machine with ping/pong tracking
--
-- This module defines the core WebSocket constants and re-exports submodules.

module WS

import public WS.Opcode
import public WS.Frame
import public WS.Handshake
import public WS.CloseCode
import public WS.Session

||| Default WebSocket port (HTTP, unencrypted).
public export
wsPort : Bits16
wsPort = 80

||| Default WebSocket Secure port (HTTPS/TLS).
public export
wssPort : Bits16
wssPort = 443

||| WebSocket protocol version (RFC 6455).
||| Only version 13 is standardised.
public export
wsVersion : Nat
wsVersion = 13

||| Maximum frame size in bytes (16 MiB).
||| This is our server-specific limit, not a protocol requirement.
public export
maxFrameSize : Nat
maxFrameSize = 16777216

||| Maximum payload size for control frames (RFC 6455 Section 5.5).
||| Control frames MUST NOT have a payload exceeding 125 bytes.
public export
maxControlPayloadSize : Nat
maxControlPayloadSize = 125

||| The WebSocket GUID used in the Sec-WebSocket-Accept computation
||| (RFC 6455 Section 4.2.2).  Concatenated with the client's
||| Sec-WebSocket-Key, then SHA-1 hashed and base64 encoded.
public export
wsGlobalGUID : String
wsGlobalGUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
