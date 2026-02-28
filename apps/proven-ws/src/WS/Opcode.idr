-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebSocket Opcodes (RFC 6455 Section 5.2)
--
-- Defines the 6 standard WebSocket opcodes as a sum type with
-- classification functions (data vs control), numeric encoding,
-- and Eq/Show instances.  Unknown opcodes are rejected as Nothing
-- values — they cannot cause a crash.

module WS.Opcode

%default total

-- ============================================================================
-- WebSocket Opcodes
-- ============================================================================

||| The 6 WebSocket opcodes defined in RFC 6455 Section 11.8.
||| Opcodes 0x3-0x7 (non-control) and 0xB-0xF (control) are reserved.
public export
data Opcode : Type where
  ||| 0x0 — Continuation frame (follows a fragmented message)
  Continuation : Opcode
  ||| 0x1 — Text frame (payload is UTF-8 encoded text)
  Text         : Opcode
  ||| 0x2 — Binary frame (payload is arbitrary binary data)
  Binary       : Opcode
  ||| 0x8 — Close frame (initiates or acknowledges connection close)
  Close        : Opcode
  ||| 0x9 — Ping frame (heartbeat request)
  Ping         : Opcode
  ||| 0xA — Pong frame (heartbeat response)
  Pong         : Opcode

public export
Eq Opcode where
  Continuation == Continuation = True
  Text         == Text         = True
  Binary       == Binary       = True
  Close        == Close        = True
  Ping         == Ping         = True
  Pong         == Pong         = True
  _            == _            = False

public export
Show Opcode where
  show Continuation = "Continuation(0x0)"
  show Text         = "Text(0x1)"
  show Binary       = "Binary(0x2)"
  show Close        = "Close(0x8)"
  show Ping         = "Ping(0x9)"
  show Pong         = "Pong(0xA)"

-- ============================================================================
-- Numeric Encoding
-- ============================================================================

||| Convert an opcode to its 4-bit wire value.
public export
opcodeToNibble : Opcode -> Bits8
opcodeToNibble Continuation = 0x0
opcodeToNibble Text         = 0x1
opcodeToNibble Binary       = 0x2
opcodeToNibble Close        = 0x8
opcodeToNibble Ping         = 0x9
opcodeToNibble Pong         = 0xA

||| Parse a 4-bit nibble to an opcode.
||| Returns Nothing for reserved opcodes (0x3-0x7, 0xB-0xF) — no crash.
public export
opcodeFromNibble : Bits8 -> Maybe Opcode
opcodeFromNibble 0x0 = Just Continuation
opcodeFromNibble 0x1 = Just Text
opcodeFromNibble 0x2 = Just Binary
opcodeFromNibble 0x8 = Just Close
opcodeFromNibble 0x9 = Just Ping
opcodeFromNibble 0xA = Just Pong
opcodeFromNibble _   = Nothing

-- ============================================================================
-- Classification
-- ============================================================================

||| Check if an opcode is a data opcode (Continuation, Text, Binary).
||| Data opcodes carry application payload and can be fragmented.
public export
isData : Opcode -> Bool
isData Continuation = True
isData Text         = True
isData Binary       = True
isData Close        = False
isData Ping         = False
isData Pong         = False

||| Check if an opcode is a control opcode (Close, Ping, Pong).
||| Control frames MUST NOT be fragmented and MUST have a payload
||| length of 125 bytes or less (RFC 6455 Section 5.5).
public export
isControl : Opcode -> Bool
isControl op = not (isData op)

||| Check if an opcode begins a new message (Text or Binary).
||| Continuation frames extend a message already in progress.
public export
isMessageStart : Opcode -> Bool
isMessageStart Text   = True
isMessageStart Binary = True
isMessageStart _      = False

||| Check if an opcode requires a mandatory response.
||| Ping frames MUST be responded to with a Pong.
||| Close frames MUST be responded to with a Close.
public export
requiresResponse : Opcode -> Bool
requiresResponse Ping  = True
requiresResponse Close = True
requiresResponse _     = False

||| Get the human-readable name for an opcode.
public export
opcodeName : Opcode -> String
opcodeName Continuation = "continuation"
opcodeName Text         = "text"
opcodeName Binary       = "binary"
opcodeName Close        = "close"
opcodeName Ping         = "ping"
opcodeName Pong         = "pong"
