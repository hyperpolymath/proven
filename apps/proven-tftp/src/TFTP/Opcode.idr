-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- TFTP Opcodes (RFC 1350 Section 5)
--
-- TFTP defines 5 operation codes that identify packet types. Each opcode
-- is a 2-byte unsigned integer in network byte order. The type system
-- ensures only valid opcodes can be constructed, and the total conversion
-- functions guarantee no crash on unknown opcode values.

module TFTP.Opcode

%default total

-- ============================================================================
-- TFTP Opcodes (RFC 1350 Section 5)
-- ============================================================================

||| The 5 TFTP operation codes as defined in RFC 1350.
||| Each opcode identifies the type of a TFTP packet.
public export
data Opcode : Type where
  ||| Opcode 1: Read Request (RRQ).
  ||| Client requests to read a file from the server.
  RRQ   : Opcode
  ||| Opcode 2: Write Request (WRQ).
  ||| Client requests to write a file to the server.
  WRQ   : Opcode
  ||| Opcode 3: Data packet.
  ||| Contains a block of file data with a block number.
  DATA  : Opcode
  ||| Opcode 4: Acknowledgement.
  ||| Acknowledges receipt of a DATA or WRQ packet.
  ACK   : Opcode
  ||| Opcode 5: Error.
  ||| Indicates an error condition; terminates the transfer.
  ERROR : Opcode

public export
Eq Opcode where
  RRQ   == RRQ   = True
  WRQ   == WRQ   = True
  DATA  == DATA  = True
  ACK   == ACK   = True
  ERROR == ERROR = True
  _     == _     = False

public export
Show Opcode where
  show RRQ   = "RRQ"
  show WRQ   = "WRQ"
  show DATA  = "DATA"
  show ACK   = "ACK"
  show ERROR = "ERROR"

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert an opcode to its 2-byte numeric code (1-5).
public export
opcodeValue : Opcode -> Bits16
opcodeValue RRQ   = 1
opcodeValue WRQ   = 2
opcodeValue DATA  = 3
opcodeValue ACK   = 4
opcodeValue ERROR = 5

||| Decode a 2-byte numeric value to an opcode.
||| Returns Nothing for values outside the valid range (1-5).
public export
opcodeFromValue : Bits16 -> Maybe Opcode
opcodeFromValue 1 = Just RRQ
opcodeFromValue 2 = Just WRQ
opcodeFromValue 3 = Just DATA
opcodeFromValue 4 = Just ACK
opcodeFromValue 5 = Just ERROR
opcodeFromValue _ = Nothing

-- ============================================================================
-- Opcode classification
-- ============================================================================

||| Whether an opcode initiates a transfer (RRQ or WRQ).
public export
isRequest : Opcode -> Bool
isRequest RRQ = True
isRequest WRQ = True
isRequest _   = False

||| Whether an opcode carries file data (DATA).
public export
isDataOpcode : Opcode -> Bool
isDataOpcode DATA = True
isDataOpcode _    = False

||| Whether an opcode is a response type (DATA, ACK, or ERROR).
public export
isResponse : Opcode -> Bool
isResponse DATA  = True
isResponse ACK   = True
isResponse ERROR = True
isResponse _     = False

||| Whether an opcode terminates a transfer (ERROR).
public export
isTerminal : Opcode -> Bool
isTerminal ERROR = True
isTerminal _     = False

||| Human-readable description of an opcode.
public export
opcodeDescription : Opcode -> String
opcodeDescription RRQ   = "Read Request — client wants to read a file"
opcodeDescription WRQ   = "Write Request — client wants to write a file"
opcodeDescription DATA  = "Data — a block of file data"
opcodeDescription ACK   = "Acknowledgement — confirms receipt of a block"
opcodeDescription ERROR = "Error — transfer terminated with error"

||| The expected response opcode for a given request/data opcode.
||| RRQ expects DATA, WRQ expects ACK, DATA expects ACK.
||| Returns Nothing for opcodes that do not expect a specific response.
public export
expectedResponse : Opcode -> Maybe Opcode
expectedResponse RRQ  = Just DATA
expectedResponse WRQ  = Just ACK
expectedResponse DATA = Just ACK
expectedResponse _    = Nothing
