-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebSocket Frame (RFC 6455 Section 5.2)
--
-- Defines the WebSocket frame structure with all header fields and
-- payload.  Frame validation ensures control frames do not exceed
-- 125 bytes, masking is present from clients, and the FIN bit is
-- set correctly for control frames.  Invalid frames are rejected
-- as typed errors — no crash.

module WS.Frame

import WS.Opcode

%default total

-- ============================================================================
-- Frame Structure (RFC 6455 Section 5.2)
-- ============================================================================

||| A parsed WebSocket frame with all header fields and payload.
|||
||| Wire format:
|||   0                   1                   2                   3
|||   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
|||  +-+-+-+-+-------+-+-------------+-------------------------------+
|||  |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
|||  |I|S|S|S|  (4)  |A|     (7)     |           (16/64)             |
|||  |N|V|V|V|       |S|             |   (if payload len==126/127)   |
|||  | |1|2|3|       |K|             |                               |
|||  +-+-+-+-+-------+-+-------------+-------------------------------+
public export
record Frame where
  constructor MkFrame
  ||| FIN bit — True if this is the final fragment of a message
  fin           : Bool
  ||| Opcode identifying the frame type
  opcode        : Opcode
  ||| MASK bit — True if payload is XOR-masked (required from clients)
  masked        : Bool
  ||| Payload length in bytes (0 to 2^63-1)
  payloadLength : Nat
  ||| 4-byte masking key (present only if masked is True)
  maskingKey    : Maybe (Vect 4 Bits8)
  ||| Payload data (unmasked)
  payload       : List Bits8

public export
Show Frame where
  show f = "Frame("
           ++ (if f.fin then "FIN" else "...") ++ " "
           ++ show f.opcode ++ " "
           ++ (if f.masked then "MASKED" else "UNMASKED") ++ " "
           ++ "len=" ++ show f.payloadLength
           ++ ")"

-- ============================================================================
-- Frame Validation (RFC 6455 Section 5.5)
-- ============================================================================

||| Errors detected during frame validation.
public export
data FrameError : Type where
  ||| Control frame exceeds 125-byte payload limit (RFC 6455 Section 5.5)
  ControlFrameTooLarge  : (opcode : Opcode) -> (size : Nat) -> FrameError
  ||| Control frame is fragmented (FIN bit not set)
  ControlFrameFragmented : (opcode : Opcode) -> FrameError
  ||| Client frame is not masked (RFC 6455 Section 5.1)
  ClientFrameNotMasked  : FrameError
  ||| Server frame is masked (servers MUST NOT mask)
  ServerFrameMasked     : FrameError
  ||| Payload exceeds maximum allowed frame size
  PayloadTooLarge       : (size : Nat) -> (maxSize : Nat) -> FrameError
  ||| Reserved opcode used
  ReservedOpcode        : (nibble : Bits8) -> FrameError
  ||| Payload length and actual data mismatch
  PayloadLengthMismatch : (declared : Nat) -> (actual : Nat) -> FrameError

public export
Show FrameError where
  show (ControlFrameTooLarge op n) =
    "Control frame " ++ show op ++ " payload too large: " ++ show n
    ++ " bytes (max 125)"
  show (ControlFrameFragmented op) =
    "Control frame " ++ show op ++ " must not be fragmented"
  show ClientFrameNotMasked    = "Client frame must be masked"
  show ServerFrameMasked       = "Server frame must not be masked"
  show (PayloadTooLarge s m)   =
    "Payload too large: " ++ show s ++ " bytes (max " ++ show m ++ ")"
  show (ReservedOpcode n)      =
    "Reserved opcode: 0x" ++ show (cast {to=Nat} n)
  show (PayloadLengthMismatch d a) =
    "Payload length mismatch: declared " ++ show d ++ ", actual " ++ show a

||| Maximum payload size for control frames (RFC 6455 Section 5.5).
public export
maxControlPayload : Nat
maxControlPayload = 125

||| Validate a frame received from a client.
||| Checks: masking required, control frame size, control frame fragmentation,
||| payload length consistency.
public export
validateClientFrame : (maxFrameSize : Nat) -> Frame -> Either FrameError Frame
validateClientFrame maxSize f =
  -- Client frames MUST be masked
  if not f.masked
    then Left ClientFrameNotMasked
  -- Control frames MUST NOT exceed 125 bytes
  else if isControl f.opcode && f.payloadLength > maxControlPayload
    then Left (ControlFrameTooLarge f.opcode f.payloadLength)
  -- Control frames MUST NOT be fragmented
  else if isControl f.opcode && not f.fin
    then Left (ControlFrameFragmented f.opcode)
  -- Payload size limit
  else if f.payloadLength > maxSize
    then Left (PayloadTooLarge f.payloadLength maxSize)
  -- Payload length must match actual data
  else if f.payloadLength /= length f.payload
    then Left (PayloadLengthMismatch f.payloadLength (length f.payload))
  else Right f

||| Validate a frame received from a server.
||| Server frames MUST NOT be masked (RFC 6455 Section 5.1).
public export
validateServerFrame : (maxFrameSize : Nat) -> Frame -> Either FrameError Frame
validateServerFrame maxSize f =
  if f.masked
    then Left ServerFrameMasked
  else if isControl f.opcode && f.payloadLength > maxControlPayload
    then Left (ControlFrameTooLarge f.opcode f.payloadLength)
  else if isControl f.opcode && not f.fin
    then Left (ControlFrameFragmented f.opcode)
  else if f.payloadLength > maxSize
    then Left (PayloadTooLarge f.payloadLength maxSize)
  else if f.payloadLength /= length f.payload
    then Left (PayloadLengthMismatch f.payloadLength (length f.payload))
  else Right f

-- ============================================================================
-- Frame Construction
-- ============================================================================

||| Build a server-to-client text frame (unmasked, FIN set).
public export
makeTextFrame : (payload : List Bits8) -> Frame
makeTextFrame p = MkFrame
  { fin           = True
  , opcode        = Text
  , masked        = False
  , payloadLength = length p
  , maskingKey    = Nothing
  , payload       = p
  }

||| Build a server-to-client binary frame (unmasked, FIN set).
public export
makeBinaryFrame : (payload : List Bits8) -> Frame
makeBinaryFrame p = MkFrame
  { fin           = True
  , opcode        = Binary
  , masked        = False
  , payloadLength = length p
  , maskingKey    = Nothing
  , payload       = p
  }

||| Build a Pong frame in response to a Ping.
||| The Pong MUST echo the Ping's payload (RFC 6455 Section 5.5.3).
public export
makePongFrame : (pingPayload : List Bits8) -> Frame
makePongFrame p = MkFrame
  { fin           = True
  , opcode        = Pong
  , masked        = False
  , payloadLength = length p
  , maskingKey    = Nothing
  , payload       = p
  }

||| Build a Close frame with an optional status code and reason.
public export
makeCloseFrame : (statusCode : Maybe Bits16) -> (reason : List Bits8) -> Frame
makeCloseFrame code reasonBytes =
  let codeBytes = case code of
                    Nothing => []
                    Just c  => [ cast (prim__shr_Bits16 c 8)
                               , cast (prim__and_Bits16 c 0xFF)
                               ]
      payload = codeBytes ++ reasonBytes
  in MkFrame
    { fin           = True
    , opcode        = Close
    , masked        = False
    , payloadLength = length payload
    , maskingKey    = Nothing
    , payload       = payload
    }

||| Build a Ping frame with optional payload.
public export
makePingFrame : (payload : List Bits8) -> Frame
makePingFrame p = MkFrame
  { fin           = True
  , opcode        = Ping
  , masked        = False
  , payloadLength = length p
  , maskingKey    = Nothing
  , payload       = p
  }
