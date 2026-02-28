-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- TFTP Packet Structure (RFC 1350 Section 5)
--
-- TFTP packets come in 5 flavours: RRQ, WRQ, DATA, ACK, and ERROR.
-- Each is a variant of the TFTPPacket sum type carrying only the fields
-- relevant to that packet type. Block numbers are Bits16 (0-65535),
-- DATA payloads are capped at 512 bytes per block, and request packets
-- carry a filename and transfer mode string.

module TFTP.Packet

import TFTP.Opcode
import TFTP.Error
import TFTP.Mode

%default total

-- ============================================================================
-- TFTP Packet variants (RFC 1350 Section 5, Figures 5-1 through 5-5)
-- ============================================================================

||| A fully parsed TFTP packet.
||| Each constructor carries exactly the fields for that packet type.
public export
data TFTPPacket : Type where
  ||| Read Request: client wants to read a file from the server.
  ||| Carries the filename and transfer mode.
  ReadRequest  : (filename : String) -> (mode : TransferMode) -> TFTPPacket
  ||| Write Request: client wants to write a file to the server.
  ||| Carries the filename and transfer mode.
  WriteRequest : (filename : String) -> (mode : TransferMode) -> TFTPPacket
  ||| Data packet: carries one block of file data.
  ||| Block numbers start at 1 and increment with each block.
  ||| The last block has fewer than 512 bytes (possibly 0).
  DataPacket   : (blockNum : Bits16) -> (payload : List Bits8) -> TFTPPacket
  ||| Acknowledgement: confirms receipt of a DATA or WRQ packet.
  ||| The block number echoes the block being acknowledged.
  ||| ACK with block 0 acknowledges a WRQ.
  Acknowledgement : (blockNum : Bits16) -> TFTPPacket
  ||| Error packet: indicates an error and terminates the transfer.
  ||| Carries an error code and a human-readable error message.
  ErrorPacket  : (errInfo : ErrorInfo) -> TFTPPacket

public export
Show TFTPPacket where
  show (ReadRequest fn m)    = "RRQ \"" ++ fn ++ "\" " ++ show m
  show (WriteRequest fn m)   = "WRQ \"" ++ fn ++ "\" " ++ show m
  show (DataPacket blk dat)  = "DATA #" ++ show (cast {to=Nat} blk)
                               ++ " [" ++ show (length dat) ++ " bytes]"
  show (Acknowledgement blk) = "ACK #" ++ show (cast {to=Nat} blk)
  show (ErrorPacket err)     = show err

-- ============================================================================
-- Packet construction helpers
-- ============================================================================

||| Create a read request packet.
public export
mkReadRequest : (filename : String) -> (mode : TransferMode) -> TFTPPacket
mkReadRequest = ReadRequest

||| Create a write request packet.
public export
mkWriteRequest : (filename : String) -> (mode : TransferMode) -> TFTPPacket
mkWriteRequest = WriteRequest

||| Create a data packet with validated block number and payload.
||| Returns Nothing if the payload exceeds the block size (512 bytes).
public export
mkDataPacket : (blockNum : Bits16) -> (payload : List Bits8) -> Maybe TFTPPacket
mkDataPacket blk dat =
  if length dat > 512
    then Nothing  -- Payload too large for a single block
    else Just (DataPacket blk dat)

||| Create an acknowledgement packet.
public export
mkAck : (blockNum : Bits16) -> TFTPPacket
mkAck = Acknowledgement

||| Create an error packet with a standard error code.
public export
mkErrorPacket : TFTPError -> TFTPPacket
mkErrorPacket err = ErrorPacket (mkError err)

||| Create an error packet with a custom error message.
public export
mkErrorPacketWithMsg : TFTPError -> String -> TFTPPacket
mkErrorPacketWithMsg err msg = ErrorPacket (mkErrorWithMsg err msg)

-- ============================================================================
-- Packet properties
-- ============================================================================

||| Get the opcode for a packet.
public export
packetOpcode : TFTPPacket -> Opcode
packetOpcode (ReadRequest _ _)    = RRQ
packetOpcode (WriteRequest _ _)   = WRQ
packetOpcode (DataPacket _ _)     = DATA
packetOpcode (Acknowledgement _)  = ACK
packetOpcode (ErrorPacket _)      = ERROR

||| Get the block number from a DATA or ACK packet.
||| Returns Nothing for RRQ, WRQ, and ERROR packets.
public export
packetBlockNum : TFTPPacket -> Maybe Bits16
packetBlockNum (DataPacket blk _)    = Just blk
packetBlockNum (Acknowledgement blk) = Just blk
packetBlockNum _                     = Nothing

||| Check whether a DATA packet is the last block of a transfer.
||| The last block has fewer than 512 bytes of data (RFC 1350 Section 2).
||| Returns Nothing for non-DATA packets.
public export
isLastBlock : TFTPPacket -> Maybe Bool
isLastBlock (DataPacket _ dat) = Just (length dat < 512)
isLastBlock _                  = Nothing

-- ============================================================================
-- Parse errors
-- ============================================================================

||| Errors that can occur when parsing a TFTP packet from raw bytes.
public export
data TFTPParseError : Type where
  ||| Packet is too short to contain an opcode (minimum 2 bytes).
  PacketTooShort  : (actual : Nat) -> TFTPParseError
  ||| Unknown or reserved opcode value.
  UnknownOpcode   : (value : Bits16) -> TFTPParseError
  ||| Request packet has no null-terminated filename.
  MissingFilename : TFTPParseError
  ||| Request packet has no null-terminated mode string.
  MissingMode     : TFTPParseError
  ||| Unrecognised transfer mode string.
  InvalidMode     : (mode : String) -> TFTPParseError
  ||| DATA packet payload exceeds block size.
  PayloadTooLarge : (actual : Nat) -> TFTPParseError
  ||| Error packet has an invalid error code.
  InvalidErrorCode : (value : Bits16) -> TFTPParseError

public export
Show TFTPParseError where
  show (PacketTooShort n)   = "Packet too short: " ++ show n ++ " bytes (need >= 2)"
  show (UnknownOpcode v)    = "Unknown opcode: " ++ show (cast {to=Nat} v)
  show MissingFilename      = "Missing null-terminated filename"
  show MissingMode          = "Missing null-terminated mode string"
  show (InvalidMode m)      = "Invalid transfer mode: " ++ m
  show (PayloadTooLarge n)  = "Payload too large: " ++ show n ++ " bytes (max 512)"
  show (InvalidErrorCode v) = "Invalid error code: " ++ show (cast {to=Nat} v)
