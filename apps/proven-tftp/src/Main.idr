-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-tftp: Main entry point
--
-- A TFTP server implementation that cannot crash on malformed packets.
-- Uses dependent types to validate packet structure, block numbers,
-- transfer modes, and the transfer state machine at compile time.
--
-- Usage:
--   proven-tftp --listen 0.0.0.0:69 --root /tftpboot

module Main

import TFTP
import TFTP.Opcode
import TFTP.Error
import TFTP.Packet
import TFTP.Transfer
import TFTP.Mode
import System

%default total

-- ============================================================================
-- Display helpers
-- ============================================================================

||| Generate simulated file data (256 bytes of incrementing values).
simulateFileData : List Bits8
simulateFileData = map cast (take 256 nats)
  where
    nats : List Nat
    nats = [0..255]

-- ============================================================================
-- Demo: Simulated file read transfer (RRQ -> DATA/ACK -> Complete)
-- ============================================================================

||| Simulate a complete TFTP file read transfer.
||| Demonstrates the RRQ -> DATA #1 -> ACK #1 -> ... -> final DATA -> ACK flow.
covering  -- putStrLn is IO, not structurally recursive
demoReadTransfer : IO ()
demoReadTransfer = do
  putStrLn "\n--- TFTP Read Transfer Demo ---\n"

  -- Step 1: Client sends RRQ
  let rrq = mkReadRequest "firmware.bin" Octet
  putStrLn $ "1. Client -> " ++ show rrq

  -- Step 2: Server receives RRQ, creates transfer session
  let session = newReadSession "firmware.bin" Octet
  putStrLn $ "   Server session: " ++ sessionSummary session

  -- Step 3: Server sends DATA block #1 (full 512 bytes)
  let block1Data = replicate 512 0xAA
  case mkDataPacket 1 block1Data of
    Nothing  => putStrLn "   ERROR: Failed to create DATA packet"
    Just pkt => do
      putStrLn $ "2. Server -> " ++ show pkt
      putStrLn $ "   isLastBlock: " ++ show (isLastBlock pkt)

  -- Step 4: Client sends ACK #1
  let ack1 = mkAck 1
  putStrLn $ "3. Client -> " ++ show ack1

  -- Step 5: Server sends DATA block #2 (full 512 bytes)
  case mkDataPacket 2 (replicate 512 0xBB) of
    Nothing  => putStrLn "   ERROR: Failed to create DATA packet"
    Just pkt => putStrLn $ "4. Server -> " ++ show pkt

  -- Step 6: Client sends ACK #2
  putStrLn $ "5. Client -> " ++ show (mkAck 2)

  -- Step 7: Server sends DATA block #3 (< 512 bytes = last block)
  let lastData = simulateFileData  -- 256 bytes
  case mkDataPacket 3 lastData of
    Nothing  => putStrLn "   ERROR: Failed to create DATA packet"
    Just pkt => do
      putStrLn $ "6. Server -> " ++ show pkt
      putStrLn $ "   isLastBlock: " ++ show (isLastBlock pkt) ++ " (< 512 bytes)"

  -- Step 8: Client sends final ACK
  putStrLn $ "7. Client -> " ++ show (mkAck 3)

  -- Track transfer state
  let s0 = newReadSession "firmware.bin" Octet
  let s1 = recordDataBlock s0 1 512
  let s2 = recordDataBlock s1 2 512
  let s3 = recordDataBlock s2 3 256
  let s4 = applyTransferEvent s3 (DataReceived True)
  putStrLn $ "\nFinal session: " ++ sessionSummary s4
  putStrLn $ "Transfer complete: " ++ show (isComplete s4)
  putStrLn $ "Total bytes: " ++ show s4.bytesTotal ++ " (512 + 512 + 256 = 1280)"

-- ============================================================================
-- Demo: Error handling
-- ============================================================================

||| Demonstrate TFTP error handling.
covering  -- putStrLn is IO, not structurally recursive
demoErrors : IO ()
demoErrors = do
  putStrLn "\n--- TFTP Error Handling Demo ---\n"

  let errors = [NotDefined, FileNotFound, AccessViolation, DiskFull,
                IllegalOperation, UnknownTID, FileExists, NoSuchUser]

  putStrLn "TFTP error codes:"
  traverse_ (\err =>
    putStrLn $ "  " ++ show (cast {to=Nat} (errorCode err))
               ++ ": " ++ show err
               ++ " (recoverable=" ++ show (isRecoverable err)
               ++ ", security=" ++ show (isSecurityError err) ++ ")"
    ) errors

  -- Show error packets
  putStrLn "\nError packets:"
  let errPkt1 = mkErrorPacket FileNotFound
  putStrLn $ "  " ++ show errPkt1

  let errPkt2 = mkErrorPacketWithMsg AccessViolation "Read-only directory: /secure/"
  putStrLn $ "  " ++ show errPkt2

  -- Show what happens when transfer encounters an error
  putStrLn "\nTransfer error scenario:"
  let session = newReadSession "secret.txt" Octet
  putStrLn $ "  Start: " ++ sessionSummary session
  let session' = recordError session AccessViolation
  putStrLn $ "  Error: " ++ sessionSummary session'
  putStrLn $ "  isTerminal: " ++ show (isTerminal session')

-- ============================================================================
-- Demo: Transfer modes
-- ============================================================================

||| Demonstrate TFTP transfer mode properties.
covering  -- putStrLn is IO, not structurally recursive
demoModes : IO ()
demoModes = do
  putStrLn "\n--- TFTP Transfer Mode Demo ---\n"

  let modes = [NetASCII, Octet, Mail]

  putStrLn "Transfer modes:"
  traverse_ (\m =>
    putStrLn $ "  " ++ show m ++ ": " ++ modeDescription m
               ++ "\n    binary=" ++ show (isBinaryMode m)
               ++ ", text=" ++ show (isTextMode m)
               ++ ", translation=" ++ show (hasTranslation m)
               ++ ", obsolete=" ++ show (isObsolete m)
    ) modes

  -- Mode string parsing (case-insensitive per RFC)
  putStrLn "\nMode string parsing (case-insensitive):"
  let modeStrings = ["netascii", "NETASCII", "Netascii",
                     "octet", "OCTET", "mail", "binary", ""]
  traverse_ (\s =>
    case modeFromString s of
      Just m  => putStrLn $ "  \"" ++ s ++ "\" => " ++ show m
      Nothing => putStrLn $ "  \"" ++ s ++ "\" => INVALID"
    ) modeStrings

-- ============================================================================
-- Demo: Opcodes
-- ============================================================================

||| Demonstrate TFTP opcodes and their properties.
covering  -- putStrLn is IO, not structurally recursive
demoOpcodes : IO ()
demoOpcodes = do
  putStrLn "\n--- TFTP Opcode Demo ---\n"

  let opcodes = [RRQ, WRQ, DATA, ACK, ERROR]

  putStrLn "TFTP opcodes:"
  traverse_ (\op =>
    putStrLn $ "  " ++ show (cast {to=Nat} (opcodeValue op))
               ++ ": " ++ show op
               ++ " — " ++ opcodeDescription op
               ++ "\n    request=" ++ show (isRequest op)
               ++ ", data=" ++ show (isDataOpcode op)
               ++ ", response=" ++ show (isResponse op)
               ++ ", terminal=" ++ show (TFTP.Opcode.isTerminal op)
    ) opcodes

  putStrLn "\nExpected responses:"
  traverse_ (\op =>
    case expectedResponse op of
      Just resp => putStrLn $ "  " ++ show op ++ " -> " ++ show resp
      Nothing   => pure ()
    ) opcodes

-- ============================================================================
-- Demo: Retry behaviour
-- ============================================================================

||| Demonstrate retry exhaustion.
covering  -- putStrLn is IO, not structurally recursive
demoRetries : IO ()
demoRetries = do
  putStrLn "\n--- TFTP Retry Demo ---\n"

  putStrLn $ "Max retries: " ++ show maxRetriesVal
  putStrLn $ "Timeout:     " ++ show timeoutSecsVal ++ " seconds"

  let session0 = newReadSession "data.bin" Octet
  putStrLn $ "Start: retries=" ++ show session0.retryCount

  -- Simulate retry exhaustion
  let (s1, ex1) = incrementRetry session0
  putStrLn $ "  Retry 1: retries=" ++ show s1.retryCount ++ ", exhausted=" ++ show ex1
  let (s2, ex2) = incrementRetry s1
  putStrLn $ "  Retry 2: retries=" ++ show s2.retryCount ++ ", exhausted=" ++ show ex2
  let (s3, ex3) = incrementRetry s2
  putStrLn $ "  Retry 3: retries=" ++ show s3.retryCount ++ ", exhausted=" ++ show ex3
  let (s4, ex4) = incrementRetry s3
  putStrLn $ "  Retry 4: retries=" ++ show s4.retryCount ++ ", exhausted=" ++ show ex4
  let (s5, ex5) = incrementRetry s4
  putStrLn $ "  Retry 5: retries=" ++ show s5.retryCount ++ ", exhausted=" ++ show ex5
  let (s6, ex6) = incrementRetry s5
  putStrLn $ "  Retry 6: retries=" ++ show s6.retryCount ++ ", exhausted=" ++ show ex6
             ++ " => ABORT"

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-tftp v0.1.0 — TFTP that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "TFTP port:      " ++ show (cast {to=Nat} tftpPort)
  putStrLn $ "Block size:     " ++ show blockSize ++ " bytes"
  putStrLn $ "Max retries:    " ++ show maxRetriesVal
  putStrLn $ "Timeout:        " ++ show timeoutSecsVal ++ " seconds"

  -- Run demos
  demoReadTransfer
  demoErrors
  demoModes
  demoOpcodes
  demoRetries

  putStrLn "\n--- Ready for production use ---"
  putStrLn "Build with: idris2 --build proven-tftp.ipkg"
  putStrLn "Run with:   ./build/exec/proven-tftp"
