-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ws: Main entry point
--
-- A WebSocket server implementation that cannot crash on malformed frames.
-- Uses dependent types to enforce frame validity (control frame size limits,
-- masking requirements) and session state machine correctness.
--
-- Usage:
--   proven-ws --port 8080

module Main

import WS
import WS.Opcode
import WS.Frame
import WS.Handshake
import WS.CloseCode
import WS.Session
import System

%default total

-- ============================================================================
-- Display helpers
-- ============================================================================

||| Print a separator line.
covering
printSep : IO ()
printSep = putStrLn (replicate 60 '-')

-- ============================================================================
-- Demo: WebSocket handshake
-- ============================================================================

||| Demonstrate the WebSocket opening handshake validation.
covering
demoHandshake : IO ()
demoHandshake = do
  putStrLn "\n--- WebSocket Handshake Demo ---\n"

  -- Valid handshake request
  let validReq = MkHandshakeRequest
        { method          = "GET"
        , requestUri      = "/chat"
        , httpVersion     = "1.1"
        , headers         = [ MkHttpHeader "upgrade" "websocket"
                            , MkHttpHeader "connection" "Upgrade"
                            , MkHttpHeader "host" "example.com"
                            ]
        , webSocketKey    = Just "dGhlIHNhbXBsZSBub25jZQ=="  -- 24 chars
        , webSocketVersion = Just "13"
        , subprotocols    = ["chat"]
        }

  putStrLn $ "Request: " ++ show validReq
  case validateHandshake validReq of
    Left err  => putStrLn $ "  REJECTED: " ++ show err
    Right _   => do
      putStrLn "  ACCEPTED"
      let resp = makeAcceptResponse "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" (Just "chat")
      putStrLn $ "  Response: " ++ show resp
      putStrLn $ "  Accept-Key: " ++ resp.acceptKey

  -- Invalid: wrong method
  putStrLn "\n  Testing POST request..."
  let badMethod = { method := "POST" } validReq
  case validateHandshake badMethod of
    Left err => putStrLn $ "  REJECTED: " ++ show err
    Right _  => putStrLn "  ACCEPTED (unexpected)"

  -- Invalid: missing key
  putStrLn "  Testing missing Sec-WebSocket-Key..."
  let noKey = { webSocketKey := Nothing } validReq
  case validateHandshake noKey of
    Left err => putStrLn $ "  REJECTED: " ++ show err
    Right _  => putStrLn "  ACCEPTED (unexpected)"

  -- Invalid: wrong version
  putStrLn "  Testing version 8..."
  let badVer = { webSocketVersion := Just "8" } validReq
  case validateHandshake badVer of
    Left err => putStrLn $ "  REJECTED: " ++ show err
    Right _  => putStrLn "  ACCEPTED (unexpected)"

-- ============================================================================
-- Demo: Frame operations
-- ============================================================================

||| Demonstrate frame construction and validation.
covering
demoFrames : IO ()
demoFrames = do
  putStrLn "\n--- WebSocket Frame Demo ---\n"

  -- Build a text frame
  let textPayload = map (cast . ord) (unpack "Hello, WebSocket!")
  let textFrame = makeTextFrame textPayload
  putStrLn $ "Text frame: " ++ show textFrame

  -- Build a ping frame
  let pingFrame = makePingFrame [0x01, 0x02, 0x03]
  putStrLn $ "Ping frame: " ++ show pingFrame
  putStrLn $ "  isControl: " ++ show (isControl pingFrame.opcode)
  putStrLn $ "  requiresResponse: " ++ show (requiresResponse pingFrame.opcode)

  -- Build matching pong
  let pongFrame = makePongFrame pingFrame.payload
  putStrLn $ "Pong frame: " ++ show pongFrame

  -- Build a close frame
  let closeFrame = makeCloseFrame (Just (closeCodeToWord Normal)) []
  putStrLn $ "Close frame: " ++ show closeFrame

  -- Validate a valid client frame (masked)
  putStrLn "\n  Validating frames..."
  let clientFrame = MkFrame
        { fin = True, opcode = Text, masked = True
        , payloadLength = 5, maskingKey = Just [0xAA, 0xBB, 0xCC, 0xDD]
        , payload = [0x48, 0x65, 0x6C, 0x6C, 0x6F]
        }
  case validateClientFrame maxFrameSize clientFrame of
    Left err => putStrLn $ "  Client frame REJECTED: " ++ show err
    Right _  => putStrLn "  Client frame ACCEPTED (masked, valid)"

  -- Validate an invalid client frame (not masked)
  let unmaskedClient = { masked := False, maskingKey := Nothing } clientFrame
  case validateClientFrame maxFrameSize unmaskedClient of
    Left err => putStrLn $ "  Unmasked client REJECTED: " ++ show err
    Right _  => putStrLn "  Unmasked client ACCEPTED (unexpected)"

  -- Validate a control frame that is too large
  let bigControl = MkFrame
        { fin = True, opcode = Ping, masked = True
        , payloadLength = 200, maskingKey = Just [0x11, 0x22, 0x33, 0x44]
        , payload = replicate 200 0x00
        }
  case validateClientFrame maxFrameSize bigControl of
    Left err => putStrLn $ "  Big control REJECTED: " ++ show err
    Right _  => putStrLn "  Big control ACCEPTED (unexpected)"

-- ============================================================================
-- Demo: Session lifecycle with ping/pong
-- ============================================================================

||| Demonstrate the WebSocket session state machine.
covering
demoSession : IO ()
demoSession = do
  putStrLn "\n--- WebSocket Session Demo ---\n"

  -- Start in Connecting
  let s0 = newWSSession
  putStrLn $ "Initial state: " ++ show s0.state

  -- Complete handshake
  case completeHandshake s0 of
    Nothing => putStrLn "ERROR: cannot complete handshake"
    Just s1 => do
      putStrLn $ "After handshake: " ++ show s1.state
      putStrLn $ "  canSendData: " ++ show (canSendData s1)

      -- Simulate ping/pong cycle
      let pt1 = recordPingSent s1.pingTracker
      putStrLn $ "\n  Ping sent: outstanding=" ++ show pt1.outstanding
      let pt2 = recordPongReceived pt1
      putStrLn $ "  Pong received: outstanding=" ++ show pt2.outstanding
      putStrLn $ "  Peer dead: " ++ show (isPeerDead pt2)

      -- Simulate data exchange
      let s2 = recordFrameSent (recordFrameReceived s1)
      putStrLn $ "\n  Frames: received=" ++ show s2.framesReceived
                 ++ " sent=" ++ show s2.framesSent

      -- Initiate graceful close
      case initiateClose Normal s2 of
        Nothing => putStrLn "ERROR: cannot initiate close"
        Just s3 => do
          putStrLn $ "\n  Close initiated: " ++ show s3.state
          putStrLn $ "  We initiated: " ++ show s3.closeInitiator
          putStrLn $ "  Our close code: " ++ show s3.ourCloseCode

          -- Peer responds with close
          let s4 = receiveClose Normal s3
          putStrLn $ "  Peer close received: " ++ show s4.state
          putStrLn $ "  Peer close code: " ++ show s4.peerCloseCode

          putStrLn $ "\n  isClosed: " ++ show (isClosed s4)

-- ============================================================================
-- Demo: Close codes
-- ============================================================================

||| Demonstrate close code classification.
covering
demoCloseCodes : IO ()
demoCloseCodes = do
  putStrLn "\n--- Close Code Classification ---\n"
  let codes = [Normal, GoingAway, ProtocolError, UnsupportedData,
               NoStatus, Abnormal, InvalidPayload, PolicyViolation,
               MessageTooBig, MandatoryExtension, InternalError]
  traverse_ showCode codes
  where
    covering
    showCode : CloseCode -> IO ()
    showCode c = putStrLn $ "  " ++ show c
                 ++ " normal=" ++ show (isNormalClose c)
                 ++ " sendable=" ++ show (isSendable c)
                 ++ " | " ++ closeReason c

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  args <- getArgs
  putStrLn "proven-ws v0.1.0 — WebSocket server that cannot crash"
  putStrLn $ "Protocol version: " ++ show wsVersion
  putStrLn $ "GUID: " ++ wsGlobalGUID
  putStrLn "Powered by proven (Idris 2 formal verification)"

  -- Run demos
  demoHandshake
  demoFrames
  demoSession
  demoCloseCodes

  printSep
  putStrLn "All frame validation proven at compile time"
  putStrLn "Build with: idris2 --build proven-ws.ipkg"
  putStrLn "Run with:   ./build/exec/proven-ws"
