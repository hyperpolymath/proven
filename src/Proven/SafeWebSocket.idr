-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeWebSocket - Type-safe WebSocket protocol primitives
|||
||| Provides verified WebSocket frame types, close codes (RFC 6455),
||| message size validation, and safe frame construction.
||| Prevents: invalid close codes, oversized frames, protocol confusion.
module Proven.SafeWebSocket

import Data.String
import Data.List
import Data.Nat
import Data.So
import Data.Bits

%default total

-- ============================================================================
-- OPCODES (RFC 6455 Section 5.2)
-- ============================================================================

||| WebSocket frame opcodes
public export
data Opcode =
    Continuation   -- 0x0
  | TextFrame      -- 0x1
  | BinaryFrame    -- 0x2
  | Close          -- 0x8
  | Ping           -- 0x9
  | Pong           -- 0xA

public export
Show Opcode where
  show Continuation = "continuation"
  show TextFrame    = "text"
  show BinaryFrame  = "binary"
  show Close        = "close"
  show Ping         = "ping"
  show Pong         = "pong"

public export
Eq Opcode where
  Continuation == Continuation = True
  TextFrame == TextFrame = True
  BinaryFrame == BinaryFrame = True
  Close == Close = True
  Ping == Ping = True
  Pong == Pong = True
  _ == _ = False

||| Whether the opcode is a control frame
public export
isControlFrame : Opcode -> Bool
isControlFrame Close = True
isControlFrame Ping  = True
isControlFrame Pong  = True
isControlFrame _     = False

||| Whether the opcode is a data frame
public export
isDataFrame : Opcode -> Bool
isDataFrame = not . isControlFrame

||| Convert opcode to its numeric value
public export
opcodeToNat : Opcode -> Nat
opcodeToNat Continuation = 0
opcodeToNat TextFrame    = 1
opcodeToNat BinaryFrame  = 2
opcodeToNat Close        = 8
opcodeToNat Ping         = 9
opcodeToNat Pong         = 10

||| Parse opcode from numeric value
public export
parseOpcode : Nat -> Maybe Opcode
parseOpcode 0  = Just Continuation
parseOpcode 1  = Just TextFrame
parseOpcode 2  = Just BinaryFrame
parseOpcode 8  = Just Close
parseOpcode 9  = Just Ping
parseOpcode 10 = Just Pong
parseOpcode _  = Nothing

-- ============================================================================
-- CLOSE CODES (RFC 6455 Section 7.4.1)
-- ============================================================================

||| Standard WebSocket close status codes
public export
data CloseCode =
    NormalClosure          -- 1000
  | GoingAway              -- 1001
  | ProtocolError          -- 1002
  | UnsupportedData        -- 1003
  | NoStatusReceived       -- 1005 (must not be sent in frame)
  | AbnormalClosure        -- 1006 (must not be sent in frame)
  | InvalidFramePayload    -- 1007
  | PolicyViolation        -- 1008
  | MessageTooBig          -- 1009
  | MandatoryExtension     -- 1010
  | InternalServerError    -- 1011
  | TLSHandshakeFailed     -- 1015 (must not be sent in frame)
  | ApplicationCode Nat    -- 4000-4999 (application-defined)

public export
Show CloseCode where
  show NormalClosure       = "1000 Normal Closure"
  show GoingAway           = "1001 Going Away"
  show ProtocolError       = "1002 Protocol Error"
  show UnsupportedData     = "1003 Unsupported Data"
  show NoStatusReceived    = "1005 No Status Received"
  show AbnormalClosure     = "1006 Abnormal Closure"
  show InvalidFramePayload = "1007 Invalid Frame Payload"
  show PolicyViolation     = "1008 Policy Violation"
  show MessageTooBig       = "1009 Message Too Big"
  show MandatoryExtension  = "1010 Mandatory Extension"
  show InternalServerError = "1011 Internal Server Error"
  show TLSHandshakeFailed  = "1015 TLS Handshake Failed"
  show (ApplicationCode n) = show n ++ " Application Code"

public export
Eq CloseCode where
  NormalClosure == NormalClosure = True
  GoingAway == GoingAway = True
  ProtocolError == ProtocolError = True
  UnsupportedData == UnsupportedData = True
  NoStatusReceived == NoStatusReceived = True
  AbnormalClosure == AbnormalClosure = True
  InvalidFramePayload == InvalidFramePayload = True
  PolicyViolation == PolicyViolation = True
  MessageTooBig == MessageTooBig = True
  MandatoryExtension == MandatoryExtension = True
  InternalServerError == InternalServerError = True
  TLSHandshakeFailed == TLSHandshakeFailed = True
  (ApplicationCode a) == (ApplicationCode b) = a == b
  _ == _ = False

||| Convert close code to its numeric value
public export
closeCodeToNat : CloseCode -> Nat
closeCodeToNat NormalClosure       = 1000
closeCodeToNat GoingAway           = 1001
closeCodeToNat ProtocolError       = 1002
closeCodeToNat UnsupportedData     = 1003
closeCodeToNat NoStatusReceived    = 1005
closeCodeToNat AbnormalClosure     = 1006
closeCodeToNat InvalidFramePayload = 1007
closeCodeToNat PolicyViolation     = 1008
closeCodeToNat MessageTooBig       = 1009
closeCodeToNat MandatoryExtension  = 1010
closeCodeToNat InternalServerError = 1011
closeCodeToNat TLSHandshakeFailed  = 1015
closeCodeToNat (ApplicationCode n) = n

||| Parse close code from numeric value
public export
parseCloseCode : Nat -> Maybe CloseCode
parseCloseCode 1000 = Just NormalClosure
parseCloseCode 1001 = Just GoingAway
parseCloseCode 1002 = Just ProtocolError
parseCloseCode 1003 = Just UnsupportedData
parseCloseCode 1005 = Just NoStatusReceived
parseCloseCode 1006 = Just AbnormalClosure
parseCloseCode 1007 = Just InvalidFramePayload
parseCloseCode 1008 = Just PolicyViolation
parseCloseCode 1009 = Just MessageTooBig
parseCloseCode 1010 = Just MandatoryExtension
parseCloseCode 1011 = Just InternalServerError
parseCloseCode 1015 = Just TLSHandshakeFailed
parseCloseCode n    = if n >= 4000 && n <= 4999
                      then Just (ApplicationCode n)
                      else Nothing

||| Whether a close code can be sent in a Close frame
||| (1005, 1006, 1015 are reserved and must NOT be sent)
public export
isSendable : CloseCode -> Bool
isSendable NoStatusReceived  = False
isSendable AbnormalClosure   = False
isSendable TLSHandshakeFailed = False
isSendable _                 = True

-- ============================================================================
-- MESSAGE SIZE LIMITS
-- ============================================================================

||| Maximum payload size for control frames (125 bytes per RFC 6455)
public export
MaxControlPayload : Nat
MaxControlPayload = 125

||| Default maximum message size (16 MiB)
public export
DefaultMaxMessageSize : Nat
DefaultMaxMessageSize = 16777216

||| A validated message payload with size check
public export
record MessagePayload where
  constructor MkPayload
  content  : String
  maxSize  : Nat
  0 sizeOk : So (length content <= maxSize)

||| Postulate for payload size checking
payloadSizeOk : (s : String) -> (max : Nat) ->
                {auto 0 _ : So (length s <= max)} ->
                So (length s <= max)

||| Create a validated message payload
public export
mkPayload : (maxSize : Nat) -> String -> Maybe MessagePayload
mkPayload maxSize content with (choose (length content <= maxSize))
  mkPayload maxSize content | Left prf = Just (MkPayload content maxSize prf)
  mkPayload maxSize content | Right _  = Nothing

||| Create a payload with the default max size
public export
mkDefaultPayload : String -> Maybe MessagePayload
mkDefaultPayload = mkPayload DefaultMaxMessageSize

-- ============================================================================
-- WEBSOCKET CONNECTION STATE (RFC 6455 Section 4)
-- ============================================================================

||| WebSocket connection state machine
public export
data ConnectionState =
    Connecting
  | Open
  | Closing
  | Closed

public export
Show ConnectionState where
  show Connecting = "CONNECTING"
  show Open       = "OPEN"
  show Closing    = "CLOSING"
  show Closed     = "CLOSED"

public export
Eq ConnectionState where
  Connecting == Connecting = True
  Open == Open = True
  Closing == Closing = True
  Closed == Closed = True
  _ == _ = False

||| Valid state transitions
public export
canTransition : ConnectionState -> ConnectionState -> Bool
canTransition Connecting Open    = True
canTransition Connecting Closed  = True   -- Connection failed
canTransition Open       Closing = True   -- Close initiated
canTransition Open       Closed  = True   -- Abrupt close
canTransition Closing    Closed  = True   -- Close completed
canTransition _          _       = False

-- ============================================================================
-- FRAME VALIDATION
-- ============================================================================

||| A validated WebSocket frame descriptor
public export
record FrameDescriptor where
  constructor MkFrame
  opcode     : Opcode
  fin        : Bool       -- Final fragment
  payloadLen : Nat

||| Validate a frame: control frames must be <= 125 bytes and not fragmented
public export
validateFrame : FrameDescriptor -> Bool
validateFrame frame =
  if isControlFrame frame.opcode
  then frame.payloadLen <= MaxControlPayload && frame.fin
  else True

||| Create a validated text frame descriptor
public export
mkTextFrame : Nat -> Bool -> Maybe FrameDescriptor
mkTextFrame len fin =
  let frame = MkFrame TextFrame fin len
  in if validateFrame frame then Just frame else Nothing

||| Create a validated binary frame descriptor
public export
mkBinaryFrame : Nat -> Bool -> Maybe FrameDescriptor
mkBinaryFrame len fin =
  let frame = MkFrame BinaryFrame fin len
  in if validateFrame frame then Just frame else Nothing

||| Create a validated close frame
public export
mkCloseFrame : CloseCode -> String -> Maybe FrameDescriptor
mkCloseFrame code reason =
  if not (isSendable code) then Nothing
  else let payloadLen = 2 + length reason  -- 2 bytes for code + reason
       in if payloadLen > MaxControlPayload then Nothing
          else Just (MkFrame Close True payloadLen)

||| Create a ping frame
public export
mkPingFrame : String -> Maybe FrameDescriptor
mkPingFrame payload =
  if length payload > MaxControlPayload then Nothing
  else Just (MkFrame Ping True (length payload))

||| Create a pong frame (must echo ping payload)
public export
mkPongFrame : String -> Maybe FrameDescriptor
mkPongFrame payload =
  if length payload > MaxControlPayload then Nothing
  else Just (MkFrame Pong True (length payload))
