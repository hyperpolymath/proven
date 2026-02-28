-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- TFTP Transfer State Machine (RFC 1350 Section 2)
--
-- A TFTP transfer progresses through a sequence of states:
-- Idle -> Reading/Writing -> Complete (or Error at any point).
-- The state machine tracks the current block number, retry count,
-- and enforces valid transitions. Invalid transitions (e.g., receiving
-- DATA while Idle) are rejected — they cannot cause a crash.

module TFTP.Transfer

import TFTP.Opcode
import TFTP.Error
import TFTP.Mode
import TFTP.Packet

%default total

-- ============================================================================
-- Transfer states
-- ============================================================================

||| The 5 states of a TFTP transfer lifecycle.
public export
data TransferState : Type where
  ||| No transfer in progress. Ready to accept RRQ or WRQ.
  Idle     : TransferState
  ||| Reading a file from the server. Expecting DATA packets.
  Reading  : TransferState
  ||| Writing a file to the server. Sending DATA packets.
  Writing  : TransferState
  ||| An error occurred. Transfer is terminated.
  InError  : TransferState
  ||| Transfer completed successfully. All data transferred.
  Complete : TransferState

public export
Eq TransferState where
  Idle     == Idle     = True
  Reading  == Reading  = True
  Writing  == Writing  = True
  InError  == InError  = True
  Complete == Complete = True
  _        == _        = False

public export
Show TransferState where
  show Idle     = "Idle"
  show Reading  = "Reading"
  show Writing  = "Writing"
  show InError  = "Error"
  show Complete = "Complete"

-- ============================================================================
-- Transfer events
-- ============================================================================

||| Events that drive transfer state transitions.
public export
data TransferEvent : Type where
  ||| A Read Request was sent/received. Begins a read transfer.
  StartRead    : TransferEvent
  ||| A Write Request was sent/received. Begins a write transfer.
  StartWrite   : TransferEvent
  ||| A DATA packet was received (during read) or sent (during write).
  DataReceived : (isLastBlock : Bool) -> TransferEvent
  ||| An ACK packet was received.
  AckReceived  : TransferEvent
  ||| An ERROR packet was received.
  ErrorOccurred : TransferEvent
  ||| Retry timeout expired and max retries exceeded.
  RetryExhausted : TransferEvent

public export
Show TransferEvent where
  show StartRead           = "StartRead"
  show StartWrite          = "StartWrite"
  show (DataReceived last) = "DataReceived(last=" ++ show last ++ ")"
  show AckReceived         = "AckReceived"
  show ErrorOccurred       = "ErrorOccurred"
  show RetryExhausted      = "RetryExhausted"

-- ============================================================================
-- Transfer transition function
-- ============================================================================

||| Result of a transfer state transition.
public export
record TransferTransition where
  constructor MkTransferTransition
  ||| The new state after the transition.
  newState : TransferState
  ||| Whether the transition was valid.
  valid    : Bool

||| Transfer state transition function (total over all state/event pairs).
||| Invalid transitions produce the same state with valid=False.
public export
transferTransition : TransferState -> TransferEvent -> TransferTransition
-- From Idle: only StartRead and StartWrite are valid
transferTransition Idle StartRead      = MkTransferTransition Reading True
transferTransition Idle StartWrite     = MkTransferTransition Writing True
transferTransition Idle _              = MkTransferTransition Idle False
-- From Reading: DATA and ERROR events
transferTransition Reading (DataReceived True)  = MkTransferTransition Complete True
transferTransition Reading (DataReceived False) = MkTransferTransition Reading True
transferTransition Reading ErrorOccurred        = MkTransferTransition InError True
transferTransition Reading RetryExhausted       = MkTransferTransition InError True
transferTransition Reading _                    = MkTransferTransition Reading False
-- From Writing: ACK and ERROR events
transferTransition Writing AckReceived    = MkTransferTransition Writing True
transferTransition Writing ErrorOccurred  = MkTransferTransition InError True
transferTransition Writing RetryExhausted = MkTransferTransition InError True
transferTransition Writing (DataReceived True) = MkTransferTransition Complete True
transferTransition Writing _              = MkTransferTransition Writing False
-- From InError and Complete: terminal states (no valid transitions)
transferTransition InError  _ = MkTransferTransition InError False
transferTransition Complete _ = MkTransferTransition Complete False

-- ============================================================================
-- Transfer session record
-- ============================================================================

||| Complete state for a TFTP transfer in progress.
public export
record TransferSession where
  constructor MkTransferSession
  ||| Current transfer state.
  state       : TransferState
  ||| Filename being transferred.
  filename    : String
  ||| Transfer mode.
  mode        : TransferMode
  ||| Current block number (0 for WRQ-ACK, 1+ for data blocks).
  currentBlock : Bits16
  ||| Number of retries for the current block.
  retryCount  : Nat
  ||| Total bytes transferred so far.
  bytesTotal  : Nat
  ||| Last error (if in error state).
  lastError   : Maybe ErrorInfo

||| Maximum number of retries before giving up.
public export
maxRetries : Nat
maxRetries = 5

||| Timeout in seconds before retransmitting.
public export
timeoutSecs : Nat
timeoutSecs = 5

||| Create a new transfer session for a read request.
public export
newReadSession : (filename : String) -> (mode : TransferMode) -> TransferSession
newReadSession fn m = MkTransferSession
  { state        = Reading
  , filename     = fn
  , mode         = m
  , currentBlock = 1    -- First DATA block expected is #1
  , retryCount   = 0
  , bytesTotal   = 0
  , lastError    = Nothing
  }

||| Create a new transfer session for a write request.
public export
newWriteSession : (filename : String) -> (mode : TransferMode) -> TransferSession
newWriteSession fn m = MkTransferSession
  { state        = Writing
  , filename     = fn
  , mode         = m
  , currentBlock = 0    -- ACK #0 acknowledges the WRQ
  , retryCount   = 0
  , bytesTotal   = 0
  , lastError    = Nothing
  }

-- ============================================================================
-- Session operations
-- ============================================================================

||| Apply an event to a transfer session, returning the updated session.
public export
applyTransferEvent : TransferSession -> TransferEvent -> TransferSession
applyTransferEvent session event =
  let result = transferTransition session.state event
  in if result.valid
       then { state := result.newState } session
       else session

||| Record receipt of a DATA block, updating block number and byte count.
public export
recordDataBlock : TransferSession -> (blockNum : Bits16) -> (blockSize : Nat) -> TransferSession
recordDataBlock session blk size =
  { currentBlock := blk
  , bytesTotal   := session.bytesTotal + size
  , retryCount   := 0  -- Reset retry counter on successful receive
  } session

||| Increment the retry counter. Returns True if retries are exhausted.
public export
incrementRetry : TransferSession -> (TransferSession, Bool)
incrementRetry session =
  let newCount = session.retryCount + 1
      exhausted = newCount > maxRetries
  in ({ retryCount := newCount } session, exhausted)

||| Record an error in the session.
public export
recordError : TransferSession -> TFTPError -> TransferSession
recordError session err =
  { state     := InError
  , lastError := Just (mkError err)
  } session

||| Check if the transfer is in a terminal state (Complete or InError).
public export
isTerminal : TransferSession -> Bool
isTerminal session =
  session.state == Complete || session.state == InError

||| Check if the transfer completed successfully.
public export
isComplete : TransferSession -> Bool
isComplete session = session.state == Complete

||| Get a summary of the transfer session.
public export
sessionSummary : TransferSession -> String
sessionSummary s =
  "Transfer{" ++ show s.state
  ++ ", file=\"" ++ s.filename ++ "\""
  ++ ", mode=" ++ show s.mode
  ++ ", block=" ++ show (cast {to=Nat} s.currentBlock)
  ++ ", bytes=" ++ show s.bytesTotal
  ++ ", retries=" ++ show s.retryCount
  ++ "}"
