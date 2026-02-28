-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- LPD Protocol State Machine (RFC 1179)
--
-- The LPD protocol operates in a request-response pattern. During job
-- reception, the server moves through states: Idle -> ReceivingControlFile
-- -> ReceivingDataFile -> Idle. Each state constrains what operations
-- are valid. Invalid transitions produce typed error values.

module LPD.Protocol

import LPD.Command
import LPD.Job

%default total

-- ============================================================================
-- Protocol states
-- ============================================================================

||| The states of the LPD job reception protocol.
||| After a ReceiveJob command, the server expects a control file followed
||| by one or more data files.
public export
data ProtocolState : Type where
  ||| Idle: waiting for a top-level command.
  Idle                : ProtocolState
  ||| Receiving control file: the server expects control file data.
  ReceivingControlFile : (queueName : String) -> (expectedSize : Nat) -> ProtocolState
  ||| Receiving data file: the server expects print data.
  ReceivingDataFile    : (queueName : String) -> (expectedSize : Nat) -> ProtocolState

public export
Eq ProtocolState where
  Idle                     == Idle                     = True
  (ReceivingControlFile q1 _) == (ReceivingControlFile q2 _) = q1 == q2
  (ReceivingDataFile q1 _)    == (ReceivingDataFile q2 _)    = q1 == q2
  _                        == _                        = False

public export
Show ProtocolState where
  show Idle                       = "Idle"
  show (ReceivingControlFile q s) = "ReceivingControlFile(" ++ q
                                     ++ ", " ++ show s ++ " bytes)"
  show (ReceivingDataFile q s)    = "ReceivingDataFile(" ++ q
                                     ++ ", " ++ show s ++ " bytes)"

-- ============================================================================
-- Protocol actions
-- ============================================================================

||| Actions the protocol state machine can produce.
public export
data ProtocolAction : Type where
  ||| Send an acknowledgement byte (0x00 = success).
  SendAck          : ProtocolAction
  ||| Send a negative acknowledgement byte (0x01 = error).
  SendNack         : ProtocolAction
  ||| Process the received control file.
  ProcessControl   : (content : String) -> ProtocolAction
  ||| Process the received data file and enqueue a print job.
  ProcessData      : (content : String) -> ProtocolAction
  ||| Report the queue state to the client.
  ReportQueueState : (queueName : String) -> (verbose : Bool) -> ProtocolAction
  ||| Start printing waiting jobs.
  StartPrinting    : (queueName : String) -> ProtocolAction
  ||| Remove specified jobs from the queue.
  RemoveFromQueue  : (queueName : String) -> (agent : String)
                  -> (jobIds : List String) -> ProtocolAction

public export
Show ProtocolAction where
  show SendAck                = "ACK"
  show SendNack               = "NACK"
  show (ProcessControl _)     = "ProcessControl"
  show (ProcessData _)        = "ProcessData"
  show (ReportQueueState q v) = "ReportQueue(" ++ q ++ ", verbose=" ++ show v ++ ")"
  show (StartPrinting q)      = "StartPrinting(" ++ q ++ ")"
  show (RemoveFromQueue q a _) = "RemoveJobs(" ++ q ++ ", " ++ a ++ ")"

-- ============================================================================
-- Protocol errors
-- ============================================================================

||| Errors from protocol state transitions.
public export
data ProtocolError : Type where
  ||| A command was received in an invalid state.
  InvalidState    : (command : String) -> (state : ProtocolState) -> ProtocolError
  ||| The received data size does not match the expected size.
  SizeMismatch    : (expected : Nat) -> (actual : Nat) -> ProtocolError
  ||| The control file is malformed.
  MalformedControl : (reason : String) -> ProtocolError

public export
Show ProtocolError where
  show (InvalidState cmd st)   = "Invalid command '" ++ cmd ++ "' in state " ++ show st
  show (SizeMismatch exp act)  = "Size mismatch: expected " ++ show exp
                                 ++ ", got " ++ show act
  show (MalformedControl r)    = "Malformed control file: " ++ r

-- ============================================================================
-- Protocol transitions
-- ============================================================================

||| Result of a protocol state transition.
public export
record TransitionResult where
  constructor MkTransition
  ||| The new protocol state.
  newState : ProtocolState
  ||| Actions to perform.
  actions  : List ProtocolAction

||| Handle a top-level command in the Idle state.
||| This function is total: every command in every state is handled.
public export
handleCommand : ProtocolState -> Command -> TransitionResult
-- Idle state: accept top-level commands
handleCommand Idle (PrintJob q) = MkTransition Idle
  [SendAck, StartPrinting q]
handleCommand Idle (ReceiveJob q) = MkTransition Idle
  [SendAck]  -- Ready for sub-commands
handleCommand Idle (ShortQueueState q _) = MkTransition Idle
  [ReportQueueState q False]
handleCommand Idle (LongQueueState q _) = MkTransition Idle
  [ReportQueueState q True]
handleCommand Idle (RemoveJobs q a js) = MkTransition Idle
  [RemoveFromQueue q a js]
-- Receiving states: only sub-commands should be used, not top-level commands
handleCommand st _ = MkTransition st [SendNack]

||| Handle a sub-command during the job reception phase.
public export
handleSubCommand : ProtocolState -> SubCommand -> TransitionResult
-- AbortJob: return to Idle regardless of current state
handleSubCommand _ AbortJob = MkTransition Idle [SendAck]
-- ReceiveControlFile: enter control file reception
handleSubCommand Idle (ReceiveControlFile count name) =
  MkTransition (ReceivingControlFile name count) [SendAck]
handleSubCommand (ReceivingControlFile _ _) (ReceiveControlFile count name) =
  MkTransition (ReceivingControlFile name count) [SendAck]
handleSubCommand (ReceivingDataFile _ _) (ReceiveControlFile count name) =
  MkTransition (ReceivingControlFile name count) [SendAck]
-- ReceiveDataFile: enter data file reception
handleSubCommand Idle (ReceiveDataFile count name) =
  MkTransition (ReceivingDataFile name count) [SendAck]
handleSubCommand (ReceivingControlFile _ _) (ReceiveDataFile count name) =
  MkTransition (ReceivingDataFile name count) [SendAck]
handleSubCommand (ReceivingDataFile _ _) (ReceiveDataFile count name) =
  MkTransition (ReceivingDataFile name count) [SendAck]

||| Complete the reception of a control file with its content.
public export
completeControlFile : ProtocolState -> String -> Either ProtocolError TransitionResult
completeControlFile (ReceivingControlFile _ expected) content =
  let actual = length content
  in if actual == expected
       then Right (MkTransition Idle [SendAck, ProcessControl content])
       else Left (SizeMismatch expected actual)
completeControlFile st _ = Left (InvalidState "control file data" st)

||| Complete the reception of a data file with its content.
public export
completeDataFile : ProtocolState -> String -> Either ProtocolError TransitionResult
completeDataFile (ReceivingDataFile _ expected) content =
  let actual = length content
  in if actual == expected
       then Right (MkTransition Idle [SendAck, ProcessData content])
       else Left (SizeMismatch expected actual)
completeDataFile st _ = Left (InvalidState "data file data" st)

||| Check if the protocol is idle (ready for new commands).
public export
isIdle : ProtocolState -> Bool
isIdle Idle = True
isIdle _    = False

||| Check if the protocol is in a receiving state.
public export
isReceiving : ProtocolState -> Bool
isReceiving (ReceivingControlFile _ _) = True
isReceiving (ReceivingDataFile _ _)    = True
isReceiving _                         = False
