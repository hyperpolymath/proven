-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- LPD Commands (RFC 1179 Section 5)
--
-- The Line Printer Daemon protocol uses single-byte command codes
-- followed by operands. All five commands are represented as a closed
-- sum type with their byte codes. Unknown commands parse to Nothing.

module LPD.Command

%default total

-- ============================================================================
-- LPD Commands (RFC 1179 Section 5)
-- ============================================================================

||| LPD protocol commands as defined in RFC 1179.
||| Each command is initiated by a single byte code from the client.
public export
data Command : Type where
  ||| 0x01: Print any waiting jobs on the named queue.
  ||| The server checks the queue and prints pending jobs.
  PrintJob       : (queueName : String) -> Command
  ||| 0x02: Receive a print job. The server enters job-receive mode.
  ||| Subsequent sub-commands transfer control and data files.
  ReceiveJob     : (queueName : String) -> Command
  ||| 0x03: Send short queue state (one line per job).
  ||| Returns a brief listing of the specified queue.
  ShortQueueState : (queueName : String) -> (jobList : List String) -> Command
  ||| 0x04: Send long queue state (verbose, multiple lines per job).
  ||| Returns detailed status for each job in the queue.
  LongQueueState  : (queueName : String) -> (jobList : List String) -> Command
  ||| 0x05: Remove jobs from the queue.
  ||| The agent and optional job IDs specify which jobs to remove.
  RemoveJobs     : (queueName : String) -> (agent : String)
                -> (jobIds : List String) -> Command

public export
Eq Command where
  (PrintJob a)           == (PrintJob b)           = a == b
  (ReceiveJob a)         == (ReceiveJob b)         = a == b
  (ShortQueueState a _)  == (ShortQueueState b _)  = a == b
  (LongQueueState a _)   == (LongQueueState b _)   = a == b
  (RemoveJobs a _ _)     == (RemoveJobs b _ _)     = a == b
  _                      == _                      = False

public export
Show Command where
  show (PrintJob q)           = "PrintJob(" ++ q ++ ")"
  show (ReceiveJob q)         = "ReceiveJob(" ++ q ++ ")"
  show (ShortQueueState q _)  = "ShortQueueState(" ++ q ++ ")"
  show (LongQueueState q _)   = "LongQueueState(" ++ q ++ ")"
  show (RemoveJobs q a js)    = "RemoveJobs(" ++ q ++ ", " ++ a
                                ++ ", " ++ show (length js) ++ " jobs)"

-- ============================================================================
-- Byte codes (RFC 1179 Section 5)
-- ============================================================================

||| Convert a command to its byte code.
public export
commandCode : Command -> Bits8
commandCode (PrintJob _)          = 0x01
commandCode (ReceiveJob _)        = 0x02
commandCode (ShortQueueState _ _) = 0x03
commandCode (LongQueueState _ _)  = 0x04
commandCode (RemoveJobs _ _ _)    = 0x05

||| The human-readable name for a command code.
public export
commandName : Bits8 -> String
commandName 0x01 = "Print Waiting Jobs"
commandName 0x02 = "Receive Print Job"
commandName 0x03 = "Send Short Queue State"
commandName 0x04 = "Send Long Queue State"
commandName 0x05 = "Remove Jobs"
commandName _    = "Unknown"

-- ============================================================================
-- Receive sub-commands (RFC 1179 Section 6)
-- ============================================================================

||| Sub-commands used during the ReceiveJob phase.
||| These transfer control files and data files to the server.
public export
data SubCommand : Type where
  ||| 0x01: Abort job. Cancel the current receive operation.
  AbortJob         : SubCommand
  ||| 0x02: Receive control file. The control file specifies job attributes.
  ReceiveControlFile : (count : Nat) -> (name : String) -> SubCommand
  ||| 0x03: Receive data file. The data file contains the print data.
  ReceiveDataFile    : (count : Nat) -> (name : String) -> SubCommand

public export
Eq SubCommand where
  AbortJob               == AbortJob               = True
  (ReceiveControlFile _ n) == (ReceiveControlFile _ m) = n == m
  (ReceiveDataFile _ n)    == (ReceiveDataFile _ m)    = n == m
  _                      == _                      = False

public export
Show SubCommand where
  show AbortJob                = "AbortJob"
  show (ReceiveControlFile n f) = "ReceiveControlFile(" ++ show n ++ ", " ++ f ++ ")"
  show (ReceiveDataFile n f)    = "ReceiveDataFile(" ++ show n ++ ", " ++ f ++ ")"

||| Convert a sub-command to its byte code.
public export
subCommandCode : SubCommand -> Bits8
subCommandCode AbortJob               = 0x01
subCommandCode (ReceiveControlFile _ _) = 0x02
subCommandCode (ReceiveDataFile _ _)    = 0x03

-- ============================================================================
-- Command parsing
-- ============================================================================

||| Errors from parsing LPD commands.
public export
data CommandParseError : Type where
  ||| The command line is empty (no data received).
  EmptyCommand     : CommandParseError
  ||| The command byte is not a valid LPD command code.
  UnknownCommand   : (code : Bits8) -> CommandParseError
  ||| The queue name operand is missing.
  MissingQueueName : (code : Bits8) -> CommandParseError

public export
Show CommandParseError where
  show EmptyCommand        = "Empty command (no data)"
  show (UnknownCommand c)  = "Unknown command code: 0x" ++ showHex c
  show (MissingQueueName c) = "Missing queue name for command 0x" ++ showHex c
  where
    showHex : Bits8 -> String
    showHex b = show (cast {to=Nat} b)

||| Parse a command byte and operand string into a typed Command.
||| Returns Left for unknown or malformed commands (no crash).
public export
parseCommand : Bits8 -> String -> Either CommandParseError Command
parseCommand code operand =
  if length operand == 0 && code /= 0x03 && code /= 0x04
    then case code of
           0x01 => Left (MissingQueueName code)
           0x02 => Left (MissingQueueName code)
           0x05 => Left (MissingQueueName code)
           _    => Left (UnknownCommand code)
  else case code of
    0x01 => Right (PrintJob operand)
    0x02 => Right (ReceiveJob operand)
    0x03 => Right (ShortQueueState operand [])
    0x04 => Right (LongQueueState operand [])
    0x05 => Right (RemoveJobs operand "" [])
    _    => Left (UnknownCommand code)

||| List all valid command byte codes.
public export
allCommandCodes : List Bits8
allCommandCodes = [0x01, 0x02, 0x03, 0x04, 0x05]
