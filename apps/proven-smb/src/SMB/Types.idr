-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SMB.Types: Core protocol types for SMB2/3.
--
-- Defines closed sum types for SMB2 commands (the 16 command codes from
-- MS-SMB2 Section 2.2), dialect versions (SMB 2.0.2 through SMB 3.1.1),
-- and share types returned during tree connect.

module SMB.Types

%default total

-- ============================================================================
-- SMB2 commands (MS-SMB2 Section 2.2)
-- ============================================================================

||| SMB2/3 command codes from MS-SMB2 specification Section 2.2.
||| Each command has a request and response pair; this type represents
||| the command identity independent of direction.
public export
data Command : Type where
  ||| Protocol version negotiation (Section 2.2.3).
  Negotiate      : Command
  ||| Authenticate and establish a session (Section 2.2.5).
  SessionSetup   : Command
  ||| Terminate an authenticated session (Section 2.2.7).
  Logoff         : Command
  ||| Connect to a shared resource (Section 2.2.9).
  TreeConnect    : Command
  ||| Disconnect from a shared resource (Section 2.2.11).
  TreeDisconnect : Command
  ||| Open or create a file or named pipe (Section 2.2.13).
  Create         : Command
  ||| Close an open file handle (Section 2.2.15).
  Close          : Command
  ||| Read data from a file or named pipe (Section 2.2.19).
  Read           : Command
  ||| Write data to a file or named pipe (Section 2.2.21).
  Write          : Command
  ||| Acquire or release a byte-range lock (Section 2.2.26).
  Lock           : Command
  ||| Issue a device-specific I/O control (Section 2.2.31).
  Ioctl          : Command
  ||| Cancel a pending request (Section 2.2.30).
  Cancel         : Command
  ||| Enumerate directory contents (Section 2.2.33).
  QueryDirectory : Command
  ||| Register for change notifications on a directory (Section 2.2.35).
  ChangeNotify   : Command
  ||| Query file or filesystem metadata (Section 2.2.37).
  QueryInfo      : Command
  ||| Set file or filesystem metadata (Section 2.2.39).
  SetInfo        : Command

public export
Eq Command where
  Negotiate      == Negotiate      = True
  SessionSetup   == SessionSetup   = True
  Logoff         == Logoff         = True
  TreeConnect    == TreeConnect    = True
  TreeDisconnect == TreeDisconnect = True
  Create         == Create         = True
  Close          == Close          = True
  Read           == Read           = True
  Write          == Write          = True
  Lock           == Lock           = True
  Ioctl          == Ioctl          = True
  Cancel         == Cancel         = True
  QueryDirectory == QueryDirectory = True
  ChangeNotify   == ChangeNotify   = True
  QueryInfo      == QueryInfo      = True
  SetInfo        == SetInfo        = True
  _              == _              = False

public export
Show Command where
  show Negotiate      = "SMB2_NEGOTIATE"
  show SessionSetup   = "SMB2_SESSION_SETUP"
  show Logoff         = "SMB2_LOGOFF"
  show TreeConnect    = "SMB2_TREE_CONNECT"
  show TreeDisconnect = "SMB2_TREE_DISCONNECT"
  show Create         = "SMB2_CREATE"
  show Close          = "SMB2_CLOSE"
  show Read           = "SMB2_READ"
  show Write          = "SMB2_WRITE"
  show Lock           = "SMB2_LOCK"
  show Ioctl          = "SMB2_IOCTL"
  show Cancel         = "SMB2_CANCEL"
  show QueryDirectory = "SMB2_QUERY_DIRECTORY"
  show ChangeNotify   = "SMB2_CHANGE_NOTIFY"
  show QueryInfo      = "SMB2_QUERY_INFO"
  show SetInfo        = "SMB2_SET_INFO"

-- ============================================================================
-- SMB2/3 dialect versions
-- ============================================================================

||| SMB protocol dialect versions.
||| The dialect is negotiated during the initial NEGOTIATE exchange and
||| determines available features and security requirements.
public export
data Dialect : Type where
  ||| SMB 2.0.2: base SMB2 protocol (Windows Vista SP1).
  SMB2_0_2 : Dialect
  ||| SMB 2.1: minor enhancements (Windows 7).
  SMB2_1   : Dialect
  ||| SMB 3.0: multi-channel, encryption (Windows 8).
  SMB3_0   : Dialect
  ||| SMB 3.0.2: minor fixes (Windows 8.1).
  SMB3_0_2 : Dialect
  ||| SMB 3.1.1: mandatory integrity, pre-auth negotiation (Windows 10).
  SMB3_1_1 : Dialect

public export
Eq Dialect where
  SMB2_0_2 == SMB2_0_2 = True
  SMB2_1   == SMB2_1   = True
  SMB3_0   == SMB3_0   = True
  SMB3_0_2 == SMB3_0_2 = True
  SMB3_1_1 == SMB3_1_1 = True
  _        == _        = False

public export
Show Dialect where
  show SMB2_0_2 = "SMB 2.0.2"
  show SMB2_1   = "SMB 2.1"
  show SMB3_0   = "SMB 3.0"
  show SMB3_0_2 = "SMB 3.0.2"
  show SMB3_1_1 = "SMB 3.1.1"

-- ============================================================================
-- Share types
-- ============================================================================

||| SMB2 share types returned in TREE_CONNECT responses.
||| Determines how the client should interact with the shared resource.
public export
data ShareType : Type where
  ||| Disk share: filesystem access (SMB2_SHARE_TYPE_DISK).
  Disk  : ShareType
  ||| Named pipe share: IPC communication (SMB2_SHARE_TYPE_PIPE).
  Pipe  : ShareType
  ||| Print share: printer access (SMB2_SHARE_TYPE_PRINT).
  Print : ShareType

public export
Eq ShareType where
  Disk  == Disk  = True
  Pipe  == Pipe  = True
  Print == Print = True
  _     == _     = False

public export
Show ShareType where
  show Disk  = "SMB2_SHARE_TYPE_DISK"
  show Pipe  = "SMB2_SHARE_TYPE_PIPE"
  show Print = "SMB2_SHARE_TYPE_PRINT"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All SMB2 commands.
public export
allCommands : List Command
allCommands = [Negotiate, SessionSetup, Logoff, TreeConnect, TreeDisconnect,
               Create, Close, Read, Write, Lock, Ioctl, Cancel,
               QueryDirectory, ChangeNotify, QueryInfo, SetInfo]

||| All SMB dialect versions.
public export
allDialects : List Dialect
allDialects = [SMB2_0_2, SMB2_1, SMB3_0, SMB3_0_2, SMB3_1_1]

||| All share types.
public export
allShareTypes : List ShareType
allShareTypes = [Disk, Pipe, Print]
