-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for TACACS+ (RFC 8907).
||| All types are closed sum types with Show instances.
module TACACS.Types

%default total

---------------------------------------------------------------------------
-- Packet Type (RFC 8907 Section 4.1)
---------------------------------------------------------------------------

||| TACACS+ packet types identifying the service category.
public export
data PacketType : Type where
  Authentication : PacketType
  Authorization  : PacketType
  Accounting     : PacketType

public export
Show PacketType where
  show Authentication = "Authentication"
  show Authorization  = "Authorization"
  show Accounting     = "Accounting"

---------------------------------------------------------------------------
-- Authentication Type (RFC 8907 Section 5.4.2)
---------------------------------------------------------------------------

||| Authentication method types.
public export
data AuthenType : Type where
  ASCII    : AuthenType
  PAP      : AuthenType
  CHAP     : AuthenType
  MSCHAPv1 : AuthenType
  MSCHAPv2 : AuthenType

public export
Show AuthenType where
  show ASCII    = "ASCII"
  show PAP      = "PAP"
  show CHAP     = "CHAP"
  show MSCHAPv1 = "MS-CHAPv1"
  show MSCHAPv2 = "MS-CHAPv2"

---------------------------------------------------------------------------
-- Authentication Action
---------------------------------------------------------------------------

||| Authentication actions requested by the client.
public export
data AuthenAction : Type where
  Login      : AuthenAction
  ChangePass : AuthenAction
  SendAuth   : AuthenAction

public export
Show AuthenAction where
  show Login      = "Login"
  show ChangePass = "ChangePass"
  show SendAuth   = "SendAuth"

---------------------------------------------------------------------------
-- Authentication Status
---------------------------------------------------------------------------

||| Authentication reply status values.
public export
data AuthenStatus : Type where
  Pass    : AuthenStatus
  Fail    : AuthenStatus
  GetData : AuthenStatus
  GetUser : AuthenStatus
  GetPass : AuthenStatus
  Restart : AuthenStatus
  AuthenError  : AuthenStatus
  Follow  : AuthenStatus

public export
Show AuthenStatus where
  show Pass    = "PASS"
  show Fail    = "FAIL"
  show GetData = "GETDATA"
  show GetUser = "GETUSER"
  show GetPass = "GETPASS"
  show Restart = "RESTART"
  show AuthenError  = "ERROR"
  show Follow  = "FOLLOW"

---------------------------------------------------------------------------
-- Authorization Status
---------------------------------------------------------------------------

||| Authorization reply status values.
public export
data AuthorStatus : Type where
  PassAdd  : AuthorStatus
  PassRepl : AuthorStatus
  AuthorFail   : AuthorStatus
  AuthorError  : AuthorStatus
  AuthorFollow : AuthorStatus

public export
Show AuthorStatus where
  show PassAdd     = "PASS_ADD"
  show PassRepl    = "PASS_REPL"
  show AuthorFail   = "FAIL"
  show AuthorError  = "ERROR"
  show AuthorFollow = "FOLLOW"

---------------------------------------------------------------------------
-- Accounting Status
---------------------------------------------------------------------------

||| Accounting reply status values.
public export
data AcctStatus : Type where
  AcctSuccess : AcctStatus
  AcctError   : AcctStatus
  AcctFollow  : AcctStatus

public export
Show AcctStatus where
  show AcctSuccess = "SUCCESS"
  show AcctError   = "ERROR"
  show AcctFollow  = "FOLLOW"

---------------------------------------------------------------------------
-- Accounting Flag
---------------------------------------------------------------------------

||| Accounting record flags indicating record type.
public export
data AcctFlag : Type where
  Start    : AcctFlag
  Stop     : AcctFlag
  Watchdog : AcctFlag

public export
Show AcctFlag where
  show Start    = "START"
  show Stop     = "STOP"
  show Watchdog = "WATCHDOG"
