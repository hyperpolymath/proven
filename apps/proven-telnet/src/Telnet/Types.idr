-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for Telnet (RFC 854).
||| All types are closed sum types with Show instances.
module Telnet.Types

%default total

---------------------------------------------------------------------------
-- Telnet Command (RFC 854 Section 3)
---------------------------------------------------------------------------

||| Telnet protocol commands.
public export
data Command : Type where
  SE               : Command
  NOP              : Command
  DataMark         : Command
  Break            : Command
  InterruptProcess : Command
  AbortOutput      : Command
  AreYouThere      : Command
  EraseChar        : Command
  EraseLine        : Command
  GoAhead          : Command
  SB               : Command
  Will             : Command
  Wont             : Command
  Do               : Command
  Dont             : Command
  IAC              : Command

public export
Show Command where
  show SE               = "SE"
  show NOP              = "NOP"
  show DataMark         = "DATA MARK"
  show Break            = "BREAK"
  show InterruptProcess = "IP"
  show AbortOutput      = "AO"
  show AreYouThere      = "AYT"
  show EraseChar        = "EC"
  show EraseLine        = "EL"
  show GoAhead          = "GA"
  show SB               = "SB"
  show Will             = "WILL"
  show Wont             = "WONT"
  show Do               = "DO"
  show Dont             = "DONT"
  show IAC              = "IAC"

---------------------------------------------------------------------------
-- Telnet Option
---------------------------------------------------------------------------

||| Telnet negotiation options.
public export
data Option : Type where
  Echo              : Option
  SuppressGoAhead   : Option
  Status            : Option
  TimingMark        : Option
  TerminalType      : Option
  WindowSize        : Option
  TerminalSpeed     : Option
  RemoteFlowControl : Option
  Linemode          : Option
  Environment       : Option

public export
Show Option where
  show Echo              = "ECHO"
  show SuppressGoAhead   = "SUPPRESS-GO-AHEAD"
  show Status            = "STATUS"
  show TimingMark        = "TIMING-MARK"
  show TerminalType      = "TERMINAL-TYPE"
  show WindowSize        = "NAWS"
  show TerminalSpeed     = "TERMINAL-SPEED"
  show RemoteFlowControl = "REMOTE-FLOW-CONTROL"
  show Linemode          = "LINEMODE"
  show Environment       = "NEW-ENVIRON"

---------------------------------------------------------------------------
-- Negotiation State
---------------------------------------------------------------------------

||| State of a Telnet option negotiation.
public export
data NegotiationState : Type where
  Inactive : NegotiationState
  WillSent : NegotiationState
  DoSent   : NegotiationState
  Active   : NegotiationState

public export
Show NegotiationState where
  show Inactive = "Inactive"
  show WillSent = "WILL Sent"
  show DoSent   = "DO Sent"
  show Active   = "Active"
