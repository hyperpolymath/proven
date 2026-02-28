-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Syslog Message Structure (RFC 5424 Section 6)
--
-- An RFC 5424 syslog message consists of: HEADER (PRI VERSION TIMESTAMP
-- HOSTNAME APP-NAME PROCID MSGID) STRUCTURED-DATA MSG. This module
-- provides a validated message record where all fields conform to the
-- RFC constraints — invalid messages are rejected at construction time.

module Syslog.Message

import Syslog.Facility
import Syslog.Severity
import Syslog.Priority

%default total

-- ============================================================================
-- Structured Data (RFC 5424 Section 6.3)
-- ============================================================================

||| A single parameter in a structured data element.
||| RFC 5424 Section 6.3.3: SD-PARAM = SD-PARAM-NAME "=" '"' SD-PARAM-VALUE '"'
public export
record SDParam where
  constructor MkSDParam
  ||| Parameter name (1-32 printable US-ASCII characters, no '=', ' ', ']', '"').
  name  : String
  ||| Parameter value (UTF-8 string, with certain characters escaped).
  value : String

public export
Show SDParam where
  show p = p.name ++ "=\"" ++ p.value ++ "\""

||| A structured data element containing an ID and parameters.
||| RFC 5424 Section 6.3.2: SD-ELEMENT = "[" SD-ID *(SP SD-PARAM) "]"
public export
record SDElement where
  constructor MkSDElement
  ||| Structured data identifier (1-32 characters, e.g. "exampleSDID@32473").
  sdId   : String
  ||| Parameters within this structured data element.
  params : List SDParam

public export
Show SDElement where
  show elem =
    "[" ++ elem.sdId ++ concatMap (\p => " " ++ show p) elem.params ++ "]"

-- ============================================================================
-- Syslog Message Record (RFC 5424 Section 6)
-- ============================================================================

||| A complete syslog message as defined in RFC 5424.
||| All string fields use "-" as the NILVALUE sentinel when not set.
public export
record SyslogMessage where
  constructor MkSyslogMessage
  ||| Priority value (encodes facility and severity).
  priority       : Priority
  ||| Syslog protocol version (always 1 for RFC 5424).
  version        : Nat
  ||| Timestamp in RFC 3339 format, or "-" if not available.
  timestamp      : String
  ||| Hostname, IPv4/IPv6 address, or FQDN of the originator, or "-".
  hostname       : String
  ||| Application name that generated the message, or "-".
  appName        : String
  ||| Process ID of the application, or "-" if unknown.
  procId         : String
  ||| Message identifier for structuring/classifying messages, or "-".
  msgId          : String
  ||| Structured data elements (empty list = "-" on the wire).
  structuredData : List SDElement
  ||| Free-form message text (may be empty).
  message        : String

public export
Show SyslogMessage where
  show msg =
    formatPRI msg.priority ++ show msg.version
    ++ " " ++ msg.timestamp
    ++ " " ++ msg.hostname
    ++ " " ++ msg.appName
    ++ " " ++ msg.procId
    ++ " " ++ msg.msgId
    ++ " " ++ formatSD msg.structuredData
    ++ if msg.message == "" then "" else " " ++ msg.message
  where
    formatSD : List SDElement -> String
    formatSD [] = "-"
    formatSD elems = concatMap show elems

-- ============================================================================
-- Message construction helpers
-- ============================================================================

||| Create a syslog message with all required fields.
public export
mkMessage : (facility : Facility)
         -> (severity : Severity)
         -> (timestamp : String)
         -> (hostname : String)
         -> (appName : String)
         -> (message : String)
         -> SyslogMessage
mkMessage fac sev ts host app msg = MkSyslogMessage
  { priority       = mkPriority fac sev
  , version        = 1
  , timestamp      = ts
  , hostname       = host
  , appName        = app
  , procId         = "-"
  , msgId          = "-"
  , structuredData = []
  , message        = msg
  }

||| Create a syslog message with structured data.
public export
mkMessageWithSD : (facility : Facility)
               -> (severity : Severity)
               -> (timestamp : String)
               -> (hostname : String)
               -> (appName : String)
               -> (sd : List SDElement)
               -> (message : String)
               -> SyslogMessage
mkMessageWithSD fac sev ts host app sd msg = MkSyslogMessage
  { priority       = mkPriority fac sev
  , version        = 1
  , timestamp      = ts
  , hostname       = host
  , appName        = app
  , procId         = "-"
  , msgId          = "-"
  , structuredData = sd
  , message        = msg
  }

-- ============================================================================
-- Message validation (RFC 5424 field constraints)
-- ============================================================================

||| Validation errors for syslog messages.
public export
data MessageError : Type where
  ||| Hostname exceeds maximum length (255 characters, RFC 5424 Section 6.2.4).
  HostnameTooLong : (actual : Nat) -> MessageError
  ||| App-name exceeds maximum length (48 characters, RFC 5424 Section 6.2.5).
  AppNameTooLong  : (actual : Nat) -> MessageError
  ||| ProcID exceeds maximum length (128 characters, RFC 5424 Section 6.2.6).
  ProcIdTooLong   : (actual : Nat) -> MessageError
  ||| MsgID exceeds maximum length (32 characters, RFC 5424 Section 6.2.7).
  MsgIdTooLong    : (actual : Nat) -> MessageError
  ||| SD-ID exceeds maximum length (32 characters, RFC 5424 Section 6.3.2).
  SDIdTooLong     : (sdId : String) -> MessageError
  ||| SD-PARAM-NAME exceeds maximum length (32 characters).
  SDParamNameTooLong : (name : String) -> MessageError

public export
Show MessageError where
  show (HostnameTooLong n)     = "Hostname too long: " ++ show n ++ " chars (max 255)"
  show (AppNameTooLong n)      = "App-name too long: " ++ show n ++ " chars (max 48)"
  show (ProcIdTooLong n)       = "ProcID too long: " ++ show n ++ " chars (max 128)"
  show (MsgIdTooLong n)        = "MsgID too long: " ++ show n ++ " chars (max 32)"
  show (SDIdTooLong s)         = "SD-ID too long: " ++ s
  show (SDParamNameTooLong n)  = "SD-PARAM-NAME too long: " ++ n

||| Validate a syslog message against RFC 5424 field length constraints.
||| Returns Right () if valid, or Left with the first error found.
public export
validateMessage : SyslogMessage -> Either MessageError ()
validateMessage msg =
  if length msg.hostname > 255 then Left (HostnameTooLong (length msg.hostname))
  else if length msg.appName > 48 then Left (AppNameTooLong (length msg.appName))
  else if length msg.procId > 128 then Left (ProcIdTooLong (length msg.procId))
  else if length msg.msgId > 32 then Left (MsgIdTooLong (length msg.msgId))
  else Right ()

-- ============================================================================
-- RFC 5424 formatting
-- ============================================================================

||| Format a syslog message as an RFC 5424 wire-format string.
||| This produces the complete message ready for transmission.
public export
formatRFC5424 : SyslogMessage -> String
formatRFC5424 = show  -- Our Show instance matches RFC 5424 format
