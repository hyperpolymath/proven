-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- Syslog Facilities (RFC 5424 Section 6.2.1)
--
-- Syslog defines 24 facility codes indicating which subsystem generated
-- the message. Each facility is assigned a numeric code (0-23) that forms
-- the upper 5 bits of the priority value. The type system ensures only
-- valid facility codes can be constructed.

module Syslog.Facility

%default total

-- ============================================================================
-- Syslog Facilities (RFC 5424 Section 6.2.1, Table 1)
-- ============================================================================

||| The 24 syslog facility values as defined in RFC 5424.
||| These indicate which machine subsystem generated the log message.
public export
data Facility : Type where
  ||| Facility 0: Kernel messages.
  Kern     : Facility
  ||| Facility 1: User-level messages.
  User     : Facility
  ||| Facility 2: Mail system.
  Mail     : Facility
  ||| Facility 3: System daemons.
  Daemon   : Facility
  ||| Facility 4: Security/authorization messages.
  Auth     : Facility
  ||| Facility 5: Messages generated internally by syslogd.
  SyslogF  : Facility
  ||| Facility 6: Line printer subsystem.
  LPR      : Facility
  ||| Facility 7: Network news subsystem.
  News     : Facility
  ||| Facility 8: UUCP subsystem.
  UUCP     : Facility
  ||| Facility 9: Clock daemon (note 1).
  Cron     : Facility
  ||| Facility 10: Security/authorization messages (private).
  AuthPriv : Facility
  ||| Facility 11: FTP daemon.
  FTP      : Facility
  ||| Facility 12: NTP subsystem.
  NTPFac   : Facility
  ||| Facility 13: Log audit.
  Audit    : Facility
  ||| Facility 14: Log alert.
  Alert    : Facility
  ||| Facility 15: Clock daemon (note 2).
  Clock    : Facility
  ||| Facility 16: Local use 0.
  Local0   : Facility
  ||| Facility 17: Local use 1.
  Local1   : Facility
  ||| Facility 18: Local use 2.
  Local2   : Facility
  ||| Facility 19: Local use 3.
  Local3   : Facility
  ||| Facility 20: Local use 4.
  Local4   : Facility
  ||| Facility 21: Local use 5.
  Local5   : Facility
  ||| Facility 22: Local use 6.
  Local6   : Facility
  ||| Facility 23: Local use 7.
  Local7   : Facility

public export
Eq Facility where
  Kern     == Kern     = True
  User     == User     = True
  Mail     == Mail     = True
  Daemon   == Daemon   = True
  Auth     == Auth     = True
  SyslogF  == SyslogF  = True
  LPR      == LPR      = True
  News     == News     = True
  UUCP     == UUCP     = True
  Cron     == Cron     = True
  AuthPriv == AuthPriv = True
  FTP      == FTP      = True
  NTPFac   == NTPFac   = True
  Audit    == Audit    = True
  Alert    == Alert    = True
  Clock    == Clock    = True
  Local0   == Local0   = True
  Local1   == Local1   = True
  Local2   == Local2   = True
  Local3   == Local3   = True
  Local4   == Local4   = True
  Local5   == Local5   = True
  Local6   == Local6   = True
  Local7   == Local7   = True
  _        == _        = False

public export
Show Facility where
  show Kern     = "kern"
  show User     = "user"
  show Mail     = "mail"
  show Daemon   = "daemon"
  show Auth     = "auth"
  show SyslogF  = "syslog"
  show LPR      = "lpr"
  show News     = "news"
  show UUCP     = "uucp"
  show Cron     = "cron"
  show AuthPriv = "authpriv"
  show FTP      = "ftp"
  show NTPFac   = "ntp"
  show Audit    = "audit"
  show Alert    = "alert"
  show Clock    = "clock"
  show Local0   = "local0"
  show Local1   = "local1"
  show Local2   = "local2"
  show Local3   = "local3"
  show Local4   = "local4"
  show Local5   = "local5"
  show Local6   = "local6"
  show Local7   = "local7"

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a facility to its numeric code (0-23).
public export
facilityCode : Facility -> Nat
facilityCode Kern     = 0
facilityCode User     = 1
facilityCode Mail     = 2
facilityCode Daemon   = 3
facilityCode Auth     = 4
facilityCode SyslogF  = 5
facilityCode LPR      = 6
facilityCode News     = 7
facilityCode UUCP     = 8
facilityCode Cron     = 9
facilityCode AuthPriv = 10
facilityCode FTP      = 11
facilityCode NTPFac   = 12
facilityCode Audit    = 13
facilityCode Alert    = 14
facilityCode Clock    = 15
facilityCode Local0   = 16
facilityCode Local1   = 17
facilityCode Local2   = 18
facilityCode Local3   = 19
facilityCode Local4   = 20
facilityCode Local5   = 21
facilityCode Local6   = 22
facilityCode Local7   = 23

||| Decode a numeric code to a facility.
||| Returns Nothing for codes outside the valid range (0-23).
public export
facilityFromCode : Nat -> Maybe Facility
facilityFromCode 0  = Just Kern
facilityFromCode 1  = Just User
facilityFromCode 2  = Just Mail
facilityFromCode 3  = Just Daemon
facilityFromCode 4  = Just Auth
facilityFromCode 5  = Just SyslogF
facilityFromCode 6  = Just LPR
facilityFromCode 7  = Just News
facilityFromCode 8  = Just UUCP
facilityFromCode 9  = Just Cron
facilityFromCode 10 = Just AuthPriv
facilityFromCode 11 = Just FTP
facilityFromCode 12 = Just NTPFac
facilityFromCode 13 = Just Audit
facilityFromCode 14 = Just Alert
facilityFromCode 15 = Just Clock
facilityFromCode 16 = Just Local0
facilityFromCode 17 = Just Local1
facilityFromCode 18 = Just Local2
facilityFromCode 19 = Just Local3
facilityFromCode 20 = Just Local4
facilityFromCode 21 = Just Local5
facilityFromCode 22 = Just Local6
facilityFromCode 23 = Just Local7
facilityFromCode _  = Nothing

-- ============================================================================
-- Facility classification
-- ============================================================================

||| Whether a facility is one of the local-use facilities (Local0-Local7).
||| These are available for site-specific customisation.
public export
isLocal : Facility -> Bool
isLocal Local0 = True
isLocal Local1 = True
isLocal Local2 = True
isLocal Local3 = True
isLocal Local4 = True
isLocal Local5 = True
isLocal Local6 = True
isLocal Local7 = True
isLocal _      = False

||| Whether a facility is security-related (Auth or AuthPriv).
public export
isSecurityFacility : Facility -> Bool
isSecurityFacility Auth     = True
isSecurityFacility AuthPriv = True
isSecurityFacility _        = False
