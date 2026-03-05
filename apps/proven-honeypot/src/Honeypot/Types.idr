-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Honeypot.Types : Core types for the network honeypot server.
-- Defines service emulations, interaction fidelity levels,
-- alert severities, and attacker action classifications.

module Honeypot.Types

%default total

---------------------------------------------------------------------------
-- ServiceEmulation : Network services the honeypot can impersonate.
---------------------------------------------------------------------------

||| Network services that the honeypot can emulate to attract attackers.
public export
data ServiceEmulation : Type where
  SSH    : ServiceEmulation
  HTTP   : ServiceEmulation
  FTP    : ServiceEmulation
  SMTP   : ServiceEmulation
  Telnet : ServiceEmulation
  MySQL  : ServiceEmulation
  RDP    : ServiceEmulation

export
Show ServiceEmulation where
  show SSH    = "SSH"
  show HTTP   = "HTTP"
  show FTP    = "FTP"
  show SMTP   = "SMTP"
  show Telnet = "Telnet"
  show MySQL  = "MySQL"
  show RDP    = "RDP"

---------------------------------------------------------------------------
-- InteractionLevel : Fidelity of the honeypot emulation.
---------------------------------------------------------------------------

||| How deeply the honeypot simulates a real service.
public export
data InteractionLevel : Type where
  Low    : InteractionLevel
  Medium : InteractionLevel
  High   : InteractionLevel

export
Show InteractionLevel where
  show Low    = "Low"
  show Medium = "Medium"
  show High   = "High"

---------------------------------------------------------------------------
-- AlertSeverity : Severity classification for honeypot alerts.
---------------------------------------------------------------------------

||| Severity level of a generated alert.
public export
data AlertSeverity : Type where
  Info     : AlertSeverity
  ASLow    : AlertSeverity
  ASMedium : AlertSeverity
  ASHigh   : AlertSeverity
  Critical : AlertSeverity

export
Show AlertSeverity where
  show Info     = "Info"
  show ASLow    = "Low"
  show ASMedium = "Medium"
  show ASHigh   = "High"
  show Critical = "Critical"

---------------------------------------------------------------------------
-- AttackerAction : Classifications of observed attacker behaviour.
---------------------------------------------------------------------------

||| Types of attacker actions observed by the honeypot.
public export
data AttackerAction : Type where
  Scan          : AttackerAction
  BruteForce    : AttackerAction
  Exploit       : AttackerAction
  Payload       : AttackerAction
  Lateral       : AttackerAction
  Exfiltration  : AttackerAction

export
Show AttackerAction where
  show Scan         = "Scan"
  show BruteForce   = "BruteForce"
  show Exploit      = "Exploit"
  show Payload      = "Payload"
  show Lateral      = "Lateral"
  show Exfiltration = "Exfiltration"
