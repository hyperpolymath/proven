-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-hardened: Core protocol types for hardened application server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Hardened.Types

%default total

-- ============================================================================
-- HardeningLevel
-- ============================================================================

||| Progressive levels of system hardening applied to the server.
public export
data HardeningLevel : Type where
  ||| Basic hardening: disable unused services, apply patches.
  Minimal  : HardeningLevel
  ||| Standard hardening: firewalling, access controls, logging.
  Standard : HardeningLevel
  ||| High hardening: mandatory access control, encrypted storage.
  High     : HardeningLevel
  ||| Maximum hardening: all controls active, minimal attack surface.
  Maximum  : HardeningLevel

export
Show HardeningLevel where
  show Minimal  = "Minimal"
  show Standard = "Standard"
  show High     = "High"
  show Maximum  = "Maximum"

-- ============================================================================
-- SecurityControl
-- ============================================================================

||| Individual security controls that can be enabled on the server.
public export
data SecurityControl : Type where
  ||| Address Space Layout Randomisation.
  ASLR        : SecurityControl
  ||| Data Execution Prevention (W^X).
  DEP         : SecurityControl
  ||| Stack canary / stack smashing protection.
  StackCanary : SecurityControl
  ||| Control Flow Integrity enforcement.
  CFI         : SecurityControl
  ||| Application-level sandboxing (seccomp, pledge, etc.).
  Sandboxing  : SecurityControl
  ||| Secure Boot chain verification.
  SecureBoot  : SecurityControl
  ||| Comprehensive audit logging.
  AuditLog    : SecurityControl

export
Show SecurityControl where
  show ASLR        = "ASLR"
  show DEP         = "DEP"
  show StackCanary = "StackCanary"
  show CFI         = "CFI"
  show Sandboxing  = "Sandboxing"
  show SecureBoot  = "SecureBoot"
  show AuditLog    = "AuditLog"

-- ============================================================================
-- ComplianceStandard
-- ============================================================================

||| Industry compliance standards the server can be validated against.
public export
data ComplianceStandard : Type where
  ||| Center for Internet Security benchmarks.
  CIS       : ComplianceStandard
  ||| Security Technical Implementation Guides (DoD).
  STIG      : ComplianceStandard
  ||| NIST Special Publication 800-53 controls.
  NIST80053 : ComplianceStandard
  ||| Payment Card Industry Data Security Standard.
  PCI_DSS   : ComplianceStandard
  ||| Federal Information Processing Standard 140.
  FIPS140   : ComplianceStandard

export
Show ComplianceStandard where
  show CIS       = "CIS"
  show STIG      = "STIG"
  show NIST80053 = "NIST80053"
  show PCI_DSS   = "PCI_DSS"
  show FIPS140   = "FIPS140"

-- ============================================================================
-- AuditEvent
-- ============================================================================

||| Categories of security-relevant events captured by the audit system.
public export
data AuditEvent : Type where
  ||| A new process was started.
  ProcessStart        : AuditEvent
  ||| A file was accessed (read, write, or execute).
  FileAccess          : AuditEvent
  ||| A network connection was established or attempted.
  NetworkConn         : AuditEvent
  ||| A privilege escalation occurred (su, sudo, setuid).
  PrivilegeEscalation : AuditEvent
  ||| A configuration file or system setting was changed.
  ConfigChange        : AuditEvent
  ||| An authentication attempt (success or failure).
  AuthAttempt         : AuditEvent

export
Show AuditEvent where
  show ProcessStart        = "ProcessStart"
  show FileAccess          = "FileAccess"
  show NetworkConn         = "NetworkConn"
  show PrivilegeEscalation = "PrivilegeEscalation"
  show ConfigChange        = "ConfigChange"
  show AuthAttempt         = "AuthAttempt"

-- ============================================================================
-- HealthStatus
-- ============================================================================

||| Overall health state of the hardened server.
public export
data HealthStatus : Type where
  ||| All controls active, no anomalies detected.
  Healthy      : HealthStatus
  ||| Some controls degraded but server is operational.
  Degraded     : HealthStatus
  ||| Security posture compromised -- immediate remediation needed.
  Compromised  : HealthStatus
  ||| Server is not responding to health checks.
  Unresponsive : HealthStatus

export
Show HealthStatus where
  show Healthy      = "Healthy"
  show Degraded     = "Degraded"
  show Compromised  = "Compromised"
  show Unresponsive = "Unresponsive"
