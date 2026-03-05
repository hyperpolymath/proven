-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-hardened: Hardened application server.
--
-- Architecture:
--   - Types: HardeningLevel, SecurityControl, ComplianceStandard,
--            AuditEvent, HealthStatus
--
-- This module defines core hardening constants and re-exports Hardened.Types.

module Hardened

import public Hardened.Types

%default total

||| Size of the in-memory audit event ring buffer (in events).
public export
auditBufferSize : Nat
auditBufferSize = 65536

||| Maximum number of concurrent requests the hardened server will accept.
public export
maxConcurrentRequests : Nat
maxConcurrentRequests = 1000
