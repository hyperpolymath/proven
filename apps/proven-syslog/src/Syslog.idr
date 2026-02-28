-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-syslog: A syslog (RFC 5424) implementation that cannot crash.
--
-- Architecture:
--   - Facility: 24 facility codes with compile-time exhaustive matching
--   - Severity: 8 severity levels with Ord for filtering
--   - Priority: Combined facility*8+severity encoding with validated range 0-191
--   - Message: Full RFC 5424 message structure with structured data and validation
--   - Transport: UDP/514, TCP/514, TLS/6514 with property functions
--
-- This module defines the core Syslog types and re-exports submodules.

module Syslog

import public Syslog.Facility
import public Syslog.Severity
import public Syslog.Message
import public Syslog.Priority
import public Syslog.Transport

||| Syslog default port (UDP and TCP, RFC 5426/6587).
public export
syslogPort : Bits16
syslogPort = 514

||| Syslog TLS port (RFC 5425).
public export
syslogTlsPort : Bits16
syslogTlsPort = 6514

||| Maximum syslog message size over UDP (RFC 5426 Section 3.1).
||| Receivers SHOULD be able to accept messages up to 2048 bytes.
public export
maxMessageSizeUDP : Nat
maxMessageSizeUDP = 2048

||| Maximum syslog message size for RFC 5424 (TCP/TLS).
||| Implementations SHOULD support messages up to 480,000 bytes.
public export
maxRfc5424Size : Nat
maxRfc5424Size = 480000
