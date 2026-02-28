-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-dns: A DNS resolver implementation that cannot crash.
--
-- Architecture:
--   - Name: Length-validated domain names (max 63 per label, 253 total)
--   - RecordType: 9 record types as a closed sum type (A, AAAA, CNAME, etc.)
--   - Query: Validated query construction with typed errors
--   - Response: Response codes and resource records with full sections
--   - Zone: Zone management with SOA, NS, and structural validation
--
-- This module defines core DNS constants and re-exports all submodules.

module DNS

import public DNS.Name
import public DNS.RecordType
import public DNS.Query
import public DNS.Response
import public DNS.Zone

||| Standard DNS port (RFC 1035).
public export
dnsPort : Bits16
dnsPort = 53

||| Maximum UDP message size without EDNS (RFC 1035 Section 4.2.1).
public export
maxUdpSize : Nat
maxUdpSize = 512

||| Maximum TCP message size (RFC 1035 Section 4.2.2).
public export
maxTcpSize : Nat
maxTcpSize = 65535

||| Maximum label length in bytes (RFC 1035 Section 2.3.4).
public export
maxLabelLength : Nat
maxLabelLength = 63

||| Maximum total domain name length including dots (RFC 1035).
public export
maxNameLength : Nat
maxNameLength = 253

||| EDNS(0) default UDP payload size (RFC 6891).
public export
ednsUdpSize : Nat
ednsUdpSize = 4096
