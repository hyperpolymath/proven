-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ntp: An NTP (RFC 5905) implementation that cannot crash.
--
-- Architecture:
--   - Timestamp: 64-bit NTP timestamps with safe arithmetic and epoch conversion
--   - Packet: Full NTPv4 packet structure with validated field ranges
--   - Mode: 8 association modes with direction classification
--   - Stratum: Stratum levels with reference identifier descriptions
--   - Filter: Clock filter algorithm selecting best of 8 samples by minimum delay
--
-- This module defines the core NTP types and re-exports submodules.

module NTP

import public NTP.Timestamp
import public NTP.Packet
import public NTP.Mode
import public NTP.Stratum
import public NTP.Filter

||| NTP default port (RFC 5905 Section 7.2).
public export
ntpPort : Bits16
ntpPort = 123

||| NTP version number for NTPv4 (RFC 5905).
public export
ntpVersion : Bits8
ntpVersion = 4

||| Maximum valid stratum for a synchronised server.
public export
maxStratumVal : Nat
maxStratumVal = 15

||| Minimum poll interval exponent (log2 seconds).
||| 2^4 = 16 seconds minimum between polls.
public export
minPollInterval : Nat
minPollInterval = 4

||| Maximum poll interval exponent (log2 seconds).
||| 2^17 = 131072 seconds (~36 hours) maximum between polls.
public export
maxPollInterval : Nat
maxPollInterval = 17

||| Seconds between the NTP epoch (1900-01-01) and Unix epoch (1970-01-01).
||| Provided here for convenience; also available in NTP.Timestamp.
public export
ntpUnixEpochOffset : Nat
ntpUnixEpochOffset = 2208988800
