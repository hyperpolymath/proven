-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-media streaming server.
||| Re-exports core types and provides server constants.
module Media

import public Media.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default RTMP ingest port.
public export
rtmpPort : Nat
rtmpPort = 1935

||| Default HLS segment duration in seconds.
public export
hlsSegmentDuration : Nat
hlsSegmentDuration = 6

||| Default DASH segment duration in seconds.
public export
dashSegmentDuration : Nat
dashSegmentDuration = 4

||| Maximum bitrate in bits per second (50 Mbps).
public export
maxBitrate : Nat
maxBitrate = 50000000

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-media"
