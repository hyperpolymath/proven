-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-rtsp.
||| Re-exports RTSP.Types and provides protocol constants.
module RTSP

import public RTSP.Types

%default total

---------------------------------------------------------------------------
-- Protocol Constants (RFC 7826)
---------------------------------------------------------------------------

||| Default RTSP port (plaintext).
public export
rtspPort : Nat
rtspPort = 554

||| Default RTSPS port (TLS-secured).
public export
rtspsPort : Nat
rtspsPort = 322

||| Default session timeout in seconds.
public export
defaultSessionTimeout : Nat
defaultSessionTimeout = 60
