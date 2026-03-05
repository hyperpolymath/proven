-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- OCSP : Top-level module for the proven-ocsp RFC 6960 responder.
-- Re-exports OCSP.Types and defines server constants.

module OCSP

import public OCSP.Types

%default total

---------------------------------------------------------------------------
-- Constants
---------------------------------------------------------------------------

||| Default OCSP responder port (HTTP).
public export
ocspPort : Nat
ocspPort = 80

||| Maximum OCSP request size in bytes.
public export
maxRequestSize : Nat
maxRequestSize = 4096

||| Default response time-to-live in seconds.
public export
defaultResponseTTL : Nat
defaultResponseTTL = 3600
