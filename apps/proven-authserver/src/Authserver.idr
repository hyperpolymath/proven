-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-authserver: Authentication server.
--
-- Architecture:
--   - Types: AuthMethod, TokenType, AuthResult, MFAMethod, SessionState
--
-- This module defines core auth constants and re-exports Authserver.Types.

module Authserver

import public Authserver.Types

%default total

||| HTTPS port for the authentication service.
public export
authPort : Nat
authPort = 8443

||| Default access token time-to-live in seconds (1 hour).
public export
tokenTTL : Nat
tokenTTL = 3600

||| Default refresh token time-to-live in seconds (24 hours).
public export
refreshTTL : Nat
refreshTTL = 86400

||| Maximum consecutive failed login attempts before account lockout.
public export
maxLoginAttempts : Nat
maxLoginAttempts = 5
