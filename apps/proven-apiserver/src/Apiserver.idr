-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-apiserver: API gateway server.
--
-- Architecture:
--   - Types: AuthScheme, RateLimitStrategy, APIVersion, ResponseFormat, GatewayError
--
-- This module defines core API gateway constants and re-exports Apiserver.Types.

module Apiserver

import public Apiserver.Types

%default total

||| Default HTTPS port for the API gateway.
public export
apiPort : Nat
apiPort = 443

||| Default rate limit in requests per minute per client.
public export
defaultRateLimit : Nat
defaultRateLimit = 1000

||| Maximum request payload size in bytes (1 MB).
public export
maxPayloadSize : Nat
maxPayloadSize = 1048576
