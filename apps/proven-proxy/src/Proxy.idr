-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-proxy: An HTTP forward/reverse proxy that cannot crash.
--
-- Architecture:
--   - Proxy.Types: ProxyMode, HopByHopHeader, CacheDirective, ProxyError
--     as closed sum types with Show/Eq instances.
--
-- This module defines core proxy constants and re-exports Proxy.Types.

module Proxy

import public Proxy.Types

%default total

-- ============================================================================
-- Proxy constants
-- ============================================================================

||| Default listening port for the proxy server.
public export
defaultPort : Bits16
defaultPort = 8080

||| Maximum allowed HTTP header size in bytes.
||| Headers exceeding this size are rejected to prevent resource exhaustion.
public export
maxHeaderSize : Nat
maxHeaderSize = 8192
