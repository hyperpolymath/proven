-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-httpd: An HTTP/1.1 implementation that cannot crash.
--
-- Architecture:
--   - Method: 9 HTTP methods as a closed sum type (no unknown method crashes)
--   - Status: 29 status codes with category classification
--   - Request: Validated request parsing with typed errors
--   - Response: Safe response construction and serialisation
--   - Router: Pattern-based route matching with captured parameters
--
-- This module defines core HTTP constants and re-exports all submodules.

module HTTP

import public HTTP.Method
import public HTTP.Status
import public HTTP.Request
import public HTTP.Response
import public HTTP.Router

||| HTTP version string (RFC 7230).
public export
httpVersion : String
httpVersion = "HTTP/1.1"

||| Default HTTP port (RFC 7230 Section 2.7.1).
public export
defaultPort : Bits16
defaultPort = 80

||| Maximum header section size in bytes (common server limit).
||| Requests with headers exceeding this are rejected with 431.
public export
maxHeaderSize : Nat
maxHeaderSize = 8192

||| Maximum request body size in bytes (10 MiB default).
||| Requests exceeding this are rejected with 413 Payload Too Large.
public export
maxBodySize : Nat
maxBodySize = 10485760

||| Default HTTPS port (RFC 7230 Section 2.7.2).
public export
httpsPort : Bits16
httpsPort = 443

||| Server identification string for proven-httpd.
public export
serverIdent : String
serverIdent = "proven-httpd/0.1.0"
