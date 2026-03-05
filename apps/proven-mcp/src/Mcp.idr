-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Mcp: Top-level module for the Model Context Protocol server.
-- Re-exports all protocol types from Mcp.Types and defines
-- server configuration constants.

module Mcp

import public Mcp.Types

%default total

------------------------------------------------------------------------
-- Server configuration constants
------------------------------------------------------------------------

||| The MCP protocol version this server implements.
public export
mcpVersion : String
mcpVersion = "2025-03-26"

||| Maximum size in bytes for a single content payload.
public export
maxContentSize : Nat
maxContentSize = 10485760

||| Default timeout in seconds for request processing.
public export
defaultTimeout : Nat
defaultTimeout = 30
