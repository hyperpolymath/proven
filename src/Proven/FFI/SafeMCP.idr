-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeMCP operations
|||
||| This module exports MCP prompt injection detection to the C ABI
||| via Idris2's RefC backend.
|||
||| Return conventions:
||| - Injection analysis -> Int (confidence level: 0=None, 1=Low, 2=Medium, 3=High, 4=Critical)
||| - Bool checks -> Int (0 = false, 1 = true)
||| - Sanitization -> String (filtered content)
module Proven.FFI.SafeMCP

import Proven.SafeMCP

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode InjectionConfidence as Int
encodeConfidence : InjectionConfidence -> Int
encodeConfidence None = 0
encodeConfidence Low = 1
encodeConfidence Medium = 2
encodeConfidence High = 3
encodeConfidence Critical = 4

--------------------------------------------------------------------------------
-- Injection Detection
--------------------------------------------------------------------------------

export
proven_idris_mcp_has_injection_pattern : String -> Int
proven_idris_mcp_has_injection_pattern = encodeBool . hasInjectionPattern

export
proven_idris_mcp_analyze_injection : String -> Int
proven_idris_mcp_analyze_injection = encodeConfidence . analyzeInjection

export
proven_idris_mcp_is_injection_free : String -> Int
proven_idris_mcp_is_injection_free s =
  encodeBool (analyzeInjection s == None)

--------------------------------------------------------------------------------
-- Content Sanitization
--------------------------------------------------------------------------------

export
proven_idris_mcp_sanitize_tool_input : String -> String
proven_idris_mcp_sanitize_tool_input = sanitizeToolInput

--------------------------------------------------------------------------------
-- Tool Name Validation
--------------------------------------------------------------------------------

export
proven_idris_mcp_is_valid_tool_name : String -> Int
proven_idris_mcp_is_valid_tool_name = encodeBool . isValidToolName

export
proven_idris_mcp_mk_tool_name : String -> (Int, String)
proven_idris_mcp_mk_tool_name s = case mkToolName s of
  Nothing => (1, "Invalid tool name")
  Just (MkToolName n) => (0, n)

--------------------------------------------------------------------------------
-- Result Size Check
--------------------------------------------------------------------------------

export
proven_idris_mcp_max_result_size : Int
proven_idris_mcp_max_result_size = cast maxResultSize

export
proven_idris_mcp_is_result_size_ok : Int -> Int
proven_idris_mcp_is_result_size_ok size = encodeBool (cast size <= maxResultSize)

--------------------------------------------------------------------------------
-- MCP Role Info
--------------------------------------------------------------------------------

export
proven_idris_mcp_role_name : Int -> String
proven_idris_mcp_role_name 0 = show User
proven_idris_mcp_role_name 1 = show Assistant
proven_idris_mcp_role_name 2 = show System
proven_idris_mcp_role_name 3 = show Tool
proven_idris_mcp_role_name _ = "unknown"
