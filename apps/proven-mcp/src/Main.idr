-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main: Entry point for the proven-mcp server skeleton.
-- Prints the server identity, protocol version, configuration
-- constants, and enumerates all protocol type constructors.

module Main

import Mcp

%default total

------------------------------------------------------------------------
-- All constructors for each protocol type, collected as lists for
-- display purposes.
------------------------------------------------------------------------

||| All MCP message types.
allMessageTypes : List MessageType
allMessageTypes = [ Initialize, Initialized, Ping, CallTool, ToolResult
                  , ListTools, ListResources, ReadResource, ListPrompts
                  , GetPrompt, Subscribe, Unsubscribe, Notification, Cancel ]

||| All transport mechanisms.
allTransports : List Transport
allTransports = [Stdio, SSE, WebSocket, StreamableHTTP]

||| All content types.
allContentTypes : List ContentType
allContentTypes = [Text, Image, Resource, Embedding]

||| All error codes.
allErrorCodes : List ErrorCode
allErrorCodes = [ParseError, InvalidRequest, MethodNotFound, InvalidParams, InternalError, Timeout]

||| All capabilities.
allCapabilities : List Capability
allCapabilities = [Tools, Resources, Prompts, Logging, Sampling]

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "proven-mcp: Model Context Protocol Server"
  putStrLn $ "  MCP version:      " ++ mcpVersion
  putStrLn $ "  Max content size: " ++ show maxContentSize ++ " bytes"
  putStrLn $ "  Default timeout:  " ++ show defaultTimeout ++ "s"
  putStrLn ""
  putStrLn $ "MessageType: " ++ show allMessageTypes
  putStrLn $ "Transport:   " ++ show allTransports
  putStrLn $ "ContentType: " ++ show allContentTypes
  putStrLn $ "ErrorCode:   " ++ show allErrorCodes
  putStrLn $ "Capability:  " ++ show allCapabilities
