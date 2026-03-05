-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Mcp.Types: Core protocol types for the Model Context Protocol
-- server. Based on the MCP specification. All types are closed
-- sum types with total Show instances.

module Mcp.Types

%default total

------------------------------------------------------------------------
-- MessageType
-- The set of JSON-RPC message types defined by the MCP spec.
------------------------------------------------------------------------

||| MCP JSON-RPC message types. Covers the full lifecycle from
||| initialisation through tool/resource/prompt operations to
||| subscription management and cancellation.
public export
data MessageType : Type where
  ||| Client sends capabilities and protocol version.
  Initialize    : MessageType
  ||| Server acknowledges initialisation.
  Initialized   : MessageType
  ||| Keepalive ping.
  Ping          : MessageType
  ||| Client requests execution of a named tool.
  CallTool      : MessageType
  ||| Server returns the result of a tool call.
  ToolResult    : MessageType
  ||| Client requests the list of available tools.
  ListTools     : MessageType
  ||| Client requests the list of available resources.
  ListResources : MessageType
  ||| Client requests the contents of a specific resource.
  ReadResource  : MessageType
  ||| Client requests the list of available prompts.
  ListPrompts   : MessageType
  ||| Client requests a specific prompt template.
  GetPrompt     : MessageType
  ||| Client subscribes to resource change notifications.
  Subscribe     : MessageType
  ||| Client unsubscribes from resource change notifications.
  Unsubscribe   : MessageType
  ||| Server sends an asynchronous notification.
  Notification  : MessageType
  ||| Client cancels a pending request.
  Cancel        : MessageType

export
Show MessageType where
  show Initialize    = "Initialize"
  show Initialized   = "Initialized"
  show Ping          = "Ping"
  show CallTool      = "CallTool"
  show ToolResult    = "ToolResult"
  show ListTools     = "ListTools"
  show ListResources = "ListResources"
  show ReadResource  = "ReadResource"
  show ListPrompts   = "ListPrompts"
  show GetPrompt     = "GetPrompt"
  show Subscribe     = "Subscribe"
  show Unsubscribe   = "Unsubscribe"
  show Notification  = "Notification"
  show Cancel        = "Cancel"

------------------------------------------------------------------------
-- Transport
-- The transport layer over which MCP messages are carried.
------------------------------------------------------------------------

||| MCP transport mechanisms. Stdio is the original transport;
||| SSE and WebSocket are for browser/network use; StreamableHTTP
||| is the newest addition to the spec.
public export
data Transport : Type where
  ||| Standard input/output (the original MCP transport).
  Stdio          : Transport
  ||| Server-Sent Events over HTTP.
  SSE            : Transport
  ||| Full-duplex WebSocket connection.
  WebSocket      : Transport
  ||| Streamable HTTP (newest MCP transport).
  StreamableHTTP : Transport

export
Show Transport where
  show Stdio          = "Stdio"
  show SSE            = "SSE"
  show WebSocket      = "WebSocket"
  show StreamableHTTP = "StreamableHTTP"

------------------------------------------------------------------------
-- ContentType
-- The kinds of content that can appear in MCP messages.
------------------------------------------------------------------------

||| The type of content carried in an MCP tool result or resource.
public export
data ContentType : Type where
  ||| Plain or structured text content.
  Text      : ContentType
  ||| Image content (base64-encoded or URI).
  Image     : ContentType
  ||| A reference to an MCP resource.
  Resource  : ContentType
  ||| A dense vector embedding.
  Embedding : ContentType

export
Show ContentType where
  show Text      = "Text"
  show Image     = "Image"
  show Resource  = "Resource"
  show Embedding = "Embedding"

------------------------------------------------------------------------
-- ErrorCode
-- JSON-RPC error codes used in MCP error responses.
------------------------------------------------------------------------

||| Standard JSON-RPC error codes used in MCP error responses.
public export
data ErrorCode : Type where
  ||| The request could not be parsed as valid JSON-RPC.
  ParseError     : ErrorCode
  ||| The request is valid JSON-RPC but semantically invalid.
  InvalidRequest : ErrorCode
  ||| The requested method does not exist.
  MethodNotFound : ErrorCode
  ||| The method parameters are invalid.
  InvalidParams  : ErrorCode
  ||| An internal server error occurred.
  InternalError  : ErrorCode
  ||| The request timed out.
  Timeout        : ErrorCode

export
Show ErrorCode where
  show ParseError     = "ParseError"
  show InvalidRequest = "InvalidRequest"
  show MethodNotFound = "MethodNotFound"
  show InvalidParams  = "InvalidParams"
  show InternalError  = "InternalError"
  show Timeout        = "Timeout"

------------------------------------------------------------------------
-- Capability
-- Server capabilities advertised during initialisation.
------------------------------------------------------------------------

||| Capabilities that an MCP server can advertise during the
||| initialisation handshake.
public export
data Capability : Type where
  ||| Server provides callable tools.
  Tools     : Capability
  ||| Server provides readable resources.
  Resources : Capability
  ||| Server provides prompt templates.
  Prompts   : Capability
  ||| Server supports structured logging.
  Logging   : Capability
  ||| Server supports LLM sampling requests.
  Sampling  : Capability

export
Show Capability where
  show Tools     = "Tools"
  show Resources = "Resources"
  show Prompts   = "Prompts"
  show Logging   = "Logging"
  show Sampling  = "Sampling"
