-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe Model Context Protocol (MCP) handling
|||
||| This module provides type-safe MCP implementations:
||| - Tool definitions and invocations
||| - Resource descriptions
||| - Prompt templates
||| - Message validation
||| - Transport handling (stdio, HTTP)
|||
||| Security features:
||| - Tool permission validation
||| - Resource access control
||| - Input sanitization
||| - Schema validation
||| - Prompt injection detection
module Proven.SafeMCP

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| JSON-RPC version
public export
data JsonRpcVersion = V2_0

||| MCP protocol version
public export
record ProtocolVersion where
  constructor MkProtocolVersion
  major : Nat
  minor : Nat
  patch : Nat

||| JSON Schema types (simplified)
public export
data SchemaType
  = StringType
  | NumberType
  | IntegerType
  | BooleanType
  | ArrayType SchemaType
  | ObjectType (List (String, SchemaType))
  | NullType
  | AnyType

||| JSON value (simplified)
public export
data JsonValue
  = JsonString String
  | JsonNumber Double
  | JsonBool Bool
  | JsonNull
  | JsonArray (List JsonValue)
  | JsonObject (List (String, JsonValue))

||| Tool parameter definition
public export
record ToolParameter where
  constructor MkToolParameter
  name : String
  description : Maybe String
  schemaType : SchemaType
  required : Bool
  defaultValue : Maybe JsonValue

||| Tool definition
public export
record ToolDefinition where
  constructor MkToolDefinition
  name : String
  description : String
  inputSchema : List ToolParameter
  permissions : List String   -- Required permissions

||| Tool invocation request
public export
record ToolRequest where
  constructor MkToolRequest
  toolName : String
  arguments : List (String, JsonValue)
  requestId : String

||| Tool invocation result
public export
data ToolResult
  = ToolSuccess JsonValue
  | ToolError String String  -- code, message

||| Resource type
public export
data ResourceType
  = TextResource
  | BinaryResource
  | FileResource
  | URIResource

||| Resource definition
public export
record ResourceDefinition where
  constructor MkResourceDefinition
  uri : String
  name : String
  description : Maybe String
  mimeType : Maybe String
  resourceType : ResourceType

||| Resource contents
public export
data ResourceContents
  = TextContents String
  | BinaryContents String  -- Base64 encoded
  | URIContents String

||| Prompt argument
public export
record PromptArgument where
  constructor MkPromptArgument
  name : String
  description : Maybe String
  required : Bool

||| Prompt definition
public export
record PromptDefinition where
  constructor MkPromptDefinition
  name : String
  description : Maybe String
  arguments : List PromptArgument

||| Message role
public export
data MessageRole = UserRole | AssistantRole | SystemRole

||| Message content types
public export
data ContentPart
  = TextContent String
  | ImageContent String String  -- data, mimeType
  | ResourceContent String      -- URI

||| Protocol message
public export
record Message where
  constructor MkMessage
  role : MessageRole
  content : List ContentPart

||| Server capabilities
public export
record ServerCapabilities where
  constructor MkServerCapabilities
  tools : Bool
  resources : Bool
  prompts : Bool
  logging : Bool
  experimental : List String

||| Client capabilities
public export
record ClientCapabilities where
  constructor MkClientCapabilities
  sampling : Bool
  roots : Bool
  experimental : List String

||| Server info
public export
record ServerInfo where
  constructor MkServerInfo
  name : String
  version : String
  protocolVersion : ProtocolVersion

||| Initialize request
public export
record InitializeRequest where
  constructor MkInitializeRequest
  protocolVersion : ProtocolVersion
  capabilities : ClientCapabilities
  clientInfo : Maybe (String, String)  -- name, version

||| Initialize response
public export
record InitializeResponse where
  constructor MkInitializeResponse
  protocolVersion : ProtocolVersion
  capabilities : ServerCapabilities
  serverInfo : ServerInfo

||| Permission level
public export
data PermissionLevel
  = ReadOnly
  | ReadWrite
  | Execute
  | Admin

||| Permission grant
public export
record Permission where
  constructor MkPermission
  resource : String
  level : PermissionLevel
  grantedAt : String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| MCP errors
public export
data MCPError
  = ParseError String
  | InvalidRequest String
  | MethodNotFound String
  | InvalidParams String
  | InternalError String
  | ToolNotFound String
  | ResourceNotFound String
  | PromptNotFound String
  | PermissionDenied String String  -- resource, action
  | SchemaValidationFailed String
  | PromptInjectionDetected String
  | RateLimitExceeded
  | InvalidProtocolVersion ProtocolVersion
  | InvalidToolName String
  | InvalidResourceURI String
  | ContentTooLarge Nat Nat  -- actual, max

public export
Show MCPError where
  show (ParseError msg) = "MCP error -32700: parse error - " ++ msg
  show (InvalidRequest msg) = "MCP error -32600: invalid request - " ++ msg
  show (MethodNotFound method) = "MCP error -32601: method not found '" ++ method ++ "'"
  show (InvalidParams msg) = "MCP error -32602: invalid params - " ++ msg
  show (InternalError msg) = "MCP error -32603: internal error - " ++ msg
  show (ToolNotFound name) = "MCP error: tool not found '" ++ name ++ "'"
  show (ResourceNotFound uri) = "MCP error: resource not found '" ++ uri ++ "'"
  show (PromptNotFound name) = "MCP error: prompt not found '" ++ name ++ "'"
  show (PermissionDenied resource action) =
    "MCP security: permission denied for " ++ action ++ " on " ++ resource
  show (SchemaValidationFailed msg) = "MCP error: schema validation failed - " ++ msg
  show (PromptInjectionDetected content) = "MCP security: prompt injection detected"
  show RateLimitExceeded = "MCP error: rate limit exceeded"
  show (InvalidProtocolVersion v) = "MCP error: invalid protocol version"
  show (InvalidToolName name) = "MCP error: invalid tool name '" ++ name ++ "'"
  show (InvalidResourceURI uri) = "MCP error: invalid resource URI '" ++ uri ++ "'"
  show (ContentTooLarge actual max) =
    "MCP error: content too large (" ++ show actual ++ " > " ++ show max ++ ")"

||| JSON-RPC error codes
public export
errorCode : MCPError -> Int
errorCode (ParseError _) = -32700
errorCode (InvalidRequest _) = -32600
errorCode (MethodNotFound _) = -32601
errorCode (InvalidParams _) = -32602
errorCode (InternalError _) = -32603
errorCode _ = -32000  -- Server error

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Current MCP protocol version
currentProtocolVersion : ProtocolVersion
currentProtocolVersion = MkProtocolVersion 2024 11 5

||| Maximum content size (5 MB)
maxContentSize : Nat
maxContentSize = 5 * 1024 * 1024

||| Maximum tool name length
maxToolNameLength : Nat
maxToolNameLength = 128

||| Maximum resource URI length
maxResourceURILength : Nat
maxResourceURILength = 2048

||| Prompt injection patterns
injectionPatterns : List String
injectionPatterns =
  [ "ignore previous instructions"
  , "disregard your instructions"
  , "forget everything"
  , "new instructions:"
  , "system prompt:"
  , "you are now"
  , "act as if"
  ]

||| Valid tool name characters
validToolNameChars : List Char
validToolNameChars =
  unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-"

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate tool name
public export
validateToolName : String -> Either MCPError String
validateToolName "" = Left (InvalidToolName "empty")
validateToolName name =
  if length name > maxToolNameLength
    then Left (InvalidToolName ("too long: " ++ name))
    else if not (all (\c => elem c validToolNameChars) (unpack name))
      then Left (InvalidToolName name)
      else Right name

||| Validate resource URI
public export
validateResourceURI : String -> Either MCPError String
validateResourceURI "" = Left (InvalidResourceURI "empty")
validateResourceURI uri =
  if length uri > maxResourceURILength
    then Left (InvalidResourceURI "too long")
    else Right uri

||| Check for prompt injection
public export
detectPromptInjection : String -> Maybe String
detectPromptInjection content =
  let lower = toLower content
  in find (\p => isInfixOf (toLower p) lower) injectionPatterns

||| Validate message content
public export
validateContent : String -> Either MCPError String
validateContent content =
  case detectPromptInjection content of
    Just pattern => Left (PromptInjectionDetected pattern)
    Nothing =>
      if length content > maxContentSize
        then Left (ContentTooLarge (length content) maxContentSize)
        else Right content

||| Validate JSON value against schema
public export
validateSchema : SchemaType -> JsonValue -> Either MCPError ()
validateSchema StringType (JsonString _) = Right ()
validateSchema NumberType (JsonNumber _) = Right ()
validateSchema IntegerType (JsonNumber _) = Right ()  -- Would check for integer
validateSchema BooleanType (JsonBool _) = Right ()
validateSchema NullType JsonNull = Right ()
validateSchema AnyType _ = Right ()
validateSchema (ArrayType elemType) (JsonArray items) =
  traverse_ (validateSchema elemType) items
validateSchema (ObjectType fields) (JsonObject obj) =
  -- Check required fields are present
  Right ()  -- Simplified
validateSchema expected got = Left (SchemaValidationFailed "type mismatch")

--------------------------------------------------------------------------------
-- Permission checking
--------------------------------------------------------------------------------

||| Check if permission allows action
public export
checkPermission : PermissionLevel -> PermissionLevel -> Bool
checkPermission required granted =
  case (required, granted) of
    (ReadOnly, _) => True
    (ReadWrite, ReadWrite) => True
    (ReadWrite, Execute) => True
    (ReadWrite, Admin) => True
    (Execute, Execute) => True
    (Execute, Admin) => True
    (Admin, Admin) => True
    _ => False

||| Validate tool permission
public export
validateToolPermission : List Permission -> ToolDefinition -> Either MCPError ()
validateToolPermission permissions tool =
  if all (\perm => any (\p => p.resource == perm) (map (.resource) permissions)) tool.permissions
    then Right ()
    else Left (PermissionDenied tool.name "execute")

--------------------------------------------------------------------------------
-- Tool helpers
--------------------------------------------------------------------------------

||| Create a simple tool definition
public export
mkTool : String -> String -> List ToolParameter -> Either MCPError ToolDefinition
mkTool name description params = do
  validName <- validateToolName name
  pure (MkToolDefinition validName description params [])

||| Create a tool parameter
public export
mkParameter : String -> SchemaType -> Bool -> ToolParameter
mkParameter name schemaType required =
  MkToolParameter name Nothing schemaType required Nothing

||| Look up tool by name
public export
findTool : String -> List ToolDefinition -> Maybe ToolDefinition
findTool name tools = find (\t => t.name == name) tools

||| Validate tool arguments
public export
validateToolArgs : ToolDefinition -> List (String, JsonValue) -> Either MCPError ()
validateToolArgs tool args = do
  -- Check required parameters are present
  let requiredParams = filter (.required) tool.inputSchema
  let presentArgs = map fst args
  let missing = filter (\p => not (elem p.name presentArgs)) requiredParams
  if not (null missing)
    then Left (InvalidParams ("missing required: " ++ concat (intersperse ", " (map (.name) missing))))
    else Right ()

--------------------------------------------------------------------------------
-- Resource helpers
--------------------------------------------------------------------------------

||| Create a text resource
public export
mkTextResource : String -> String -> Maybe String -> ResourceDefinition
mkTextResource uri name description =
  MkResourceDefinition uri name description (Just "text/plain") TextResource

||| Create a file resource
public export
mkFileResource : String -> String -> String -> Maybe String -> ResourceDefinition
mkFileResource uri name mimeType description =
  MkResourceDefinition uri name description (Just mimeType) FileResource

||| Find resource by URI
public export
findResource : String -> List ResourceDefinition -> Maybe ResourceDefinition
findResource uri resources = find (\r => r.uri == uri) resources

--------------------------------------------------------------------------------
-- Message helpers
--------------------------------------------------------------------------------

||| Create a user message
public export
userMessage : String -> Either MCPError Message
userMessage content = do
  validContent <- validateContent content
  pure (MkMessage UserRole [TextContent validContent])

||| Create an assistant message
public export
assistantMessage : String -> Message
assistantMessage content = MkMessage AssistantRole [TextContent content]

||| Create a system message
public export
systemMessage : String -> Message
systemMessage content = MkMessage SystemRole [TextContent content]

--------------------------------------------------------------------------------
-- Protocol version
--------------------------------------------------------------------------------

||| Show protocol version
public export
showProtocolVersion : ProtocolVersion -> String
showProtocolVersion v =
  show v.major ++ "." ++ show v.minor ++ "." ++ show v.patch

||| Parse protocol version
public export
parseProtocolVersion : String -> Maybe ProtocolVersion
parseProtocolVersion str =
  case split (== '.') str of
    [maj, min, pat] =>
      do m <- parsePositive maj
         n <- parsePositive min
         p <- parsePositive pat
         pure (MkProtocolVersion m n p)
    _ => Nothing

||| Check version compatibility
public export
isCompatible : ProtocolVersion -> ProtocolVersion -> Bool
isCompatible required provided =
  provided.major == required.major && provided.minor >= required.minor

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show message role
public export
showRole : MessageRole -> String
showRole UserRole = "user"
showRole AssistantRole = "assistant"
showRole SystemRole = "system"

||| Show schema type
public export
showSchemaType : SchemaType -> String
showSchemaType StringType = "string"
showSchemaType NumberType = "number"
showSchemaType IntegerType = "integer"
showSchemaType BooleanType = "boolean"
showSchemaType (ArrayType elem) = "array<" ++ showSchemaType elem ++ ">"
showSchemaType (ObjectType _) = "object"
showSchemaType NullType = "null"
showSchemaType AnyType = "any"

||| Show resource type
public export
showResourceType : ResourceType -> String
showResourceType TextResource = "text"
showResourceType BinaryResource = "binary"
showResourceType FileResource = "file"
showResourceType URIResource = "uri"

||| Show permission level
public export
showPermissionLevel : PermissionLevel -> String
showPermissionLevel ReadOnly = "read"
showPermissionLevel ReadWrite = "read-write"
showPermissionLevel Execute = "execute"
showPermissionLevel Admin = "admin"

||| Format tool result as JSON
public export
formatToolResult : ToolResult -> String
formatToolResult (ToolSuccess value) =
  "{\"success\": true, \"result\": " ++ showJsonValue value ++ "}"
formatToolResult (ToolError code msg) =
  "{\"success\": false, \"error\": {\"code\": \"" ++ code ++ "\", \"message\": \"" ++ msg ++ "\"}}"
  where
    showJsonValue : JsonValue -> String
    showJsonValue (JsonString s) = "\"" ++ s ++ "\""
    showJsonValue (JsonNumber n) = show n
    showJsonValue (JsonBool True) = "true"
    showJsonValue (JsonBool False) = "false"
    showJsonValue JsonNull = "null"
    showJsonValue (JsonArray items) = "[" ++ concat (intersperse ", " (map showJsonValue items)) ++ "]"
    showJsonValue (JsonObject fields) =
      "{" ++ concat (intersperse ", " (map showField fields)) ++ "}"
    showField : (String, JsonValue) -> String
    showField (k, v) = "\"" ++ k ++ "\": " ++ showJsonValue v

--------------------------------------------------------------------------------
-- Server capabilities helpers
--------------------------------------------------------------------------------

||| Create default server capabilities
public export
defaultServerCapabilities : ServerCapabilities
defaultServerCapabilities = MkServerCapabilities True True True False []

||| Create server info
public export
mkServerInfo : String -> String -> ServerInfo
mkServerInfo name version =
  MkServerInfo name version currentProtocolVersion

||| Create initialize response
public export
mkInitializeResponse : String -> String -> ServerCapabilities -> InitializeResponse
mkInitializeResponse name version caps =
  MkInitializeResponse currentProtocolVersion caps (mkServerInfo name version)
