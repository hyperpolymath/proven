// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMcp - Model Context Protocol (MCP) message validation that cannot crash.
 *
 * Provides validation for MCP JSON-RPC messages, method names, tool calls,
 * and resource URIs according to the MCP specification. All validation
 * functions return Result types instead of throwing on invalid inputs.
 */
/** MCP validation error types */
type mcpError =
  | InvalidJsonRpcVersion
  | InvalidId
  | InvalidMethod
  | InvalidParams
  | InvalidResult
  | InvalidErrorObject
  | InvalidResourceUri
  | InvalidToolName
  | InvalidPromptName
  | UnsupportedContentType
  | MessageTooLarge
  | MissingRequiredField

/** MCP message types */
type messageType =
  | Request
  | Response
  | Notification
  | ErrorResponse

/** MCP content types */
type contentType =
  | Text
  | Image
  | Resource

/** MCP log levels */
type logLevel =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

/** MCP capability flags */
type capabilities = {
  tools: bool,
  resources: bool,
  prompts: bool,
  logging: bool,
  sampling: bool,
  experimental: option<string>,
}

/** Validated MCP request */
type validatedRequest = {
  id: string,
  method: string,
  hasParams: bool,
}

/** Validated MCP notification */
type validatedNotification = {
  method: string,
  hasParams: bool,
}

/** Validated MCP response */
type validatedResponse = {
  id: string,
  hasResult: bool,
}

/** Validated MCP error response */
type validatedErrorResponse = {
  id: option<string>,
  code: int,
  message: string,
  hasData: bool,
}

/** Maximum allowed message size in bytes (10 MB) */
let maxMessageSize =
  10 * 1024 * 1024

/** Maximum method name length */
let maxMethodLength = 256

/** Maximum tool/prompt name length */
let maxNameLength = 128

/** Maximum URI length */
let maxUriLength = 2048

/** MCP protocol version */
let protocolVersion = "2024-11-05"

/** Standard MCP methods */
module StandardMethods = {
  // Lifecycle
  let initialize = "initialize"
  let initialized = "notifications/initialized"
  let shutdown = "shutdown"

  // Tools
  let toolsList = "tools/list"
  let toolsCall = "tools/call"

  // Resources
  let resourcesList = "resources/list"
  let resourcesRead = "resources/read"
  let resourcesSubscribe = "resources/subscribe"
  let resourcesUnsubscribe = "resources/unsubscribe"

  // Prompts
  let promptsList = "prompts/list"
  let promptsGet = "prompts/get"

  // Logging
  let loggingSetLevel = "logging/setLevel"

  // Notifications
  let notificationCancelled = "notifications/cancelled"
  let notificationProgress = "notifications/progress"
  let notificationMessage = "notifications/message"
  let notificationResourcesUpdated = "notifications/resources/updated"
  let notificationResourcesListChanged = "notifications/resources/list_changed"
  let notificationToolsListChanged = "notifications/tools/list_changed"
  let notificationPromptsListChanged = "notifications/prompts/list_changed"

  // Sampling
  let samplingCreateMessage = "sampling/createMessage"

  // Completion
  let completionComplete = "completion/complete"
}

/** JSON-RPC error codes */
module ErrorCodes = {
  let parseError = -32700
  let invalidRequest = -32600
  let methodNotFound = -32601
  let invalidParams = -32602
  let internalError = -32603
}

/** Validate a JSON-RPC version string */
let validateJsonRpcVersion = (version: string): result<unit, mcpError> => {
  if version == "2.0" {
    Ok()
  } else {
    Error(InvalidJsonRpcVersion)
  }
}

/** Check if a character is valid for method names */
let isValidMethodChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  // Alphanumeric, '/', '_', '-', '.'
  (code >= 48.0 && code <= 57.0) ||
  // 0-9
  code >= 65.0 && code <= 90.0 ||
  // A-Z
  code >= 97.0 && code <= 122.0 ||
  // a-z
  code == 47.0 ||
  // /
  code == 95.0 ||
  // _
  code == 45.0 ||
  // -
  code == 46.0 // .
}

/** Validate a method name */
let validateMethod = (method: string): result<unit, mcpError> => {
  let length = Js.String2.length(method)
  if length == 0 || length > maxMethodLength {
    Error(InvalidMethod)
  } else {
    let chars = Js.String2.split(method, "")
    let allValid = Js.Array2.every(chars, isValidMethodChar)
    if allValid {
      Ok()
    } else {
      Error(InvalidMethod)
    }
  }
}

/** Check if a method is a notification (does not expect a response) */
let isNotificationMethod = (method: string): bool => {
  Js.String2.startsWith(method, "notifications/")
}

/** Check if a method is a standard MCP method */
let isStandardMethod = (method: string): bool => {
  let standardMethods = [
    StandardMethods.initialize,
    StandardMethods.initialized,
    StandardMethods.shutdown,
    StandardMethods.toolsList,
    StandardMethods.toolsCall,
    StandardMethods.resourcesList,
    StandardMethods.resourcesRead,
    StandardMethods.resourcesSubscribe,
    StandardMethods.resourcesUnsubscribe,
    StandardMethods.promptsList,
    StandardMethods.promptsGet,
    StandardMethods.loggingSetLevel,
    StandardMethods.notificationCancelled,
    StandardMethods.notificationProgress,
    StandardMethods.notificationMessage,
    StandardMethods.notificationResourcesUpdated,
    StandardMethods.notificationResourcesListChanged,
    StandardMethods.notificationToolsListChanged,
    StandardMethods.notificationPromptsListChanged,
    StandardMethods.samplingCreateMessage,
    StandardMethods.completionComplete,
  ]
  Js.Array2.includes(standardMethods, method)
}

/** Check if first character is alphabetic or underscore */
let isValidNameFirstChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  (code >= 65.0 && code <= 90.0) ||
  // A-Z
  code >= 97.0 && code <= 122.0 ||
  // a-z
  code == 95.0 // _
}

/** Check if character is alphanumeric, underscore, or hyphen */
let isValidNameChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  (code >= 48.0 && code <= 57.0) ||
  // 0-9
  code >= 65.0 && code <= 90.0 ||
  // A-Z
  code >= 97.0 && code <= 122.0 ||
  // a-z
  code == 95.0 ||
  // _
  code == 45.0 // -
}

/** Validate a tool name */
let validateToolName = (name: string): result<unit, mcpError> => {
  let length = Js.String2.length(name)
  if length == 0 || length > maxNameLength {
    Error(InvalidToolName)
  } else {
    let firstChar = Js.String2.charAt(name, 0)
    if !isValidNameFirstChar(firstChar) {
      Error(InvalidToolName)
    } else {
      let rest = Js.String2.sliceToEnd(name, ~from=1)
      let chars = Js.String2.split(rest, "")
      let allValid = Js.Array2.every(chars, isValidNameChar)
      if allValid {
        Ok()
      } else {
        Error(InvalidToolName)
      }
    }
  }
}

/** Validate a prompt name */
let validatePromptName = (name: string): result<unit, mcpError> => {
  let length = Js.String2.length(name)
  if length == 0 || length > maxNameLength {
    Error(InvalidPromptName)
  } else {
    let firstChar = Js.String2.charAt(name, 0)
    if !isValidNameFirstChar(firstChar) {
      Error(InvalidPromptName)
    } else {
      let rest = Js.String2.sliceToEnd(name, ~from=1)
      let chars = Js.String2.split(rest, "")
      let allValid = Js.Array2.every(chars, isValidNameChar)
      if allValid {
        Ok()
      } else {
        Error(InvalidPromptName)
      }
    }
  }
}

/** Validate a resource URI */
let validateResourceUri = (uri: string): result<unit, mcpError> => {
  let length = Js.String2.length(uri)
  if length == 0 || length > maxUriLength {
    Error(InvalidResourceUri)
  } else {
    // Must contain a scheme separator
    switch Js.String2.indexOf(uri, "://") {
    | -1 => Error(InvalidResourceUri)
    | 0 => Error(InvalidResourceUri) // Scheme must be non-empty
    | schemeEnd =>
      // Validate scheme characters (alphanumeric, +, -, .)
      let scheme = Js.String2.slice(uri, ~from=0, ~to_=schemeEnd)
      let schemeChars = Js.String2.split(scheme, "")
      let validScheme = Js.Array2.every(schemeChars, c => {
        let code = Js.String2.charCodeAt(c, 0)
        (code >= 48.0 && code <= 57.0) ||
        // 0-9
        code >= 65.0 && code <= 90.0 ||
        // A-Z
        code >= 97.0 && code <= 122.0 ||
        // a-z
        code == 43.0 ||
        // +
        code == 45.0 ||
        // -
        code == 46.0 // .
      })

      if !validScheme {
        Error(InvalidResourceUri)
      } else if length <= schemeEnd + 3 {
        // Path must be non-empty after scheme
        Error(InvalidResourceUri)
      } else {
        Ok()
      }
    }
  }
}

/** Validate a message ID */
let validateId = (id: string): result<unit, mcpError> => {
  if Js.String2.length(id) == 0 {
    Error(InvalidId)
  } else {
    Ok()
  }
}

/** Validate an error code */
let validateErrorCode = (code: int): bool => {
  switch code {
  | -32700 | -32600 | -32601 | -32602 | -32603 => true
  | c if c >= -32099 && c <= -32000 => true // Server error range
  | c => c < -32099 || c > -32000 // Application-defined
  }
}

/** Get the error message for a standard error code */
let errorCodeMessage = (code: int): option<string> => {
  switch code {
  | -32700 => Some("Parse error")
  | -32600 => Some("Invalid Request")
  | -32601 => Some("Method not found")
  | -32602 => Some("Invalid params")
  | -32603 => Some("Internal error")
  | _ => None
  }
}

/** Validate message size */
let validateMessageSize = (size: int): result<unit, mcpError> => {
  if size > maxMessageSize {
    Error(MessageTooLarge)
  } else {
    Ok()
  }
}

/** Detect message type from JSON object fields */
let detectMessageType = (hasId: bool, hasMethod: bool, hasResult: bool, hasError: bool): option<
  messageType,
> => {
  if hasMethod && hasId && !hasResult && !hasError {
    Some(Request)
  } else if hasMethod && !hasId && !hasResult && !hasError {
    Some(Notification)
  } else if hasId && hasResult && !hasMethod && !hasError {
    Some(Response)
  } else if hasId && hasError && !hasMethod && !hasResult {
    Some(ErrorResponse)
  } else if hasError && !hasMethod && !hasResult {
    // Allow error response without id (for parse errors)
    Some(ErrorResponse)
  } else {
    None
  }
}

/** Parse content type from string */
let parseContentType = (s: string): option<contentType> => {
  switch s {
  | "text" => Some(Text)
  | "image" => Some(Image)
  | "resource" => Some(Resource)
  | _ => None
  }
}

/** Convert content type to string */
let contentTypeToString = (ct: contentType): string => {
  switch ct {
  | Text => "text"
  | Image => "image"
  | Resource => "resource"
  }
}

/** Parse log level from string */
let parseLogLevel = (s: string): option<logLevel> => {
  switch s {
  | "debug" => Some(Debug)
  | "info" => Some(Info)
  | "notice" => Some(Notice)
  | "warning" => Some(Warning)
  | "error" => Some(Error)
  | "critical" => Some(Critical)
  | "alert" => Some(Alert)
  | "emergency" => Some(Emergency)
  | _ => None
  }
}

/** Get severity level (0-7) for a log level */
let logLevelSeverity = (level: logLevel): int => {
  switch level {
  | Debug => 0
  | Info => 1
  | Notice => 2
  | Warning => 3
  | Error => 4
  | Critical => 5
  | Alert => 6
  | Emergency => 7
  }
}

/** Check if a protocol version is supported */
let isSupportedProtocolVersion = (version: string): bool => {
  let supported = ["2024-11-05", "2024-10-07"]
  Js.Array2.includes(supported, version)
}

/** Create default server capabilities */
let defaultServerCapabilities = (): capabilities => {
  {
    tools: true,
    resources: true,
    prompts: true,
    logging: true,
    sampling: false,
    experimental: None,
  }
}

/** Create default client capabilities */
let defaultClientCapabilities = (): capabilities => {
  {
    tools: false,
    resources: false,
    prompts: false,
    logging: false,
    sampling: true,
    experimental: None,
  }
}

/** Validate a complete MCP request structure */
let validateRequest = (jsonrpc: string, id: string, method: string): result<
  validatedRequest,
  mcpError,
> => {
  switch validateJsonRpcVersion(jsonrpc) {
  | Error(e) => Error(e)
  | Ok() =>
    switch validateId(id) {
    | Error(e) => Error(e)
    | Ok() =>
      switch validateMethod(method) {
      | Error(e) => Error(e)
      | Ok() => Ok({id, method, hasParams: false})
      }
    }
  }
}

/** Validate a complete MCP notification structure */
let validateNotification = (jsonrpc: string, method: string): result<
  validatedNotification,
  mcpError,
> => {
  switch validateJsonRpcVersion(jsonrpc) {
  | Error(e) => Error(e)
  | Ok() =>
    switch validateMethod(method) {
    | Error(e) => Error(e)
    | Ok() => Ok({method, hasParams: false})
    }
  }
}

/** Create a JSON-RPC error response object */
let createErrorResponse = (
  id: option<string>,
  code: int,
  message: string,
): validatedErrorResponse => {
  {id, code, message, hasData: false}
}

/** Error response for parse errors (id may be null) */
let parseErrorResponse = (): validatedErrorResponse =>
  createErrorResponse(None, ErrorCodes.parseError, "Parse error")

/** Error response for invalid request */
let invalidRequestResponse = (id: string): validatedErrorResponse =>
  createErrorResponse(Some(id), ErrorCodes.invalidRequest, "Invalid Request")

/** Error response for method not found */
let methodNotFoundResponse = (id: string, method: string): validatedErrorResponse =>
  createErrorResponse(Some(id), ErrorCodes.methodNotFound, `Method not found: ${method}`)

/** Error response for invalid params */
let invalidParamsResponse = (id: string): validatedErrorResponse =>
  createErrorResponse(Some(id), ErrorCodes.invalidParams, "Invalid params")

/** Error response for internal error */
let internalErrorResponse = (id: string): validatedErrorResponse =>
  createErrorResponse(Some(id), ErrorCodes.internalError, "Internal error")
