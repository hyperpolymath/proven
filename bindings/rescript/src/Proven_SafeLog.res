// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeLog - Structured logging with level validation that cannot crash.
 *
 * Provides a structured logging interface with validated log levels,
 * safe string formatting, and support for structured context fields.
 * All operations are safe and will not throw on invalid input.
 */

/** Error types for logging operations */
type logError =
  | InvalidLevel
  | InvalidFieldName
  | InvalidFieldValue
  | MessageTooLong
  | TooManyFields

/** Log severity levels following syslog conventions */
type level =
  | Emergency // System is unusable
  | Alert // Action must be taken immediately
  | Critical // Critical conditions
  | Error // Error conditions
  | Warning // Warning conditions
  | Notice // Normal but significant condition
  | Info // Informational messages
  | Debug // Debug-level messages
  | Trace // Trace-level messages (verbose debug)

/** Maximum message length */
let maxMessageLength = 8192

/** Maximum number of context fields */
let maxFields = 32

/** Maximum field name length */
let maxFieldNameLength = 64

/** Maximum field value length */
let maxFieldValueLength = 1024

/** Parse a level from string (case-insensitive) */
let levelFromString = (str: string): option<level> => {
  let lower = Js.String2.toLowerCase(str)
  switch lower {
  | "emergency" | "emerg" => Some(Emergency)
  | "alert" => Some(Alert)
  | "critical" | "crit" => Some(Critical)
  | "error" | "err" => Some(Error)
  | "warning" | "warn" => Some(Warning)
  | "notice" => Some(Notice)
  | "info" | "information" => Some(Info)
  | "debug" => Some(Debug)
  | "trace" => Some(Trace)
  | _ => None
  }
}

/** Convert level to string representation */
let levelToString = (level: level): string => {
  switch level {
  | Emergency => "EMERGENCY"
  | Alert => "ALERT"
  | Critical => "CRITICAL"
  | Error => "ERROR"
  | Warning => "WARNING"
  | Notice => "NOTICE"
  | Info => "INFO"
  | Debug => "DEBUG"
  | Trace => "TRACE"
  }
}

/** Convert level to short string representation */
let levelToShortString = (level: level): string => {
  switch level {
  | Emergency => "EMR"
  | Alert => "ALR"
  | Critical => "CRT"
  | Error => "ERR"
  | Warning => "WRN"
  | Notice => "NTC"
  | Info => "INF"
  | Debug => "DBG"
  | Trace => "TRC"
  }
}

/** Get numeric severity (0 = most severe, 8 = least severe) */
let levelSeverity = (level: level): int => {
  switch level {
  | Emergency => 0
  | Alert => 1
  | Critical => 2
  | Error => 3
  | Warning => 4
  | Notice => 5
  | Info => 6
  | Debug => 7
  | Trace => 8
  }
}

/** Check if this level is at least as severe as another */
let levelIsAtLeast = (level: level, other: level): bool => {
  levelSeverity(level) <= levelSeverity(other)
}

/** Check if this level should be logged given a minimum level */
let shouldLog = (level: level, minimumLevel: level): bool => {
  levelSeverity(level) <= levelSeverity(minimumLevel)
}

/** Supported field value types */
type fieldValue =
  | String(string)
  | Integer(int)
  | Float(float)
  | Boolean(bool)
  | Null

/** A single context field (key-value pair) */
type field = {
  name: string,
  value: fieldValue,
}

/** Source code location information */
type sourceLocation = {
  file: string,
  line: int,
  column: int,
  functionName: option<string>,
}

/** A structured log entry */
type logEntry = {
  level: level,
  message: string,
  fields: array<field>,
  timestampMs: option<float>,
  loggerName: option<string>,
  sourceLocation: option<sourceLocation>,
}

/** Validate a field name */
let isValidFieldName = (name: string): bool => {
  let len = Js.String2.length(name)
  if len == 0 || len > maxFieldNameLength {
    false
  } else {
    // First character must be letter or underscore
    let first = Js.String2.charAt(name, 0)
    let isValidFirst =
      (first >= "a" && first <= "z") || (first >= "A" && first <= "Z") || first == "_"

    if !isValidFirst {
      false
    } else {
      // Rest must be alphanumeric, underscore, hyphen, or dot
      let valid = ref(true)
      for i in 1 to len - 1 {
        let c = Js.String2.charAt(name, i)
        let isAlpha = (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
        let isDigit = c >= "0" && c <= "9"
        let isSpecial = c == "_" || c == "-" || c == "."
        if !(isAlpha || isDigit || isSpecial) {
          valid := false
        }
      }
      valid.contents
    }
  }
}

/** Validate a field value (string type) */
let isValidFieldValue = (value: string): bool => {
  let len = Js.String2.length(value)
  if len > maxFieldValueLength {
    false
  } else {
    // Check for control characters (except newline, tab, carriage return)
    let valid = ref(true)
    for i in 0 to len - 1 {
      let code = Js.String2.charCodeAt(value, i)->Belt.Float.toInt
      if code < 0x20 && code != 0x0A && code != 0x09 && code != 0x0D {
        valid := false
      }
    }
    valid.contents
  }
}

/** Validate a log message */
let isValidMessage = (message: string): bool => {
  let len = Js.String2.length(message)
  if len > maxMessageLength {
    false
  } else {
    // Check for null bytes
    !Js.String2.includes(message, "\x00")
  }
}

/** Format field value as string */
let fieldValueToString = (value: fieldValue): string => {
  switch value {
  | String(s) => s
  | Integer(i) => Belt.Int.toString(i)
  | Float(f) => Belt.Float.toString(f)
  | Boolean(b) =>
    if b {
      "true"
    } else {
      "false"
    }
  | Null => "null"
  }
}

/** Structured logger with context support */
type logger = {
  mutable name: option<string>,
  mutable minimumLevel: level,
  mutable contextFields: array<field>,
  mutable enabled: bool,
}

/** Create a new logger */
let createLogger = (): logger => {
  {
    name: None,
    minimumLevel: Info,
    contextFields: [],
    enabled: true,
  }
}

/** Create a named logger */
let createNamedLogger = (name: string): logger => {
  {
    name: Some(name),
    minimumLevel: Info,
    contextFields: [],
    enabled: true,
  }
}

/** Set minimum log level */
let setLevel = (logger: logger, level: level): unit => {
  logger.minimumLevel = level
}

/** Enable or disable the logger */
let setEnabled = (logger: logger, enabled: bool): unit => {
  logger.enabled = enabled
}

/** Add a context field that will be included in all log entries */
let addContextField = (logger: logger, name: string, value: fieldValue): result<unit, logError> => {
  if !isValidFieldName(name) {
    Error(InvalidFieldName)
  } else if Belt.Array.length(logger.contextFields) >= maxFields {
    Error(TooManyFields)
  } else {
    let field: field = {name: name, value: value}
    logger.contextFields = Belt.Array.concat(logger.contextFields, [field])
    Ok()
  }
}

/** Clear all context fields */
let clearContext = (logger: logger): unit => {
  logger.contextFields = []
}

/** Create a log entry */
let createEntry = (
  level: level,
  message: string,
  fields: array<field>,
  loggerName: option<string>,
): result<logEntry, logError> => {
  if !isValidMessage(message) {
    Error(MessageTooLong)
  } else {
    // Validate all fields
    let invalidField = fields->Belt.Array.some(f => !isValidFieldName(f.name))
    if invalidField {
      Error(InvalidFieldName)
    } else {
      Ok({
        level: level,
        message: message,
        fields: fields,
        timestampMs: Some(Js.Date.now()),
        loggerName: loggerName,
        sourceLocation: None,
      })
    }
  }
}

/** Log a message at the specified level */
let log = (
  logger: logger,
  level: level,
  message: string,
  fields: array<field>,
): result<logEntry, logError> => {
  if !logger.enabled || !shouldLog(level, logger.minimumLevel) {
    // Return a dummy entry for disabled logs
    Ok({
      level: level,
      message: message,
      fields: fields,
      timestampMs: None,
      loggerName: logger.name,
      sourceLocation: None,
    })
  } else {
    let allFields = Belt.Array.concat(logger.contextFields, fields)
    createEntry(level, message, allFields, logger.name)
  }
}

/** Log at emergency level */
let emergency = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Emergency, message, [])
}

/** Log at alert level */
let alert = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Alert, message, [])
}

/** Log at critical level */
let critical = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Critical, message, [])
}

/** Log at error level */
let error = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Error, message, [])
}

/** Log at warning level */
let warning = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Warning, message, [])
}

/** Log at notice level */
let notice = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Notice, message, [])
}

/** Log at info level */
let info = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Info, message, [])
}

/** Log at debug level */
let debug = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Debug, message, [])
}

/** Log at trace level */
let trace = (logger: logger, message: string): result<logEntry, logError> => {
  log(logger, Trace, message, [])
}

/** Format a log entry as JSON */
let formatJson = (entry: logEntry): string => {
  let parts = []

  // Level
  let _ = Js.Array2.push(parts, "\"level\":\"" ++ levelToString(entry.level) ++ "\"")

  // Message (escape for JSON)
  let escapedMsg =
    entry.message
    ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
    ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
    ->Js.String2.replaceByRe(%re("/\\n/g"), "\\n")
    ->Js.String2.replaceByRe(%re("/\\r/g"), "\\r")
    ->Js.String2.replaceByRe(%re("/\\t/g"), "\\t")
  let _ = Js.Array2.push(parts, "\"message\":\"" ++ escapedMsg ++ "\"")

  // Timestamp
  switch entry.timestampMs {
  | Some(ts) => {
      let _ = Js.Array2.push(parts, "\"timestamp_ms\":" ++ Belt.Float.toString(ts))
    }
  | None => ()
  }

  // Logger name
  switch entry.loggerName {
  | Some(name) => {
      let _ = Js.Array2.push(parts, "\"logger\":\"" ++ name ++ "\"")
    }
  | None => ()
  }

  // Fields
  entry.fields->Belt.Array.forEach(field => {
    let valueStr = switch field.value {
    | String(s) => "\"" ++ s ++ "\""
    | Integer(i) => Belt.Int.toString(i)
    | Float(f) => Belt.Float.toString(f)
    | Boolean(b) =>
      if b {
        "true"
      } else {
        "false"
      }
    | Null => "null"
    }
    let _ = Js.Array2.push(parts, "\"" ++ field.name ++ "\":" ++ valueStr)
  })

  "{" ++ Js.Array2.join(parts, ",") ++ "}"
}

/** Format a log entry as a human-readable line */
let formatLine = (entry: logEntry): string => {
  let parts = []

  // Level
  let _ = Js.Array2.push(parts, "[" ++ levelToShortString(entry.level) ++ "]")

  // Logger name
  switch entry.loggerName {
  | Some(name) => {
      let _ = Js.Array2.push(parts, name ++ ":")
    }
  | None => ()
  }

  // Message
  let _ = Js.Array2.push(parts, entry.message)

  // Fields
  if Belt.Array.length(entry.fields) > 0 {
    let fieldStrs = entry.fields->Belt.Array.map(field => {
      field.name ++ "=" ++ fieldValueToString(field.value)
    })
    let _ = Js.Array2.push(parts, "{" ++ Js.Array2.join(fieldStrs, ", ") ++ "}")
  }

  Js.Array2.join(parts, " ")
}

/** Create a child logger with additional context */
let childLogger = (parent: logger, additionalContext: array<field>): logger => {
  {
    name: parent.name,
    minimumLevel: parent.minimumLevel,
    contextFields: Belt.Array.concat(parent.contextFields, additionalContext),
    enabled: parent.enabled,
  }
}

/** Create a field helper */
let field = (name: string, value: fieldValue): field => {
  {name: name, value: value}
}

/** Create a string field */
let stringField = (name: string, value: string): field => {
  {name: name, value: String(value)}
}

/** Create an integer field */
let intField = (name: string, value: int): field => {
  {name: name, value: Integer(value)}
}

/** Create a float field */
let floatField = (name: string, value: float): field => {
  {name: name, value: Float(value)}
}

/** Create a boolean field */
let boolField = (name: string, value: bool): field => {
  {name: name, value: Boolean(value)}
}
