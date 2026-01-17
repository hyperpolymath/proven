-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe structured logging with sensitive data protection
|||
||| This module provides type-safe structured logging:
||| - Log levels with filtering
||| - Structured log entries (JSON-compatible)
||| - Automatic sensitive data redaction
||| - Context propagation (request IDs, user IDs)
||| - Log rotation configuration
||| - Audit log specific types
|||
||| Security features:
||| - PII detection and automatic redaction
||| - Secret/credential detection
||| - Log injection prevention
||| - Safe serialization of user input
||| - Audit trail requirements
module Proven.SafeLog

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Log severity levels
public export
data LogLevel
  = TRACE
  | DEBUG
  | INFO
  | WARN
  | ERROR
  | FATAL
  | AUDIT   -- Special level for audit logs (always preserved)

||| Log level ordering
public export
Ord LogLevel where
  compare TRACE TRACE = EQ
  compare TRACE _ = LT
  compare DEBUG TRACE = GT
  compare DEBUG DEBUG = EQ
  compare DEBUG _ = LT
  compare INFO TRACE = GT
  compare INFO DEBUG = GT
  compare INFO INFO = EQ
  compare INFO _ = LT
  compare WARN TRACE = GT
  compare WARN DEBUG = GT
  compare WARN INFO = GT
  compare WARN WARN = EQ
  compare WARN _ = LT
  compare ERROR TRACE = GT
  compare ERROR DEBUG = GT
  compare ERROR INFO = GT
  compare ERROR WARN = GT
  compare ERROR ERROR = EQ
  compare ERROR _ = LT
  compare FATAL AUDIT = LT
  compare FATAL FATAL = EQ
  compare FATAL _ = GT
  compare AUDIT _ = GT

public export
Eq LogLevel where
  l1 == l2 = compare l1 l2 == EQ

||| Sensitivity classification for data
public export
data Sensitivity
  = Public         -- Safe to log anywhere
  | Internal       -- Internal use only
  | Confidential   -- Limited access
  | Secret         -- Must never be logged
  | PII            -- Personally identifiable information

||| Log field value types
public export
data LogValue
  = LString String
  | LInt Integer
  | LBool Bool
  | LDouble Double
  | LNull
  | LArray (List LogValue)
  | LObject (List (String, LogValue))
  | LRedacted String  -- Redacted with reason

||| Log field with sensitivity
public export
record LogField where
  constructor MkLogField
  name : String
  value : LogValue
  sensitivity : Sensitivity

||| Trace context for distributed tracing
public export
record TraceContext where
  constructor MkTraceContext
  traceId : String
  spanId : String
  parentSpanId : Maybe String
  sampled : Bool

||| Request context
public export
record RequestContext where
  constructor MkRequestContext
  requestId : String
  userId : Maybe String
  sessionId : Maybe String
  clientIp : Maybe String  -- May need redaction
  userAgent : Maybe String

||| Complete log context
public export
record LogContext where
  constructor MkLogContext
  service : String
  environment : String
  version : Maybe String
  hostname : Maybe String
  trace : Maybe TraceContext
  request : Maybe RequestContext
  customFields : List LogField

||| Structured log entry
public export
record LogEntry where
  constructor MkLogEntry
  timestamp : String      -- ISO 8601 format
  level : LogLevel
  message : String
  context : LogContext
  fields : List LogField
  exception : Maybe String
  stackTrace : Maybe (List String)

||| Log rotation strategy
public export
data RotationStrategy
  = RotateBySize Nat          -- Bytes
  | RotateByTime Nat          -- Seconds
  | RotateByCount Nat         -- Number of entries
  | RotateDaily
  | RotateHourly

||| Log output format
public export
data LogFormat
  = JSONFormat
  | TextFormat
  | CLFFormat          -- Common Log Format
  | SyslogFormat

||| Logger configuration
public export
record LoggerConfig where
  constructor MkLoggerConfig
  minLevel : LogLevel
  format : LogFormat
  rotation : Maybe RotationStrategy
  maxFileSize : Maybe Nat
  retentionDays : Maybe Nat
  redactPII : Bool
  redactSecrets : Bool
  includeStackTraces : Bool

||| Audit log event types
public export
data AuditEventType
  = UserLogin
  | UserLogout
  | PasswordChange
  | PermissionChange
  | DataAccess
  | DataModification
  | DataDeletion
  | ConfigChange
  | SecurityEvent
  | AdminAction

||| Audit log entry (stricter requirements)
public export
record AuditEntry where
  constructor MkAuditEntry
  eventId : String
  timestamp : String
  eventType : AuditEventType
  actor : String          -- Who performed the action
  target : String         -- What was affected
  action : String         -- What was done
  result : String         -- Success/Failure
  details : List LogField
  ipAddress : String
  userAgent : Maybe String
  previousValue : Maybe String
  newValue : Maybe String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during logging operations
public export
data LogError
  = EmptyMessage
  | InvalidTimestamp String
  | SensitiveDataInMessage String
  | LogInjectionDetected String
  | InvalidLogLevel String
  | MissingRequiredField String
  | InvalidTraceId String
  | InvalidSpanId String
  | MessageTooLong Nat
  | InvalidFieldName String
  | RedactionRequired String

public export
Show LogError where
  show EmptyMessage = "Log error: empty message"
  show (InvalidTimestamp ts) = "Log error: invalid timestamp '" ++ ts ++ "'"
  show (SensitiveDataInMessage pattern) = "Log security: sensitive data pattern detected '" ++ pattern ++ "'"
  show (LogInjectionDetected pattern) = "Log security: injection pattern detected '" ++ pattern ++ "'"
  show (InvalidLogLevel lvl) = "Log error: invalid log level '" ++ lvl ++ "'"
  show (MissingRequiredField field) = "Log error: missing required field '" ++ field ++ "'"
  show (InvalidTraceId id) = "Log error: invalid trace ID '" ++ id ++ "'"
  show (InvalidSpanId id) = "Log error: invalid span ID '" ++ id ++ "'"
  show (MessageTooLong len) = "Log error: message too long (" ++ show len ++ " chars)"
  show (InvalidFieldName name) = "Log error: invalid field name '" ++ name ++ "'"
  show (RedactionRequired field) = "Log security: field '" ++ field ++ "' requires redaction"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Maximum log message length
maxMessageLength : Nat
maxMessageLength = 10000

||| Maximum field name length
maxFieldNameLength : Nat
maxFieldNameLength = 128

||| Patterns that indicate sensitive data (simplified)
sensitivePatterns : List String
sensitivePatterns =
  [ "password", "passwd", "secret", "token", "key"
  , "credential", "auth", "bearer", "api_key", "apikey"
  , "private", "ssn", "social_security"
  ]

||| Patterns that indicate PII
piiPatterns : List String
piiPatterns =
  [ "email", "phone", "address", "ssn", "birth"
  , "credit_card", "card_number", "cvv", "expiry"
  , "passport", "license", "ip_address"
  ]

||| Log injection patterns to detect
injectionPatterns : List String
injectionPatterns =
  [ "\n", "\r", "\x00", "\x1b"  -- Newlines and control characters
  ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

||| Check if string contains any pattern
containsAny : List String -> String -> Bool
containsAny patterns str =
  let lower = toLower str
  in any (\p => isInfixOf p lower) patterns

||| Detect sensitive data in string
detectSensitive : String -> Maybe String
detectSensitive str =
  let lower = toLower str
  in find (\p => isInfixOf p lower) sensitivePatterns

||| Detect PII in string
detectPII : String -> Maybe String
detectPII str =
  let lower = toLower str
  in find (\p => isInfixOf p lower) piiPatterns

||| Detect log injection attempt
detectInjection : String -> Maybe String
detectInjection str =
  find (\p => isInfixOf p str) injectionPatterns

||| Sanitize string for safe logging (remove control chars)
sanitizeForLog : String -> String
sanitizeForLog str =
  pack $ filter (\c => ord c >= 32 && ord c /= 127) (unpack str)

||| Redact sensitive portions of a string
redactString : String -> String
redactString str =
  if length str <= 4
    then "***"
    else strSubstr 0 2 str ++ "***" ++ strSubstr (minus (length str) 2) 2 str

--------------------------------------------------------------------------------
-- Validation functions
--------------------------------------------------------------------------------

||| Validate a log message
public export
validateMessage : String -> Either LogError String
validateMessage "" = Left EmptyMessage
validateMessage msg =
  let len = length msg in
  if len > maxMessageLength
    then Left (MessageTooLong len)
    else case detectInjection msg of
      Just pattern => Left (LogInjectionDetected pattern)
      Nothing => Right (sanitizeForLog msg)

||| Validate a field name
public export
validateFieldName : String -> Either LogError String
validateFieldName "" = Left (InvalidFieldName "empty")
validateFieldName name =
  if length name > maxFieldNameLength
    then Left (InvalidFieldName ("too long: " ++ name))
    else if containsAny [" ", "\t", "\n", ".", "="] name
      then Left (InvalidFieldName name)
      else Right name

||| Validate a trace ID (typically 32 hex chars)
public export
validateTraceId : String -> Either LogError String
validateTraceId "" = Left (InvalidTraceId "empty")
validateTraceId id =
  let len = length id in
  if len /= 32 && len /= 16
    then Left (InvalidTraceId ("invalid length: " ++ show len))
    else if not (all isHexDigit (unpack id))
      then Left (InvalidTraceId "not hex")
      else Right id
  where
    isHexDigit : Char -> Bool
    isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

||| Validate a span ID (typically 16 hex chars)
public export
validateSpanId : String -> Either LogError String
validateSpanId "" = Left (InvalidSpanId "empty")
validateSpanId id =
  let len = length id in
  if len /= 16 && len /= 8
    then Left (InvalidSpanId ("invalid length: " ++ show len))
    else if not (all isHexDigit (unpack id))
      then Left (InvalidSpanId "not hex")
      else Right id
  where
    isHexDigit : Char -> Bool
    isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

--------------------------------------------------------------------------------
-- Field construction with sensitivity
--------------------------------------------------------------------------------

||| Create a public log field
public export
publicField : String -> LogValue -> Either LogError LogField
publicField name value = do
  validName <- validateFieldName name
  pure (MkLogField validName value Public)

||| Create an internal log field
public export
internalField : String -> LogValue -> Either LogError LogField
internalField name value = do
  validName <- validateFieldName name
  pure (MkLogField validName value Internal)

||| Create a confidential log field (will be redacted in external logs)
public export
confidentialField : String -> LogValue -> Either LogError LogField
confidentialField name value = do
  validName <- validateFieldName name
  pure (MkLogField validName value Confidential)

||| Create a secret field (always redacted)
public export
secretField : String -> String -> Either LogError LogField
secretField name _ = do
  validName <- validateFieldName name
  pure (MkLogField validName (LRedacted "secret") Secret)

||| Create a PII field (redacted unless explicitly allowed)
public export
piiField : String -> String -> Either LogError LogField
piiField name value = do
  validName <- validateFieldName name
  pure (MkLogField validName (LString (redactString value)) PII)

--------------------------------------------------------------------------------
-- Log value construction
--------------------------------------------------------------------------------

||| Create a string log value
public export
logString : String -> LogValue
logString = LString . sanitizeForLog

||| Create an integer log value
public export
logInt : Integer -> LogValue
logInt = LInt

||| Create a boolean log value
public export
logBool : Bool -> LogValue
logBool = LBool

||| Create a null log value
public export
logNull : LogValue
logNull = LNull

||| Create an array log value
public export
logArray : List LogValue -> LogValue
logArray = LArray

||| Create an object log value
public export
logObject : List (String, LogValue) -> LogValue
logObject = LObject

||| Create a redacted log value
public export
logRedacted : String -> LogValue
logRedacted = LRedacted

--------------------------------------------------------------------------------
-- Context construction
--------------------------------------------------------------------------------

||| Create an empty log context
public export
emptyContext : String -> String -> LogContext
emptyContext service env =
  MkLogContext service env Nothing Nothing Nothing Nothing []

||| Add trace context
public export
withTrace : TraceContext -> LogContext -> LogContext
withTrace trace ctx = { trace := Just trace } ctx

||| Add request context
public export
withRequest : RequestContext -> LogContext -> LogContext
withRequest req ctx = { request := Just req } ctx

||| Add custom field to context
public export
withField : LogField -> LogContext -> LogContext
withField field ctx = { customFields $= (field ::) } ctx

--------------------------------------------------------------------------------
-- Log entry construction
--------------------------------------------------------------------------------

||| Create a basic log entry
public export
mkLogEntry : String -> LogLevel -> String -> LogContext -> Either LogError LogEntry
mkLogEntry timestamp level msg ctx = do
  validMsg <- validateMessage msg
  pure (MkLogEntry timestamp level validMsg ctx [] Nothing Nothing)

||| Add field to log entry
public export
addField : LogField -> LogEntry -> LogEntry
addField field entry = { fields $= (field ::) } entry

||| Add exception to log entry
public export
withException : String -> LogEntry -> LogEntry
withException exc entry = { exception := Just (sanitizeForLog exc) } entry

||| Add stack trace to log entry
public export
withStackTrace : List String -> LogEntry -> LogEntry
withStackTrace trace entry = { stackTrace := Just (map sanitizeForLog trace) } entry

--------------------------------------------------------------------------------
-- Audit entry construction
--------------------------------------------------------------------------------

||| Create an audit log entry
public export
mkAuditEntry : String -> String -> AuditEventType -> String -> String -> String -> String -> Either LogError AuditEntry
mkAuditEntry eventId timestamp eventType actor target action result = do
  -- Audit entries have strict requirements
  if eventId == ""
    then Left (MissingRequiredField "eventId")
    else if actor == ""
      then Left (MissingRequiredField "actor")
      else if target == ""
        then Left (MissingRequiredField "target")
        else Right (MkAuditEntry
          eventId
          timestamp
          eventType
          (sanitizeForLog actor)
          (sanitizeForLog target)
          (sanitizeForLog action)
          result
          []
          ""
          Nothing
          Nothing
          Nothing)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show log level
public export
showLogLevel : LogLevel -> String
showLogLevel TRACE = "TRACE"
showLogLevel DEBUG = "DEBUG"
showLogLevel INFO = "INFO"
showLogLevel WARN = "WARN"
showLogLevel ERROR = "ERROR"
showLogLevel FATAL = "FATAL"
showLogLevel AUDIT = "AUDIT"

||| Parse log level
public export
parseLogLevel : String -> Either LogError LogLevel
parseLogLevel "TRACE" = Right TRACE
parseLogLevel "DEBUG" = Right DEBUG
parseLogLevel "INFO" = Right INFO
parseLogLevel "WARN" = Right WARN
parseLogLevel "WARNING" = Right WARN
parseLogLevel "ERROR" = Right ERROR
parseLogLevel "FATAL" = Right FATAL
parseLogLevel "AUDIT" = Right AUDIT
parseLogLevel s = Left (InvalidLogLevel s)

||| Show sensitivity
public export
showSensitivity : Sensitivity -> String
showSensitivity Public = "public"
showSensitivity Internal = "internal"
showSensitivity Confidential = "confidential"
showSensitivity Secret = "secret"
showSensitivity PII = "pii"

||| Show audit event type
public export
showAuditEventType : AuditEventType -> String
showAuditEventType UserLogin = "USER_LOGIN"
showAuditEventType UserLogout = "USER_LOGOUT"
showAuditEventType PasswordChange = "PASSWORD_CHANGE"
showAuditEventType PermissionChange = "PERMISSION_CHANGE"
showAuditEventType DataAccess = "DATA_ACCESS"
showAuditEventType DataModification = "DATA_MODIFICATION"
showAuditEventType DataDeletion = "DATA_DELETION"
showAuditEventType ConfigChange = "CONFIG_CHANGE"
showAuditEventType SecurityEvent = "SECURITY_EVENT"
showAuditEventType AdminAction = "ADMIN_ACTION"

||| Show log value (with redaction)
public export
showLogValue : LogValue -> String
showLogValue (LString s) = "\"" ++ s ++ "\""
showLogValue (LInt i) = show i
showLogValue (LBool True) = "true"
showLogValue (LBool False) = "false"
showLogValue (LDouble d) = show d
showLogValue LNull = "null"
showLogValue (LArray vals) = "[" ++ concat (intersperse ", " (map showLogValue vals)) ++ "]"
showLogValue (LObject fields) =
  "{" ++ concat (intersperse ", " (map showKV fields)) ++ "}"
  where
    showKV : (String, LogValue) -> String
    showKV (k, v) = "\"" ++ k ++ "\": " ++ showLogValue v
showLogValue (LRedacted reason) = "\"[REDACTED: " ++ reason ++ "]\""

||| Apply redaction policy to log field
public export
applyRedaction : Bool -> Bool -> LogField -> LogField
applyRedaction redactPII redactSecrets field =
  case field.sensitivity of
    Secret => { value := LRedacted "secret" } field
    PII => if redactPII
             then { value := LRedacted "pii" } field
             else field
    Confidential => if redactSecrets
                      then { value := LRedacted "confidential" } field
                      else field
    _ => field

||| Serialize log entry to JSON format
public export
toJSON : LoggerConfig -> LogEntry -> String
toJSON config entry =
  let redactedFields = map (applyRedaction config.redactPII config.redactSecrets) entry.fields
  in "{"
     ++ "\"timestamp\": \"" ++ entry.timestamp ++ "\", "
     ++ "\"level\": \"" ++ showLogLevel entry.level ++ "\", "
     ++ "\"message\": \"" ++ entry.message ++ "\", "
     ++ "\"service\": \"" ++ entry.context.service ++ "\", "
     ++ "\"environment\": \"" ++ entry.context.environment ++ "\""
     ++ maybe "" (\t => ", \"trace_id\": \"" ++ t.traceId ++ "\"") entry.context.trace
     ++ maybe "" (\r => ", \"request_id\": \"" ++ r.requestId ++ "\"") entry.context.request
     ++ (if null redactedFields then "" else ", \"fields\": " ++ showFields redactedFields)
     ++ maybe "" (\e => ", \"exception\": \"" ++ e ++ "\"") entry.exception
     ++ "}"
  where
    showFields : List LogField -> String
    showFields fs = "{" ++ concat (intersperse ", " (map showField fs)) ++ "}"
    showField : LogField -> String
    showField f = "\"" ++ f.name ++ "\": " ++ showLogValue f.value

||| Serialize log entry to text format
public export
toText : LogEntry -> String
toText entry =
  entry.timestamp ++ " [" ++ showLogLevel entry.level ++ "] "
  ++ entry.context.service ++ " - " ++ entry.message
  ++ maybe "" (\t => " trace_id=" ++ t.traceId) entry.context.trace
  ++ maybe "" (\r => " request_id=" ++ r.requestId) entry.context.request

||| Serialize audit entry to JSON
public export
auditToJSON : AuditEntry -> String
auditToJSON entry =
  "{"
  ++ "\"event_id\": \"" ++ entry.eventId ++ "\", "
  ++ "\"timestamp\": \"" ++ entry.timestamp ++ "\", "
  ++ "\"event_type\": \"" ++ showAuditEventType entry.eventType ++ "\", "
  ++ "\"actor\": \"" ++ entry.actor ++ "\", "
  ++ "\"target\": \"" ++ entry.target ++ "\", "
  ++ "\"action\": \"" ++ entry.action ++ "\", "
  ++ "\"result\": \"" ++ entry.result ++ "\", "
  ++ "\"ip_address\": \"" ++ entry.ipAddress ++ "\""
  ++ maybe "" (\prev => ", \"previous_value\": \"" ++ prev ++ "\"") entry.previousValue
  ++ maybe "" (\new => ", \"new_value\": \"" ++ new ++ "\"") entry.newValue
  ++ "}"

--------------------------------------------------------------------------------
-- Log filtering
--------------------------------------------------------------------------------

||| Check if log level passes filter
public export
shouldLog : LogLevel -> LogLevel -> Bool
shouldLog minLevel entryLevel = entryLevel >= minLevel

||| Filter log entries by level
public export
filterByLevel : LogLevel -> List LogEntry -> List LogEntry
filterByLevel minLevel = filter (\e => shouldLog minLevel e.level)
