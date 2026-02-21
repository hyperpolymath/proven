-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Environment variable types and constraints
|||
||| This module defines types for safe environment variable access including:
||| - Variable names with validation
||| - Value types with bounds
||| - Security classifications
module Proven.SafeEnv.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Environment Variable Names
--------------------------------------------------------------------------------

||| Valid environment variable name characters
public export
isValidEnvChar : Char -> Bool
isValidEnvChar c = isAlphaNum c || c == '_'

||| Check if name is valid (uppercase letters, digits, underscore)
public export
isValidEnvName : String -> Bool
isValidEnvName s =
  let chars = unpack s
  in not (null chars) &&
     all isValidEnvChar chars &&
     not (isDigit (case chars of (c :: _) => c; [] => '0'))

||| A validated environment variable name
public export
record EnvName where
  constructor MkEnvName
  name : String
  {auto prf : isValidEnvName name = True}

public export
Show EnvName where
  show env = env.name

public export
Eq EnvName where
  e1 == e2 = e1.name == e2.name

||| Create validated env name
public export
envName : (name : String) -> {auto prf : isValidEnvName name = True} -> EnvName
envName name = MkEnvName name

||| Try to create env name
public export
tryEnvName : String -> Maybe EnvName
tryEnvName s =
  if isValidEnvName s
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    then Just (MkEnvName s {prf = believe_me Refl})
    else Nothing

--------------------------------------------------------------------------------
-- Environment Variable Values
--------------------------------------------------------------------------------

||| Environment variable value with bounds
public export
record EnvValue where
  constructor MkEnvValue
  raw : String
  maxLength : Nat

public export
Show EnvValue where
  show v = v.raw

public export
Eq EnvValue where
  v1 == v2 = v1.raw == v2.raw

||| Create bounded env value
public export
boundedValue : (maxLen : Nat) -> String -> Maybe EnvValue
boundedValue maxLen s =
  if length (unpack s) <= maxLen
    then Just (MkEnvValue s maxLen)
    else Nothing

||| Default max value length (64KB)
public export
defaultMaxValueLength : Nat
defaultMaxValueLength = 65536

--------------------------------------------------------------------------------
-- Security Classification
--------------------------------------------------------------------------------

||| Security classification for environment variables
public export
data EnvSecurity : Type where
  ||| Public variables safe to log
  Public : EnvSecurity
  ||| Sensitive variables (passwords, keys) - never log
  Sensitive : EnvSecurity
  ||| Secret variables (tokens, credentials) - redact in logs
  Secret : EnvSecurity

public export
Show EnvSecurity where
  show Public = "public"
  show Sensitive = "sensitive"
  show Secret = "secret"

public export
Eq EnvSecurity where
  Public == Public = True
  Sensitive == Sensitive = True
  Secret == Secret = True
  _ == _ = False

||| Known sensitive variable patterns
public export
sensitivePatterns : List String
sensitivePatterns =
  [ "PASSWORD"
  , "SECRET"
  , "TOKEN"
  , "KEY"
  , "API_KEY"
  , "APIKEY"
  , "PRIVATE"
  , "CREDENTIAL"
  , "AUTH"
  ]

||| Check if variable name looks sensitive
public export
isSensitiveName : String -> Bool
isSensitiveName name =
  let upper = toUpper name
  in any (\pat => isInfixOf pat upper) sensitivePatterns

||| Classify a variable by name
public export
classifyByName : String -> EnvSecurity
classifyByName name =
  if isSensitiveName name
    then Sensitive
    else Public

--------------------------------------------------------------------------------
-- Environment Variable Entry
--------------------------------------------------------------------------------

||| An environment variable entry
public export
record EnvEntry where
  constructor MkEnvEntry
  name : EnvName
  value : EnvValue
  security : EnvSecurity

public export
Show EnvEntry where
  show entry = case entry.security of
    Public => entry.name.name ++ "=" ++ entry.value.raw
    Sensitive => entry.name.name ++ "=<SENSITIVE>"
    Secret => entry.name.name ++ "=<SECRET>"

public export
Eq EnvEntry where
  e1 == e2 = e1.name == e2.name && e1.value == e2.value

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Environment access errors
public export
data EnvError : Type where
  ||| Variable not found
  NotFound : (name : String) -> EnvError

  ||| Invalid variable name
  InvalidName : (name : String) -> (reason : String) -> EnvError

  ||| Value exceeds maximum length
  ValueTooLong : (name : String) -> (length : Nat) -> (maxLength : Nat) -> EnvError

  ||| Type conversion failed
  TypeConversionFailed : (name : String) -> (value : String) -> (targetType : String) -> EnvError

  ||| Variable is sensitive
  SensitiveVariable : (name : String) -> EnvError

  ||| Access denied
  AccessDenied : (name : String) -> (reason : String) -> EnvError

public export
Show EnvError where
  show (NotFound name) = "Environment variable not found: " ++ name
  show (InvalidName name reason) = "Invalid variable name '" ++ name ++ "': " ++ reason
  show (ValueTooLong name len maxLen) =
    "Value too long for " ++ name ++ ": " ++ show len ++ " > " ++ show maxLen
  show (TypeConversionFailed name value target) =
    "Cannot convert " ++ name ++ "='" ++ value ++ "' to " ++ target
  show (SensitiveVariable name) = "Variable " ++ name ++ " is sensitive"
  show (AccessDenied name reason) = "Access denied to " ++ name ++ ": " ++ reason

public export
Eq EnvError where
  NotFound n1 == NotFound n2 = n1 == n2
  InvalidName n1 r1 == InvalidName n2 r2 = n1 == n2 && r1 == r2
  ValueTooLong n1 l1 m1 == ValueTooLong n2 l2 m2 = n1 == n2 && l1 == l2 && m1 == m2
  TypeConversionFailed n1 v1 t1 == TypeConversionFailed n2 v2 t2 = n1 == n2 && v1 == v2 && t1 == t2
  SensitiveVariable n1 == SensitiveVariable n2 = n1 == n2
  AccessDenied n1 r1 == AccessDenied n2 r2 = n1 == n2 && r1 == r2
  _ == _ = False

||| Result type for environment operations
public export
EnvResult : Type -> Type
EnvResult = Result EnvError

--------------------------------------------------------------------------------
-- Access Options
--------------------------------------------------------------------------------

||| Options for environment variable access
public export
record EnvOptions where
  constructor MkEnvOptions
  ||| Maximum value length
  maxValueLength : Nat
  ||| Allow access to sensitive variables
  allowSensitive : Bool
  ||| Require uppercase names
  requireUppercase : Bool
  ||| Allowed variable patterns (empty = all allowed)
  allowedPatterns : List String
  ||| Blocked variable patterns
  blockedPatterns : List String

||| Default access options
public export
defaultOptions : EnvOptions
defaultOptions = MkEnvOptions
  { maxValueLength = defaultMaxValueLength
  , allowSensitive = False
  , requireUppercase = False
  , allowedPatterns = []
  , blockedPatterns = []
  }

||| Strict access options
public export
strictOptions : EnvOptions
strictOptions = MkEnvOptions
  { maxValueLength = 4096
  , allowSensitive = False
  , requireUppercase = True
  , allowedPatterns = []
  , blockedPatterns = sensitivePatterns
  }

||| Permissive options (trusted context)
public export
permissiveOptions : EnvOptions
permissiveOptions = MkEnvOptions
  { maxValueLength = 1048576  -- 1MB
  , allowSensitive = True
  , requireUppercase = False
  , allowedPatterns = []
  , blockedPatterns = []
  }

--------------------------------------------------------------------------------
-- Common Variable Names
--------------------------------------------------------------------------------

||| Well-known environment variable names
public export
wellKnownVars : List String
wellKnownVars =
  [ "PATH", "HOME", "USER", "SHELL", "LANG", "TERM"
  , "PWD", "OLDPWD", "HOSTNAME", "LOGNAME"
  , "EDITOR", "VISUAL", "PAGER"
  , "TZ", "LC_ALL", "LC_CTYPE"
  , "XDG_CONFIG_HOME", "XDG_DATA_HOME", "XDG_CACHE_HOME"
  , "TMPDIR", "TEMP", "TMP"
  ]

||| Check if name is well-known
public export
isWellKnown : String -> Bool
isWellKnown name = name `elem` wellKnownVars
