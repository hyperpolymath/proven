-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeEnv - Safe environment variable access
|||
||| This module provides safe environment variable operations including:
||| - Bounds-checked value reads
||| - Automatic sensitivity detection
||| - Type-safe conversions (bool, int, port, list)
||| - Safe logging with redaction
|||
||| Example usage:
||| ```idris
||| -- Get a string variable
||| case getEnv defaultOptions "HOME" of
|||   Ok home => useHomeDir home
|||   Err (NotFound _) => useDefault
|||   Err e => handleError e
|||
||| -- Get typed variable with default
||| case getIntOr defaultOptions "PORT" 8080 of
|||   Ok port => startServer port
|||   Err e => handleError e
|||
||| -- Check sensitivity
||| let security = classifyByName "DATABASE_PASSWORD"
||| -- security = Sensitive
||| ```
module Proven.SafeEnv

import public Proven.Core
import public Proven.SafeEnv.Types
import public Proven.SafeEnv.Access
import public Proven.SafeEnv.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API (Pure - requires lookup function)
--------------------------------------------------------------------------------

||| Get environment variable
|||
||| Uses default options (64KB max, block sensitive).
public export
getEnv : EnvLookup -> String -> EnvResult String
getEnv lookup = getEnvPure lookup defaultOptions

||| Get environment variable with default
public export
getEnvOr : EnvLookup -> String -> String -> EnvResult String
getEnvOr lookup = getEnvOrPure lookup defaultOptions

||| Get environment variable with custom options
public export
getEnvWith : EnvLookup -> EnvOptions -> String -> EnvResult String
getEnvWith = getEnvPure

||| Check if environment variable exists
public export
hasEnv : EnvLookup -> String -> EnvResult Bool
hasEnv lookup = hasEnvPure lookup defaultOptions

--------------------------------------------------------------------------------
-- Typed Getters
--------------------------------------------------------------------------------

||| Get boolean environment variable
public export
getBool : EnvLookup -> String -> EnvResult Bool
getBool lookup = getBoolPure lookup defaultOptions

||| Get boolean with default
public export
getBoolOr : EnvLookup -> String -> Bool -> EnvResult Bool
getBoolOr lookup = getBoolOrPure lookup defaultOptions

||| Get integer environment variable
public export
getInt : EnvLookup -> String -> EnvResult Integer
getInt lookup = getIntPure lookup defaultOptions

||| Get integer with default
public export
getIntOr : EnvLookup -> String -> Integer -> EnvResult Integer
getIntOr lookup = getIntOrPure lookup defaultOptions

||| Get natural number environment variable
public export
getNat : EnvLookup -> String -> EnvResult Nat
getNat lookup = getNatPure lookup defaultOptions

||| Get natural with default
public export
getNatOr : EnvLookup -> String -> Nat -> EnvResult Nat
getNatOr lookup = getNatOrPure lookup defaultOptions

||| Get port number (1-65535)
public export
getPort : EnvLookup -> String -> EnvResult Nat
getPort lookup = getPortPure lookup defaultOptions

||| Get port with default
public export
getPortOr : EnvLookup -> String -> Nat -> EnvResult Nat
getPortOr lookup = getPortOrPure lookup defaultOptions

||| Get comma-separated list
public export
getList : EnvLookup -> String -> EnvResult (List String)
getList lookup = getListPure lookup defaultOptions

||| Get list with default
public export
getListOr : EnvLookup -> String -> List String -> EnvResult (List String)
getListOr lookup = getListOrPure lookup defaultOptions

--------------------------------------------------------------------------------
-- Multiple Variables
--------------------------------------------------------------------------------

||| Require all variables to be set
public export
requireAll : EnvLookup -> List String -> EnvResult (List (String, String))
requireAll lookup = requireAllPure lookup defaultOptions

||| Require any of the variables
public export
requireAny : EnvLookup -> List String -> EnvResult (String, String)
requireAny lookup = requireAnyPure lookup defaultOptions

||| Get variables with prefix
public export
getWithPrefix : EnvLookup -> List String -> String -> EnvResult (List (String, String))
getWithPrefix lookup = getWithPrefixPure lookup defaultOptions

||| Get all non-sensitive variables
public export
getPublicVars : EnvLookup -> List String -> EnvResult (List (String, String))
getPublicVars lookup = getPublicVarsPure lookup defaultOptions

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate variable name
public export
validateName : String -> EnvResult EnvName
validateName name =
  case tryEnvName name of
    Just n => Ok n
    Nothing =>
      if null (unpack name)
        then Err (InvalidName name "name cannot be empty")
        else if isDigit (case unpack name of (c :: _) => c; [] => '0')
          then Err (InvalidName name "name cannot start with digit")
          else Err (InvalidName name "name isInfixOf invalid characters")

||| Check if value is within bounds
public export
validateValue : Nat -> String -> String -> EnvResult EnvValue
validateValue maxLen name value =
  case boundedValue maxLen value of
    Just v => Ok v
    Nothing => Err (ValueTooLong name (length (unpack value)) maxLen)

--------------------------------------------------------------------------------
-- Entry Construction
--------------------------------------------------------------------------------

||| Create environment entry
public export
mkEntry : String -> String -> EnvResult EnvEntry
mkEntry name value = do
  n <- validateName name
  v <- validateValue defaultMaxValueLength name value
  let security = classifyByName name
  Ok (MkEnvEntry n v security)

||| Create public entry (no sensitivity check)
public export
mkPublicEntry : String -> String -> EnvResult EnvEntry
mkPublicEntry name value = do
  n <- validateName name
  v <- validateValue defaultMaxValueLength name value
  Ok (MkEnvEntry n v Public)

||| Create sensitive entry (explicit marking)
public export
mkSensitiveEntry : String -> String -> EnvResult EnvEntry
mkSensitiveEntry name value = do
  n <- validateName name
  v <- validateValue defaultMaxValueLength name value
  Ok (MkEnvEntry n v Sensitive)

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

||| Create loggable string from name and value
public export
loggable : String -> String -> String
loggable = toLoggable

||| Mask a value (show first/last 2 chars)
public export
mask : String -> String
mask = maskValue

||| Redact based on name classification
public export
redact : String -> String -> String
redact name value =
  let security = classifyByName name
  in redactValue security value

--------------------------------------------------------------------------------
-- Security Classification
--------------------------------------------------------------------------------

||| Classify variable by name
public export
classify : String -> EnvSecurity
classify = classifyByName

||| Check if name is sensitive
public export
isSensitive : String -> Bool
isSensitive = isSensitiveName

||| Check if name is well-known
public export
isKnown : String -> Bool
isKnown = isWellKnown

--------------------------------------------------------------------------------
-- Preset Options
--------------------------------------------------------------------------------

||| Default options (64KB max, block sensitive)
public export
defaults : EnvOptions
defaults = defaultOptions

||| Strict options (4KB max, require uppercase, block patterns)
public export
strict : EnvOptions
strict = strictOptions

||| Permissive options (1MB max, allow sensitive)
public export
permissive : EnvOptions
permissive = permissiveOptions

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is not found
public export
isNotFound : EnvError -> Bool
isNotFound (NotFound _) = True
isNotFound _ = False

||| Check if error is sensitivity related
public export
isSensitivityError : EnvError -> Bool
isSensitivityError (SensitiveVariable _) = True
isSensitivityError _ = False

||| Check if error is type conversion related
public export
isTypeError : EnvError -> Bool
isTypeError (TypeConversionFailed _ _ _) = True
isTypeError _ = False

||| Get user-friendly error message
public export
friendlyError : EnvError -> String
friendlyError (NotFound name) =
  "Environment variable '" ++ name ++ "' is not set."
friendlyError (InvalidName name reason) =
  "Invalid variable name '" ++ name ++ "': " ++ reason
friendlyError (ValueTooLong name len maxLen) =
  "Variable '" ++ name ++ "' value too long (" ++ show len ++ " chars, max " ++ show maxLen ++ ")."
friendlyError (TypeConversionFailed name value target) =
  "Cannot parse '" ++ name ++ "' as " ++ target ++ " (value: " ++ mask value ++ ")."
friendlyError (SensitiveVariable name) =
  "Access to sensitive variable '" ++ name ++ "' is blocked. Use permissive options if required."
friendlyError (AccessDenied name reason) =
  "Access to '" ++ name ++ "' denied: " ++ reason

--------------------------------------------------------------------------------
-- Common Patterns
--------------------------------------------------------------------------------

||| Get database URL (sensitive by default)
public export
getDatabaseUrl : EnvLookup -> EnvResult String
getDatabaseUrl lookup = getEnvPure lookup permissiveOptions "DATABASE_URL"

||| Get API key (sensitive, must use permissive)
public export
getApiKey : EnvLookup -> String -> EnvResult String
getApiKey lookup name = getEnvPure lookup permissiveOptions name

||| Get server port with default
public export
getServerPort : EnvLookup -> Nat -> EnvResult Nat
getServerPort lookup def = getPortOrPure lookup defaultOptions "PORT" def

||| Get debug mode
public export
getDebugMode : EnvLookup -> EnvResult Bool
getDebugMode lookup = getBoolOrPure lookup defaultOptions "DEBUG" False

||| Get log level (DEBUG, INFO, WARN, ERROR)
public export
getLogLevel : EnvLookup -> EnvResult String
getLogLevel lookup = getEnvOrPure lookup defaultOptions "LOG_LEVEL" "INFO"

||| Get allowed hosts list
public export
getAllowedHosts : EnvLookup -> EnvResult (List String)
getAllowedHosts lookup = getListOrPure lookup defaultOptions "ALLOWED_HOSTS" ["localhost"]

||| Get environment name (development, staging, production)
public export
getEnvironment : EnvLookup -> EnvResult String
getEnvironment lookup = getEnvOrPure lookup defaultOptions "ENVIRONMENT" "development"
