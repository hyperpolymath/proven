-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe environment variable access
|||
||| This module provides safe access to environment variables including:
||| - Bounds-checked reads
||| - Type conversion with error handling
||| - Sensitivity filtering
module Proven.SafeEnv.Access

import Proven.Core
import Proven.SafeEnv.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Access Control
--------------------------------------------------------------------------------

||| Check if access is allowed by options
export
checkAccess : EnvOptions -> String -> EnvResult ()
checkAccess opts name =
  -- Check uppercase requirement
  if opts.requireUppercase && toUpper name /= name
    then Err (InvalidName name "must be uppercase")
    -- Check blocked patterns
    else if any (\pat => isInfixOf pat (toUpper name)) opts.blockedPatterns
      then Err (AccessDenied name "matches blocked pattern")
      -- Check allowed patterns (empty = all allowed)
      else if not (null opts.allowedPatterns) &&
              not (any (\pat => isInfixOf pat (toUpper name)) opts.allowedPatterns)
        then Err (AccessDenied name "does not match allowed patterns")
        -- Check sensitive
        else if isSensitiveName name && not opts.allowSensitive
          then Err (SensitiveVariable name)
          else Ok ()

||| Check value length
export
checkValueLength : EnvOptions -> String -> String -> EnvResult ()
checkValueLength opts name value =
  let len = length (unpack value)
  in if len > opts.maxValueLength
       then Err (ValueTooLong name len opts.maxValueLength)
       else Ok ()

--------------------------------------------------------------------------------
-- Environment Access (Pure Interface)
--------------------------------------------------------------------------------

||| Environment lookup function type
||| (Actual implementation would be IO-based)
public export
EnvLookup : Type
EnvLookup = String -> Maybe String

||| Get environment variable with default lookup (stub)
export
getEnvPure : EnvLookup -> EnvOptions -> String -> EnvResult String
getEnvPure lookup opts name = do
  checkAccess opts name
  case lookup name of
    Nothing => Err (NotFound name)
    Just value => do
      checkValueLength opts name value
      Ok value

||| Get environment variable with default value
export
getEnvOrPure : EnvLookup -> EnvOptions -> String -> String -> EnvResult String
getEnvOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value => do
      checkValueLength opts name value
      Ok value

||| Check if environment variable exists
export
hasEnvPure : EnvLookup -> EnvOptions -> String -> EnvResult Bool
hasEnvPure lookup opts name = do
  checkAccess opts name
  Ok (isJust (lookup name))

--------------------------------------------------------------------------------
-- Type Conversions
--------------------------------------------------------------------------------

||| Parse boolean from string
export
parseBool : String -> Maybe Bool
parseBool s =
  let lower = toLower s
  in if lower `elem` ["true", "yes", "1", "on", "enabled"]
       then Just True
       else if lower `elem` ["false", "no", "0", "off", "disabled", ""]
         then Just False
         else Nothing

||| Parse integer from string
export
parseInt : String -> Maybe Integer
parseInt = parseInteger

||| Parse natural from string
export
parseNat : String -> Maybe Nat
parseNat s = do
  i <- parseInteger s
  if i >= 0
    then Just (cast i)
    else Nothing

||| Parse port number from string
export
parsePort : String -> Maybe Nat
parsePort s = do
  n <- parseNat s
  if n > 0 && n <= 65535
    then Just n
    else Nothing

||| Parse list from string (comma-separated)
export
parseList : String -> List String
parseList s =
  map trim (split (== ',') s)

||| Parse key-value from string (key=value)
export
parseKeyValue : String -> Maybe (String, String)
parseKeyValue s =
  case break (== '=') s of
    (key, rest) =>
      if null (unpack rest)
        then Nothing
        else Just (key, drop 1 rest)

--------------------------------------------------------------------------------
-- Typed Getters
--------------------------------------------------------------------------------

||| Get boolean environment variable
export
getBoolPure : EnvLookup -> EnvOptions -> String -> EnvResult Bool
getBoolPure lookup opts name = do
  value <- getEnvPure lookup opts name
  case parseBool value of
    Just b => Ok b
    Nothing => Err (TypeConversionFailed name value "bool")

||| Get boolean with default
export
getBoolOrPure : EnvLookup -> EnvOptions -> String -> Bool -> EnvResult Bool
getBoolOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value =>
      case parseBool value of
        Just b => Ok b
        Nothing => Err (TypeConversionFailed name value "bool")

||| Get integer environment variable
export
getIntPure : EnvLookup -> EnvOptions -> String -> EnvResult Integer
getIntPure lookup opts name = do
  value <- getEnvPure lookup opts name
  case parseInt value of
    Just i => Ok i
    Nothing => Err (TypeConversionFailed name value "integer")

||| Get integer with default
export
getIntOrPure : EnvLookup -> EnvOptions -> String -> Integer -> EnvResult Integer
getIntOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value =>
      case parseInt value of
        Just i => Ok i
        Nothing => Err (TypeConversionFailed name value "integer")

||| Get natural number
export
getNatPure : EnvLookup -> EnvOptions -> String -> EnvResult Nat
getNatPure lookup opts name = do
  value <- getEnvPure lookup opts name
  case parseNat value of
    Just n => Ok n
    Nothing => Err (TypeConversionFailed name value "natural")

||| Get natural with default
export
getNatOrPure : EnvLookup -> EnvOptions -> String -> Nat -> EnvResult Nat
getNatOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value =>
      case parseNat value of
        Just n => Ok n
        Nothing => Err (TypeConversionFailed name value "natural")

||| Get port number
export
getPortPure : EnvLookup -> EnvOptions -> String -> EnvResult Nat
getPortPure lookup opts name = do
  value <- getEnvPure lookup opts name
  case parsePort value of
    Just p => Ok p
    Nothing => Err (TypeConversionFailed name value "port (1-65535)")

||| Get port with default
export
getPortOrPure : EnvLookup -> EnvOptions -> String -> Nat -> EnvResult Nat
getPortOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value =>
      case parsePort value of
        Just p => Ok p
        Nothing => Err (TypeConversionFailed name value "port (1-65535)")

||| Get list environment variable
export
getListPure : EnvLookup -> EnvOptions -> String -> EnvResult (List String)
getListPure lookup opts name = do
  value <- getEnvPure lookup opts name
  Ok (parseList value)

||| Get list with default
export
getListOrPure : EnvLookup -> EnvOptions -> String -> List String -> EnvResult (List String)
getListOrPure lookup opts name def = do
  checkAccess opts name
  case lookup name of
    Nothing => Ok def
    Just value => do
      checkValueLength opts name value
      Ok (parseList value)

--------------------------------------------------------------------------------
-- Required Variables
--------------------------------------------------------------------------------

||| Require multiple variables to be set
export
requireAllPure : EnvLookup -> EnvOptions -> List String -> EnvResult (List (String, String))
requireAllPure lookup opts names = traverse getVar names
  where
    getVar : String -> EnvResult (String, String)
    getVar name = do
      value <- getEnvPure lookup opts name
      Ok (name, value)

||| Require at least one of the variables
export
requireAnyPure : EnvLookup -> EnvOptions -> List String -> EnvResult (String, String)
requireAnyPure lookup opts [] = Err (NotFound "<none specified>")
requireAnyPure lookup opts (name :: names) =
  case getEnvPure lookup opts name of
    Ok value => Ok (name, value)
    Err _ => requireAnyPure lookup opts names

--------------------------------------------------------------------------------
-- Variable Filtering
--------------------------------------------------------------------------------

||| Get all variables matching a prefix
export
getWithPrefixPure : EnvLookup -> EnvOptions -> List String -> String -> EnvResult (List (String, String))
getWithPrefixPure lookup opts allNames prefix =
  let matching = filter (isPrefixOf prefix) allNames
  in traverse getIfAllowed matching
  where
    getIfAllowed : String -> EnvResult (String, String)
    getIfAllowed name = do
      value <- getEnvPure lookup opts name
      Ok (name, value)

||| Get all non-sensitive variables
export
getPublicVarsPure : EnvLookup -> EnvOptions -> List String -> EnvResult (List (String, String))
getPublicVarsPure lookup opts allNames =
  let publicNames = filter (not . isSensitiveName) allNames
  in traverse getVar publicNames
  where
    getVar : String -> EnvResult (String, String)
    getVar name = do
      value <- getEnvPure lookup opts name
      Ok (name, value)

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

||| Redact sensitive value for logging
export
redactValue : EnvSecurity -> String -> String
redactValue Public value = value
redactValue Sensitive _ = "***REDACTED***"
redactValue Secret _ = "***SECRET***"

||| Create loggable representation
export
toLoggable : String -> String -> String
toLoggable name value =
  let security = classifyByName name
  in name ++ "=" ++ redactValue security value

||| Create masked representation (show partial)
export
maskValue : String -> String
maskValue value =
  let chars = unpack value
      len = length chars
  in if len <= 4
       then pack (replicate len '*')
       else pack (take 2 chars) ++ pack (replicate (minus len 4) '*') ++ pack (takeLast 2 chars)
  where
    takeLast : Nat -> List a -> List a
    takeLast n xs = drop (minus (length xs) n) xs
