-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeArgs - Safe CLI argument parsing
|||
||| This module provides safe argument parsing including:
||| - Long (--name) and short (-n) options
||| - Positional arguments
||| - Required/optional with defaults
||| - Type-safe value access
||| - Help generation
|||
||| Example usage:
||| ```idris
||| -- Define argument specs
||| let specs = [ flag "verbose" 'v' "Enable verbose output"
|||             , option "output" 'o' "Output file"
|||             , requiredOption "input" 'i' "Input file"
|||             ]
|||
||| -- Parse arguments
||| case parse specs ["--verbose", "-i", "data.txt"] of
|||   Ok args => do
|||     let verbose = getFlag args "verbose"  -- True
|||     let input = getOption args "input"    -- Just "data.txt"
|||     processFile verbose input
|||   Err e => putStrLn (friendlyError e)
||| ```
module Proven.SafeArgs

import public Proven.Core
import public Proven.SafeArgs.Types
import public Proven.SafeArgs.Parser
import public Proven.SafeArgs.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Parse arguments with default options
public export
parse : List ArgSpec -> List String -> ArgResult ParsedArgs
parse = parseArgs defaultParserOptions

||| Parse arguments with custom options
public export
parseWith : ParserOptions -> List ArgSpec -> List String -> ArgResult ParsedArgs
parseWith = parseArgs

||| Parse arguments strictly (no unknown, no bundling)
public export
parseStrict : List ArgSpec -> List String -> ArgResult ParsedArgs
parseStrict = parseArgs strictParserOptions

||| Parse arguments permissively (allow unknown, stop at non-option)
public export
parsePermissive : List ArgSpec -> List String -> ArgResult ParsedArgs
parsePermissive = parseArgs permissiveParserOptions

--------------------------------------------------------------------------------
-- Spec Builders
--------------------------------------------------------------------------------

||| Create a flag (boolean switch)
public export
mkFlag : String -> Char -> String -> ArgSpec
mkFlag = flag

||| Create a flag with long name only
public export
mkLongFlag : String -> String -> ArgSpec
mkLongFlag long desc = MkArgSpec
  { long = Just long
  , short = Nothing
  , description = desc
  , argType = Flag
  , required = False
  , defaultValue = Just "false"
  , allowedValues = []
  , envFallback = Nothing
  }

||| Create an option (takes value)
public export
mkOption : String -> Char -> String -> ArgSpec
mkOption = option

||| Create an option with long name only
public export
mkLongOption : String -> String -> ArgSpec
mkLongOption long desc = MkArgSpec
  { long = Just long
  , short = Nothing
  , description = desc
  , argType = Option
  , required = False
  , defaultValue = Nothing
  , allowedValues = []
  , envFallback = Nothing
  }

||| Create a required option
public export
mkRequired : String -> Char -> String -> ArgSpec
mkRequired = requiredOption

||| Create a positional argument
public export
mkPositional : String -> ArgSpec
mkPositional = positional

||| Create an optional positional with default
public export
mkOptionalPositional : String -> String -> ArgSpec
mkOptionalPositional = optionalPositional

--------------------------------------------------------------------------------
-- Spec Modifiers
--------------------------------------------------------------------------------

||| Set default value
public export
withDefault : String -> ArgSpec -> ArgSpec
withDefault def spec = { defaultValue := Just def } spec

||| Set allowed values
public export
withAllowed : List String -> ArgSpec -> ArgSpec
withAllowed vals spec = { allowedValues := vals } spec

||| Set environment fallback
public export
withEnvFallback : String -> ArgSpec -> ArgSpec
withEnvFallback env spec = { envFallback := Just env } spec

||| Make required
public export
makeRequired : ArgSpec -> ArgSpec
makeRequired spec = { required := True } spec

||| Make optional
public export
makeOptional : ArgSpec -> ArgSpec
makeOptional spec = { required := False } spec

--------------------------------------------------------------------------------
-- Result Access
--------------------------------------------------------------------------------

||| Check if flag is set
public export
isSet : ParsedArgs -> String -> Bool
isSet = getFlag

||| Get option value as Maybe
public export
getValue : ParsedArgs -> String -> Maybe String
getValue = getOption

||| Get option value with default
public export
getValueOr : ParsedArgs -> String -> String -> String
getValueOr = getOptionOr

||| Get positional argument by index
public export
getPos : ParsedArgs -> Nat -> Maybe String
getPos = getPositional

||| Get all rest arguments (after --)
public export
getRestArgs : ParsedArgs -> List String
getRestArgs = getRest

--------------------------------------------------------------------------------
-- Typed Access
--------------------------------------------------------------------------------

||| Get boolean option
public export
getBool : ParsedArgs -> String -> ArgResult (Maybe Bool)
getBool args name = getTypedOption args name parseBoolArg

||| Get boolean with default
public export
getBoolOr : ParsedArgs -> String -> Bool -> ArgResult Bool
getBoolOr args name def = do
  result <- getBool args name
  Ok (fromMaybe def result)

||| Get integer option
public export
getInt : ParsedArgs -> String -> ArgResult (Maybe Integer)
getInt args name = getTypedOption args name parseIntArg

||| Get integer with default
public export
getIntOr : ParsedArgs -> String -> Integer -> ArgResult Integer
getIntOr args name def = do
  result <- getInt args name
  Ok (fromMaybe def result)

||| Get natural option
public export
getNat : ParsedArgs -> String -> ArgResult (Maybe Nat)
getNat args name = getTypedOption args name parseNatArg

||| Get natural with default
public export
getNatOr : ParsedArgs -> String -> Nat -> ArgResult Nat
getNatOr args name def = do
  result <- getNat args name
  Ok (fromMaybe def result)

||| Get double option
public export
getDouble : ParsedArgs -> String -> ArgResult (Maybe Double)
getDouble args name = getTypedOption args name parseDoubleArg

||| Get double with default
public export
getDoubleOr : ParsedArgs -> String -> Double -> ArgResult Double
getDoubleOr args name def = do
  result <- getDouble args name
  Ok (fromMaybe def result)

--------------------------------------------------------------------------------
-- Help Generation
--------------------------------------------------------------------------------

||| Generate usage string
public export
usage : String -> List ArgSpec -> String
usage = generateUsage

||| Generate full help text
public export
help : String -> String -> List ArgSpec -> String
help = generateHelp

||| Generate help for command
public export
commandHelp : Command -> String
commandHelp cmd =
  generateHelp cmd.name cmd.description cmd.args

--------------------------------------------------------------------------------
-- Common Patterns
--------------------------------------------------------------------------------

||| Standard help flag
public export
helpFlag : ArgSpec
helpFlag = flag "help" 'h' "Show help message"

||| Standard version flag
public export
versionFlag : ArgSpec
versionFlag = flag "version" 'V' "Show version"

||| Standard verbose flag
public export
verboseFlag : ArgSpec
verboseFlag = flag "verbose" 'v' "Enable verbose output"

||| Standard quiet flag
public export
quietFlag : ArgSpec
quietFlag = flag "quiet" 'q' "Suppress output"

||| Standard debug flag
public export
debugFlag : ArgSpec
debugFlag = flag "debug" 'd' "Enable debug mode"

||| Standard output option
public export
outputOption : ArgSpec
outputOption = option "output" 'o' "Output file"

||| Standard input option
public export
inputOption : ArgSpec
inputOption = option "input" 'i' "Input file"

||| Standard config option
public export
configOption : ArgSpec
configOption = option "config" 'c' "Configuration file"

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

||| Validate that a required option was provided
public export
requireOption : ParsedArgs -> String -> ArgResult String
requireOption args name =
  case getValue args name of
    Just v => Ok v
    Nothing => Err (MissingValue ("--" ++ name))

||| Validate that at least one of the options was provided
public export
requireAny : ParsedArgs -> List String -> ArgResult (String, String)
requireAny args [] = Err (MissingValue "<any>")
requireAny args (name :: names) =
  case getValue args name of
    Just v => Ok (name, v)
    Nothing => requireAny args names

||| Validate that all options were provided
public export
requireAll : ParsedArgs -> List String -> ArgResult (List (String, String))
requireAll args names = traverse getOpt names
  where
    getOpt : String -> ArgResult (String, String)
    getOpt name = do
      v <- requireOption args name
      Ok (name, v)

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is unknown option
public export
isUnknownOption : ArgError -> Bool
isUnknownOption (UnknownOption _) = True
isUnknownOption _ = False

||| Check if error is missing required
public export
isMissingRequired : ArgError -> Bool
isMissingRequired (MissingRequired _) = True
isMissingRequired _ = False

||| Check if error is type conversion
public export
isTypeError : ArgError -> Bool
isTypeError (TypeConversionFailed _ _ _) = True
isTypeError _ = False

||| Get user-friendly error message
public export
friendlyError : ArgError -> String
friendlyError (UnknownOption name) =
  "Unknown option: " ++ name ++ ". Use --help for available options."
friendlyError (MissingRequired spec) =
  "Missing required argument: " ++ show spec
friendlyError (MissingValue opt) =
  "Option " ++ opt ++ " requires a value."
friendlyError (InvalidValue opt val reason) =
  "Invalid value for " ++ opt ++ ": '" ++ val ++ "' - " ++ reason
friendlyError (NotAllowed opt val allowed) =
  "Value '" ++ val ++ "' not allowed for " ++ opt ++ ". Choose from: " ++ show allowed
friendlyError (TooManyPositional count expected) =
  "Too many arguments: got " ++ show count ++ ", expected at most " ++ show expected
friendlyError (AmbiguousShort c matches) =
  "Ambiguous option -" ++ singleton c ++ ": could match " ++ show matches
friendlyError (TypeConversionFailed opt val target) =
  "Cannot parse " ++ opt ++ " value '" ++ val ++ "' as " ++ target
friendlyError (InvalidFormat arg reason) =
  "Invalid argument '" ++ arg ++ "': " ++ reason

--------------------------------------------------------------------------------
-- Preset Options
--------------------------------------------------------------------------------

||| Default parser options
public export
defaults : ParserOptions
defaults = defaultParserOptions

||| Strict parser options
public export
strict : ParserOptions
strict = strictParserOptions

||| Permissive parser options
public export
permissive : ParserOptions
permissive = permissiveParserOptions
