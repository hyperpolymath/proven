-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| CLI argument types and constraints
|||
||| This module defines types for safe CLI argument parsing including:
||| - Argument specifications
||| - Option types (flags, values, positional)
||| - Validation rules
module Proven.SafeArgs.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Argument Types
--------------------------------------------------------------------------------

||| Type of CLI argument
public export
data ArgType : Type where
  ||| Boolean flag (--verbose, -v)
  Flag : ArgType
  ||| Option with value (--output file)
  Option : ArgType
  ||| Positional argument
  Positional : ArgType
  ||| Rest arguments after --
  Rest : ArgType

public export
Show ArgType where
  show Flag = "flag"
  show Option = "option"
  show Positional = "positional"
  show Rest = "rest"

public export
Eq ArgType where
  Flag == Flag = True
  Option == Option = True
  Positional == Positional = True
  Rest == Rest = True
  _ == _ = False

--------------------------------------------------------------------------------
-- Argument Specification
--------------------------------------------------------------------------------

||| Argument specification
public export
record ArgSpec where
  constructor MkArgSpec
  ||| Long form (--name)
  long : Maybe String
  ||| Short form (-n)
  short : Maybe Char
  ||| Description for help
  description : String
  ||| Argument type
  argType : ArgType
  ||| Is required
  required : Bool
  ||| Default value
  defaultValue : Maybe String
  ||| Allowed values (empty = any)
  allowedValues : List String
  ||| Environment variable fallback
  envFallback : Maybe String

public export
Show ArgSpec where
  show spec =
    let longStr = maybe "" (\l => "--" ++ l) spec.long
        shortStr = maybe "" (\s => "-" ++ singleton s) spec.short
    in case (spec.long, spec.short) of
         (Just l, Just s) => "--" ++ l ++ ", -" ++ singleton s
         (Just l, Nothing) => "--" ++ l
         (Nothing, Just s) => "-" ++ singleton s
         (Nothing, Nothing) => "<positional>"

||| Create flag specification
public export
flag : String -> Char -> String -> ArgSpec
flag long short desc = MkArgSpec
  { long = Just long
  , short = Just short
  , description = desc
  , argType = Flag
  , required = False
  , defaultValue = Just "false"
  , allowedValues = []
  , envFallback = Nothing
  }

||| Create option specification
public export
option : String -> Char -> String -> ArgSpec
option long short desc = MkArgSpec
  { long = Just long
  , short = Just short
  , description = desc
  , argType = Option
  , required = False
  , defaultValue = Nothing
  , allowedValues = []
  , envFallback = Nothing
  }

||| Create required option
public export
requiredOption : String -> Char -> String -> ArgSpec
requiredOption long short desc =
  { required := True } (option long short desc)

||| Create positional argument
public export
positional : String -> ArgSpec
positional desc = MkArgSpec
  { long = Nothing
  , short = Nothing
  , description = desc
  , argType = Positional
  , required = True
  , defaultValue = Nothing
  , allowedValues = []
  , envFallback = Nothing
  }

||| Create optional positional
public export
optionalPositional : String -> String -> ArgSpec
optionalPositional desc def = MkArgSpec
  { long = Nothing
  , short = Nothing
  , description = desc
  , argType = Positional
  , required = False
  , defaultValue = Just def
  , allowedValues = []
  , envFallback = Nothing
  }

--------------------------------------------------------------------------------
-- Parsed Arguments
--------------------------------------------------------------------------------

||| Parsed argument value
public export
data ArgValue : Type where
  ||| Flag was set
  FlagSet : ArgValue
  ||| Flag was not set
  FlagNotSet : ArgValue
  ||| Option with value
  OptionValue : String -> ArgValue
  ||| Option not provided
  OptionMissing : ArgValue
  ||| Positional value
  PositionalValue : String -> ArgValue
  ||| Rest arguments
  RestValues : List String -> ArgValue

public export
Show ArgValue where
  show FlagSet = "true"
  show FlagNotSet = "false"
  show (OptionValue v) = v
  show OptionMissing = "<missing>"
  show (PositionalValue v) = v
  show (RestValues vs) = show vs

public export
Eq ArgValue where
  FlagSet == FlagSet = True
  FlagNotSet == FlagNotSet = True
  OptionValue v1 == OptionValue v2 = v1 == v2
  OptionMissing == OptionMissing = True
  PositionalValue v1 == PositionalValue v2 = v1 == v2
  RestValues vs1 == RestValues vs2 = vs1 == vs2
  _ == _ = False

||| Parsed arguments map
public export
ParsedArgs : Type
ParsedArgs = List (String, ArgValue)

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Argument parsing errors
public export
data ArgError : Type where
  ||| Unknown option
  UnknownOption : (name : String) -> ArgError

  ||| Missing required argument
  MissingRequired : (spec : ArgSpec) -> ArgError

  ||| Missing option value
  MissingValue : (option : String) -> ArgError

  ||| Invalid value
  InvalidValue : (option : String) -> (value : String) -> (reason : String) -> ArgError

  ||| Value not in allowed list
  NotAllowed : (option : String) -> (value : String) -> (allowed : List String) -> ArgError

  ||| Too many positional arguments
  TooManyPositional : (count : Nat) -> (expected : Nat) -> ArgError

  ||| Ambiguous short option
  AmbiguousShort : (char : Char) -> (matches : List String) -> ArgError

  ||| Type conversion failed
  TypeConversionFailed : (option : String) -> (value : String) -> (targetType : String) -> ArgError

  ||| Invalid argument format
  InvalidFormat : (arg : String) -> (reason : String) -> ArgError

public export
Show ArgError where
  show (UnknownOption name) = "Unknown option: " ++ name
  show (MissingRequired spec) = "Missing required argument: " ++ show spec
  show (MissingValue opt) = "Option " ++ opt ++ " requires a value"
  show (InvalidValue opt val reason) = "Invalid value for " ++ opt ++ ": " ++ val ++ " (" ++ reason ++ ")"
  show (NotAllowed opt val allowed) = "Value '" ++ val ++ "' not allowed for " ++ opt ++ ". Allowed: " ++ show allowed
  show (TooManyPositional count expected) = "Too many positional arguments: got " ++ show count ++ ", expected " ++ show expected
  show (AmbiguousShort c matches) = "Ambiguous short option -" ++ singleton c ++ ": matches " ++ show matches
  show (TypeConversionFailed opt val target) = "Cannot convert " ++ opt ++ "='" ++ val ++ "' to " ++ target
  show (InvalidFormat arg reason) = "Invalid argument format '" ++ arg ++ "': " ++ reason

public export
Eq ArgError where
  UnknownOption n1 == UnknownOption n2 = n1 == n2
  MissingValue o1 == MissingValue o2 = o1 == o2
  InvalidValue o1 v1 r1 == InvalidValue o2 v2 r2 = o1 == o2 && v1 == v2 && r1 == r2
  TooManyPositional c1 e1 == TooManyPositional c2 e2 = c1 == c2 && e1 == e2
  AmbiguousShort c1 m1 == AmbiguousShort c2 m2 = c1 == c2 && m1 == m2
  TypeConversionFailed o1 v1 t1 == TypeConversionFailed o2 v2 t2 = o1 == o2 && v1 == v2 && t1 == t2
  InvalidFormat a1 r1 == InvalidFormat a2 r2 = a1 == a2 && r1 == r2
  _ == _ = False

||| Result type for argument operations
public export
ArgResult : Type -> Type
ArgResult = Result ArgError

--------------------------------------------------------------------------------
-- Parser Options
--------------------------------------------------------------------------------

||| Parser configuration options
public export
record ParserOptions where
  constructor MkParserOptions
  ||| Allow unknown options (ignore them)
  allowUnknown : Bool
  ||| Stop parsing at first non-option
  stopAtNonOption : Bool
  ||| Allow bundled short options (-abc = -a -b -c)
  allowBundling : Bool
  ||| Allow = in option values (--opt=value)
  allowEquals : Bool
  ||| Case-sensitive option matching
  caseSensitive : Bool
  ||| Maximum argument length
  maxArgLength : Nat
  ||| Maximum number of arguments
  maxArgCount : Nat

||| Default parser options
public export
defaultParserOptions : ParserOptions
defaultParserOptions = MkParserOptions
  { allowUnknown = False
  , stopAtNonOption = False
  , allowBundling = True
  , allowEquals = True
  , caseSensitive = True
  , maxArgLength = 4096
  , maxArgCount = 1000
  }

||| Strict parser options
public export
strictParserOptions : ParserOptions
strictParserOptions = MkParserOptions
  { allowUnknown = False
  , stopAtNonOption = False
  , allowBundling = False
  , allowEquals = True
  , caseSensitive = True
  , maxArgLength = 1024
  , maxArgCount = 100
  }

||| Permissive parser options
public export
permissiveParserOptions : ParserOptions
permissiveParserOptions = MkParserOptions
  { allowUnknown = True
  , stopAtNonOption = True
  , allowBundling = True
  , allowEquals = True
  , caseSensitive = False
  , maxArgLength = 65536
  , maxArgCount = 10000
  }

--------------------------------------------------------------------------------
-- Command Definition
--------------------------------------------------------------------------------

||| Command/subcommand definition
public export
record Command where
  constructor MkCommand
  ||| Command name
  name : String
  ||| Command description
  description : String
  ||| Command arguments
  args : List ArgSpec
  ||| Subcommands
  subcommands : List Command

||| Create simple command
public export
command : String -> String -> List ArgSpec -> Command
command name desc args = MkCommand name desc args []

||| Create command with subcommands
public export
commandWithSubs : String -> String -> List ArgSpec -> List Command -> Command
commandWithSubs = MkCommand

--------------------------------------------------------------------------------
-- Help Generation
--------------------------------------------------------------------------------

||| Generate usage string for spec
public export
specUsage : ArgSpec -> String
specUsage spec = case spec.argType of
  Flag => case (spec.long, spec.short) of
            (Just l, Just s) => "[--" ++ l ++ "|-" ++ singleton s ++ "]"
            (Just l, Nothing) => "[--" ++ l ++ "]"
            (Nothing, Just s) => "[-" ++ singleton s ++ "]"
            _ => ""
  Option => case (spec.long, spec.short, spec.required) of
              (Just l, Just s, True) => "--" ++ l ++ "|-" ++ singleton s ++ " <value>"
              (Just l, Just s, False) => "[--" ++ l ++ "|-" ++ singleton s ++ " <value>]"
              (Just l, Nothing, True) => "--" ++ l ++ " <value>"
              (Just l, Nothing, False) => "[--" ++ l ++ " <value>]"
              (Nothing, Just s, True) => "-" ++ singleton s ++ " <value>"
              (Nothing, Just s, False) => "[-" ++ singleton s ++ " <value>]"
              _ => ""
  Positional => if spec.required then "<arg>" else "[<arg>]"
  Rest => "[-- <args>...]"

||| Generate help line for spec
public export
specHelp : ArgSpec -> String
specHelp spec =
  let nameStr = case (spec.long, spec.short) of
                  (Just l, Just s) => "  --" ++ l ++ ", -" ++ singleton s
                  (Just l, Nothing) => "  --" ++ l
                  (Nothing, Just s) => "  -" ++ singleton s
                  _ => "  <positional>"
      defaultStr = maybe "" (\d => " (default: " ++ d ++ ")") spec.defaultValue
      envStr = maybe "" (\e => " [env: " ++ e ++ "]") spec.envFallback
  in nameStr ++ "\n      " ++ spec.description ++ defaultStr ++ envStr
