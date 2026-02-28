-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- CLI Argument Parser
--
-- Tokenises and parses command-line arguments against a command definition.
-- Matches options by short (-v) or long (--verbose) name, validates
-- required options are present, and handles --help/--version.
-- All parse failures are returned as typed errors — no crashes.

module CLI.Parser

import CLI.ArgType
import CLI.Option
import CLI.Command

%default total

-- ============================================================================
-- Maximum arguments limit
-- ============================================================================

||| Maximum number of arguments to accept.
||| Prevents resource exhaustion from pathologically long argument lists.
public export
maxArgs : Nat
maxArgs = 256

-- ============================================================================
-- Parse Result
-- ============================================================================

||| A parsed option value: the option definition paired with its parsed value.
public export
record ParsedOption where
  constructor MkParsedOption
  ||| The option that was matched
  option : CLIOption
  ||| The parsed value
  value  : ArgValue

public export
Show ParsedOption where
  show po = "--" ++ po.option.longName ++ "=" ++ show po.value

||| The successful result of parsing a command's arguments.
public export
record ParsedArgs where
  constructor MkParsedArgs
  ||| The command that was matched (may be a subcommand)
  command     : CLICommand
  ||| Parsed option values
  options     : List ParsedOption
  ||| Positional arguments (after options)
  positionals : List String
  ||| Whether --help was requested
  helpRequested    : Bool
  ||| Whether --version was requested
  versionRequested : Bool

-- ============================================================================
-- Parse Errors
-- ============================================================================

||| Errors that can occur during argument parsing.
public export
data ParseError : Type where
  ||| Unrecognised option
  UnknownOption     : (raw : String) -> ParseError
  ||| Required option was not provided
  MissingRequired   : (optionName : String) -> ParseError
  ||| Option value failed type validation
  InvalidValue      : (optionName : String) -> ArgParseError -> ParseError
  ||| Option provided without a value (e.g., --port with no argument)
  MissingValue      : (optionName : String) -> ParseError
  ||| Too many arguments
  TooManyArgs       : (count : Nat) -> ParseError
  ||| Unknown subcommand
  UnknownSubcommand : (name : String) -> (available : List String) -> ParseError
  ||| No command specified when subcommands exist
  NoCommandSpecified : (available : List String) -> ParseError

public export
Show ParseError where
  show (UnknownOption r)       = "Unknown option: " ++ r
  show (MissingRequired n)     = "Missing required option: --" ++ n
  show (InvalidValue n e)      = "Invalid value for --" ++ n ++ ": " ++ show e
  show (MissingValue n)        = "Missing value for option: --" ++ n
  show (TooManyArgs c)         = "Too many arguments: " ++ show c
                                 ++ " (max " ++ show maxArgs ++ ")"
  show (UnknownSubcommand n a) = "Unknown subcommand '" ++ n
                                 ++ "'. Available: " ++ show a
  show (NoCommandSpecified a)  = "No command specified. Available: " ++ show a

-- ============================================================================
-- Token Classification
-- ============================================================================

||| Classify a raw argument string into a token type.
public export
data Token : Type where
  ||| Long option (e.g., "--verbose" or "--port=8080")
  LongOpt  : (name : String) -> (value : Maybe String) -> Token
  ||| Short option (e.g., "-v" or "-p8080")
  ShortOpt : (char : Char) -> (value : Maybe String) -> Token
  ||| Positional argument or subcommand name
  Positional : String -> Token
  ||| End-of-options marker (--)
  EndOfOptions : Token

public export
Show Token where
  show (LongOpt n v)  = "LongOpt(--" ++ n ++ maybe "" (\x => "=" ++ x) v ++ ")"
  show (ShortOpt c v) = "ShortOpt(-" ++ singleton c ++ maybe "" id v ++ ")"
  show (Positional s) = "Positional(" ++ s ++ ")"
  show EndOfOptions   = "--"

||| Tokenise a single argument string.
public export
tokenise : String -> Token
tokenise "--"  = EndOfOptions
tokenise s     =
  let cs = unpack s
  in case cs of
    ('-' :: '-' :: rest) =>
      let name = pack rest
      in case break (== '=') (unpack name) of
           (before, [])        => LongOpt (pack before) Nothing
           (before, _ :: after) => LongOpt (pack before) (Just (pack after))
    ('-' :: c :: [])   => ShortOpt c Nothing
    ('-' :: c :: rest)  => ShortOpt c (Just (pack rest))
    _                   => Positional s

||| Tokenise a list of argument strings.
public export
tokeniseAll : List String -> List Token
tokeniseAll = map tokenise

-- ============================================================================
-- Parse Logic
-- ============================================================================

||| Try to match a token against the command's options and produce a ParsedOption.
||| Returns Nothing if the token is not an option match.
public export
matchLongOption : CLICommand -> String -> Maybe String -> List Token
                -> Either ParseError (Maybe ParsedOption, List Token)
matchLongOption cmd name mval rest =
  case findByLong name cmd.options of
    Nothing  => Left (UnknownOption ("--" ++ name))
    Just opt =>
      case mval of
        Just v  => case parseTypedArg opt.argType v of
                     Left err  => Left (InvalidValue name err)
                     Right val => Right (Just (MkParsedOption opt val), rest)
        Nothing =>
          -- For BoolArg, treat as "true" when no value given
          case opt.argType of
            BoolArg => Right (Just (MkParsedOption opt (BoolVal True)), rest)
            _       =>
              case rest of
                []          => Left (MissingValue name)
                (Positional v :: rest') =>
                  case parseTypedArg opt.argType v of
                    Left err  => Left (InvalidValue name err)
                    Right val => Right (Just (MkParsedOption opt val), rest')
                _  => Left (MissingValue name)

||| Check that all required options have been provided.
public export
checkRequired : CLICommand -> List ParsedOption -> List ParseError
checkRequired cmd parsed =
  let provided = map (\po => po.option.longName) parsed
  in mapMaybe (\opt =>
       if opt.required && not (any (== opt.longName) provided)
         then Just (MissingRequired opt.longName)
         else Nothing
     ) cmd.options

||| Apply default values for optional options not explicitly provided.
public export
applyDefaults : CLICommand -> List ParsedOption -> List ParsedOption
applyDefaults cmd parsed =
  let provided = map (\po => po.option.longName) parsed
      defaults = mapMaybe (\opt =>
        case opt.defaultVal of
          Nothing => Nothing
          Just dv => if any (== opt.longName) provided
                       then Nothing
                       else case parseTypedArg opt.argType dv of
                              Left _    => Nothing
                              Right val => Just (MkParsedOption opt val)
        ) cmd.options
  in parsed ++ defaults
