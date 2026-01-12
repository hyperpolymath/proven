-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| CLI argument parser
|||
||| This module provides argument parsing with:
||| - Long and short option support
||| - Positional argument handling
||| - Type conversion
||| - Validation
module Proven.SafeArgs.Parser

import Proven.Core
import Proven.SafeArgs.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Argument Classification
--------------------------------------------------------------------------------

||| Classify an argument string
public export
data ArgClass : Type where
  ||| Long option (--name)
  LongOpt : String -> ArgClass
  ||| Long option with value (--name=value)
  LongOptEq : String -> String -> ArgClass
  ||| Short option (-n)
  ShortOpt : Char -> ArgClass
  ||| Bundled short options (-abc)
  BundledOpts : List Char -> ArgClass
  ||| Short option with value (-n value or -nvalue)
  ShortOptVal : Char -> String -> ArgClass
  ||| End of options (--)
  EndOfOpts : ArgClass
  ||| Positional argument
  PositionalArg : String -> ArgClass

||| Classify a single argument
export
classifyArg : ParserOptions -> String -> ArgClass
classifyArg opts arg =
  case unpack arg of
    ('-' :: '-' :: []) => EndOfOpts
    ('-' :: '-' :: rest) =>
      case break (== '=') (pack rest) of
        (name, val) =>
          if null (unpack val)
            then LongOpt (pack rest)
            else if opts.allowEquals
              then LongOptEq name (drop 1 val)
              else LongOpt (pack rest)
    ('-' :: c :: []) => ShortOpt c
    ('-' :: c :: rest) =>
      if opts.allowBundling && all isAlpha rest
        then BundledOpts (c :: rest)
        else ShortOptVal c (pack rest)
    _ => PositionalArg arg

--------------------------------------------------------------------------------
-- Spec Lookup
--------------------------------------------------------------------------------

||| Find spec by long name
export
findByLong : List ArgSpec -> String -> Maybe ArgSpec
findByLong specs name =
  let searchName = name
  in find (\s => s.long == Just searchName) specs

||| Find spec by short name
export
findByShort : List ArgSpec -> Char -> Maybe ArgSpec
findByShort specs char =
  find (\s => s.short == Just char) specs

||| Get canonical name for spec
export
specName : ArgSpec -> String
specName spec = case spec.long of
  Just l => l
  Nothing => case spec.short of
    Just s => singleton s
    Nothing => "<positional>"

--------------------------------------------------------------------------------
-- Value Parsing
--------------------------------------------------------------------------------

||| Parse boolean value
export
parseBoolArg : String -> Maybe Bool
parseBoolArg s =
  let lower = toLower s
  in if lower `elem` ["true", "yes", "1", "on"]
       then Just True
       else if lower `elem` ["false", "no", "0", "off"]
         then Just False
         else Nothing

||| Parse integer value
export
parseIntArg : String -> Maybe Integer
parseIntArg = parseInteger

||| Parse natural value
export
parseNatArg : String -> Maybe Nat
parseNatArg s = do
  i <- parseInteger s
  if i >= 0 then Just (cast i) else Nothing

||| Parse double value
export
parseDoubleArg : String -> Maybe Double
parseDoubleArg = parseDouble

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check argument length
export
checkArgLength : ParserOptions -> String -> ArgResult ()
checkArgLength opts arg =
  if length (unpack arg) > opts.maxArgLength
    then Err (InvalidFormat arg "argument too long")
    else Ok ()

||| Check argument count
export
checkArgCount : ParserOptions -> Nat -> ArgResult ()
checkArgCount opts count =
  if count > opts.maxArgCount
    then Err (TooManyPositional count opts.maxArgCount)
    else Ok ()

||| Validate value against allowed list
export
validateAllowed : ArgSpec -> String -> ArgResult String
validateAllowed spec value =
  if null spec.allowedValues
    then Ok value
    else if value `elem` spec.allowedValues
      then Ok value
      else Err (NotAllowed (specName spec) value spec.allowedValues)

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state during parsing
record ParserState where
  constructor MkParserState
  ||| Remaining arguments to parse
  remaining : List String
  ||| Parsed results so far
  parsed : ParsedArgs
  ||| Positional args seen
  positionalCount : Nat
  ||| After -- separator
  afterSeparator : Bool

||| Initial parser state
initialState : List String -> ParserState
initialState args = MkParserState args [] 0 False

--------------------------------------------------------------------------------
-- Core Parser
--------------------------------------------------------------------------------

||| Parse arguments against specs
export
parseArgs : ParserOptions -> List ArgSpec -> List String -> ArgResult ParsedArgs
parseArgs opts specs args = do
  -- Check argument count
  checkArgCount opts (length args)
  -- Check each argument length
  traverse_ (checkArgLength opts) args
  -- Parse
  let state = initialState args
  finalState <- parseLoop opts specs state
  -- Check required
  checkRequired specs finalState.parsed
  where
    parseLoop : ParserOptions -> List ArgSpec -> ParserState -> ArgResult ParserState
    parseLoop opts specs state = case state.remaining of
      [] => Ok state
      (arg :: rest) =>
        if state.afterSeparator
          then -- Everything after -- is positional
            let newParsed = ("--rest", RestValues (arg :: rest)) :: state.parsed
            in Ok ({ remaining := [], parsed := newParsed } state)
          else case classifyArg opts arg of
            EndOfOpts =>
              parseLoop opts specs ({ remaining := rest, afterSeparator := True } state)

            LongOpt name =>
              case findByLong specs name of
                Nothing =>
                  if opts.allowUnknown
                    then parseLoop opts specs ({ remaining := rest } state)
                    else Err (UnknownOption ("--" ++ name))
                Just spec =>
                  if spec.argType == Flag
                    then let newParsed = (specName spec, FlagSet) :: state.parsed
                         in parseLoop opts specs ({ remaining := rest, parsed := newParsed } state)
                    else case rest of
                      [] => Err (MissingValue ("--" ++ name))
                      (val :: rest') => do
                        validated <- validateAllowed spec val
                        let newParsed = (specName spec, OptionValue validated) :: state.parsed
                        parseLoop opts specs ({ remaining := rest', parsed := newParsed } state)

            LongOptEq name value =>
              case findByLong specs name of
                Nothing =>
                  if opts.allowUnknown
                    then parseLoop opts specs ({ remaining := rest } state)
                    else Err (UnknownOption ("--" ++ name))
                Just spec =>
                  if spec.argType == Flag
                    then Err (InvalidFormat arg "flags don't take values")
                    else do
                      validated <- validateAllowed spec value
                      let newParsed = (specName spec, OptionValue validated) :: state.parsed
                      parseLoop opts specs ({ remaining := rest, parsed := newParsed } state)

            ShortOpt c =>
              case findByShort specs c of
                Nothing =>
                  if opts.allowUnknown
                    then parseLoop opts specs ({ remaining := rest } state)
                    else Err (UnknownOption ("-" ++ singleton c))
                Just spec =>
                  if spec.argType == Flag
                    then let newParsed = (specName spec, FlagSet) :: state.parsed
                         in parseLoop opts specs ({ remaining := rest, parsed := newParsed } state)
                    else case rest of
                      [] => Err (MissingValue ("-" ++ singleton c))
                      (val :: rest') => do
                        validated <- validateAllowed spec val
                        let newParsed = (specName spec, OptionValue validated) :: state.parsed
                        parseLoop opts specs ({ remaining := rest', parsed := newParsed } state)

            ShortOptVal c value =>
              case findByShort specs c of
                Nothing =>
                  if opts.allowUnknown
                    then parseLoop opts specs ({ remaining := rest } state)
                    else Err (UnknownOption ("-" ++ singleton c))
                Just spec =>
                  if spec.argType == Flag
                    then Err (InvalidFormat arg "flags don't take values")
                    else do
                      validated <- validateAllowed spec value
                      let newParsed = (specName spec, OptionValue validated) :: state.parsed
                      parseLoop opts specs ({ remaining := rest, parsed := newParsed } state)

            BundledOpts chars =>
              parseBundled opts specs state rest chars

            PositionalArg value =>
              if opts.stopAtNonOption
                then let newParsed = ("--rest", RestValues (arg :: rest)) :: state.parsed
                     in Ok ({ remaining := [], parsed := newParsed } state)
                else let posSpecs = filter (\s => s.argType == Positional) specs
                     in if state.positionalCount >= length posSpecs
                          then if opts.allowUnknown
                                 then parseLoop opts specs ({ remaining := rest } state)
                                 else Err (TooManyPositional (S state.positionalCount) (length posSpecs))
                          else let newParsed = ("positional-" ++ show state.positionalCount, PositionalValue value) :: state.parsed
                               in parseLoop opts specs ({ remaining := rest
                                                        , parsed := newParsed
                                                        , positionalCount := S state.positionalCount } state)

    parseBundled : ParserOptions -> List ArgSpec -> ParserState -> List String -> List Char -> ArgResult ParserState
    parseBundled opts specs state rest [] = parseLoop opts specs ({ remaining := rest } state)
    parseBundled opts specs state rest (c :: cs) =
      case findByShort specs c of
        Nothing =>
          if opts.allowUnknown
            then parseBundled opts specs state rest cs
            else Err (UnknownOption ("-" ++ singleton c))
        Just spec =>
          if spec.argType /= Flag
            then Err (InvalidFormat ("-" ++ pack (c :: cs)) "only flags can be bundled")
            else let newParsed = (specName spec, FlagSet) :: state.parsed
                 in parseBundled opts specs ({ parsed := newParsed } state) rest cs

    checkRequired : List ArgSpec -> ParsedArgs -> ArgResult ()
    checkRequired [] _ = Ok ()
    checkRequired (spec :: specs) parsed =
      if spec.required && isNothing (lookup (specName spec) parsed)
        then case spec.defaultValue of
               Just _ => checkRequired specs parsed
               Nothing => Err (MissingRequired spec)
        else checkRequired specs parsed

--------------------------------------------------------------------------------
-- Result Access
--------------------------------------------------------------------------------

||| Get flag value
export
getFlag : ParsedArgs -> String -> Bool
getFlag args name =
  case lookup name args of
    Just FlagSet => True
    _ => False

||| Get option value
export
getOption : ParsedArgs -> String -> Maybe String
getOption args name =
  case lookup name args of
    Just (OptionValue v) => Just v
    _ => Nothing

||| Get option with default
export
getOptionOr : ParsedArgs -> String -> String -> String
getOptionOr args name def =
  case lookup name args of
    Just (OptionValue v) => v
    _ => def

||| Get positional by index
export
getPositional : ParsedArgs -> Nat -> Maybe String
getPositional args idx =
  case lookup ("positional-" ++ show idx) args of
    Just (PositionalValue v) => Just v
    _ => Nothing

||| Get rest arguments
export
getRest : ParsedArgs -> List String
getRest args =
  case lookup "--rest" args of
    Just (RestValues vs) => vs
    _ => []

||| Get typed option
export
getTypedOption : ParsedArgs -> String -> (String -> Maybe a) -> ArgResult (Maybe a)
getTypedOption args name parser =
  case lookup name args of
    Just (OptionValue v) =>
      case parser v of
        Just x => Ok (Just x)
        Nothing => Err (TypeConversionFailed name v "requested type")
    _ => Ok Nothing

--------------------------------------------------------------------------------
-- Help Generation
--------------------------------------------------------------------------------

||| Generate usage line
export
generateUsage : String -> List ArgSpec -> String
generateUsage progName specs =
  progName ++ " " ++ unwords (map specUsage specs)

||| Generate help text
export
generateHelp : String -> String -> List ArgSpec -> String
generateHelp progName description specs =
  unlines
    [ description
    , ""
    , "Usage: " ++ generateUsage progName specs
    , ""
    , "Options:"
    , unlines (map specHelp specs)
    ]
