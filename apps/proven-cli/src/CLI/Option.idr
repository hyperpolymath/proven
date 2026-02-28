-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- CLI Option Definitions
--
-- Defines the structure of command-line options (short name, long name,
-- description, type, required/optional, default value).  Includes
-- validation to detect duplicate short/long names at definition time.

module CLI.Option

import CLI.ArgType

%default total

-- ============================================================================
-- Option Record
-- ============================================================================

||| A single command-line option definition.
||| Options are matched by short name (-v) or long name (--verbose).
public export
record CLIOption where
  constructor MkOption
  ||| Short option name (e.g., Just 'v' for -v). Nothing if no short form.
  shortName   : Maybe Char
  ||| Long option name (e.g., "verbose" for --verbose). Must be non-empty.
  longName    : String
  ||| Human-readable description shown in help text.
  description : String
  ||| The type of value this option accepts.
  argType     : ArgType
  ||| Whether this option must be provided by the user.
  required    : Bool
  ||| Default value string (if not required). Parsed using argType.
  defaultVal  : Maybe String

public export
Show CLIOption where
  show opt = let short = maybe "" (\c => "-" ++ singleton c ++ ", ") opt.shortName
             in short ++ "--" ++ opt.longName
                ++ " " ++ show opt.argType
                ++ (if opt.required then " (required)" else "")
                ++ maybe "" (\d => " [default: " ++ d ++ "]") opt.defaultVal

-- ============================================================================
-- Option Builders
-- ============================================================================

||| Create a required string option with a short and long name.
public export
requiredString : Char -> String -> String -> CLIOption
requiredString short long desc = MkOption
  { shortName   = Just short
  , longName    = long
  , description = desc
  , argType     = StringArg
  , required    = True
  , defaultVal  = Nothing
  }

||| Create an optional boolean flag (defaults to "false").
public export
flag : Char -> String -> String -> CLIOption
flag short long desc = MkOption
  { shortName   = Just short
  , longName    = long
  , description = desc
  , argType     = BoolArg
  , required    = False
  , defaultVal  = Just "false"
  }

||| Create a required integer option.
public export
requiredInt : Char -> String -> String -> CLIOption
requiredInt short long desc = MkOption
  { shortName   = Just short
  , longName    = long
  , description = desc
  , argType     = IntArg
  , required    = True
  , defaultVal  = Nothing
  }

||| Create an optional string option with a default value.
public export
optionalString : Char -> String -> String -> String -> CLIOption
optionalString short long desc def = MkOption
  { shortName   = Just short
  , longName    = long
  , description = desc
  , argType     = StringArg
  , required    = False
  , defaultVal  = Just def
  }

||| Create an enum option (must be one of the listed values).
public export
enumOption : Char -> String -> String -> List String -> CLIOption
enumOption short long desc values = MkOption
  { shortName   = Just short
  , longName    = long
  , description = desc
  , argType     = EnumArg values
  , required    = True
  , defaultVal  = Nothing
  }

-- ============================================================================
-- Option Validation
-- ============================================================================

||| Errors detected in option definitions (before any parsing).
public export
data OptionDefError : Type where
  ||| Two options share the same short name
  DuplicateShortName : (char : Char) -> OptionDefError
  ||| Two options share the same long name
  DuplicateLongName  : (name : String) -> OptionDefError
  ||| Long name is empty
  EmptyLongName      : OptionDefError
  ||| Required option has a default value (contradictory)
  RequiredWithDefault : (name : String) -> OptionDefError

public export
Show OptionDefError where
  show (DuplicateShortName c)   = "Duplicate short option: -" ++ singleton c
  show (DuplicateLongName n)    = "Duplicate long option: --" ++ n
  show EmptyLongName            = "Option long name must not be empty"
  show (RequiredWithDefault n)  = "Required option --" ++ n ++ " has a default value"

||| Check for duplicate short names in a list of options.
public export
findDuplicateShort : List CLIOption -> Maybe Char
findDuplicateShort opts =
  let shorts = mapMaybe (.shortName) opts
  in findDup shorts
  where
    findDup : List Char -> Maybe Char
    findDup [] = Nothing
    findDup (c :: cs) = if any (== c) cs then Just c else findDup cs

||| Check for duplicate long names in a list of options.
public export
findDuplicateLong : List CLIOption -> Maybe String
findDuplicateLong opts =
  let longs = map (.longName) opts
  in findDup longs
  where
    findDup : List String -> Maybe String
    findDup [] = Nothing
    findDup (n :: ns) = if any (== n) ns then Just n else findDup ns

||| Validate a list of option definitions.
||| Returns all errors found.
public export
validateOptions : List CLIOption -> List OptionDefError
validateOptions opts =
  let dupShort = maybe [] (\c => [DuplicateShortName c]) (findDuplicateShort opts)
      dupLong  = maybe [] (\n => [DuplicateLongName n]) (findDuplicateLong opts)
      empties  = if any (\o => o.longName == "") opts then [EmptyLongName] else []
      reqDef   = mapMaybe (\o => if o.required && isJust o.defaultVal
                                   then Just (RequiredWithDefault o.longName)
                                   else Nothing) opts
  in dupShort ++ dupLong ++ empties ++ reqDef

||| Find an option by its short name.
public export
findByShort : Char -> List CLIOption -> Maybe CLIOption
findByShort c = find (\o => o.shortName == Just c)

||| Find an option by its long name.
public export
findByLong : String -> List CLIOption -> Maybe CLIOption
findByLong n = find (\o => o.longName == n)
