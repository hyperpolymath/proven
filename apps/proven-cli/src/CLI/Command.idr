-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- CLI Command Definitions
--
-- Defines the structure of CLI commands with options, descriptions,
-- and nested subcommands.  The tree structure has a configurable
-- depth limit to prevent pathological nesting.

module CLI.Command

import CLI.Option
import CLI.ArgType

%default total

-- ============================================================================
-- Command Record
-- ============================================================================

||| A CLI command with its name, description, options, and subcommands.
||| Commands can be nested to arbitrary depth (up to `maxSubcommandDepth`).
public export
record CLICommand where
  constructor MkCommand
  ||| Command name (used on the command line, e.g., "deploy")
  name        : String
  ||| Human-readable description shown in help text
  description : String
  ||| Options accepted by this command
  options     : List CLIOption
  ||| Nested subcommands (e.g., "git remote add")
  subcommands : List CLICommand
  ||| Positional argument names (in order)
  positionals : List String
  ||| Version string (only meaningful for the root command)
  version     : Maybe String

public export
Show CLICommand where
  show cmd = cmd.name ++ ": " ++ cmd.description
             ++ " (" ++ show (length cmd.options) ++ " options, "
             ++ show (length cmd.subcommands) ++ " subcommands)"

-- ============================================================================
-- Command Builders
-- ============================================================================

||| Create a simple command with no options or subcommands.
public export
simpleCommand : String -> String -> CLICommand
simpleCommand name desc = MkCommand
  { name        = name
  , description = desc
  , options     = []
  , subcommands = []
  , positionals = []
  , version     = Nothing
  }

||| Create a root command with version information.
public export
rootCommand : String -> String -> String -> CLICommand
rootCommand name desc ver = MkCommand
  { name        = name
  , description = desc
  , options     = []
  , subcommands = []
  , positionals = []
  , version     = Just ver
  }

||| Add an option to a command.
public export
withOption : CLIOption -> CLICommand -> CLICommand
withOption opt cmd = { options $= (opt ::) } cmd

||| Add multiple options to a command.
public export
withOptions : List CLIOption -> CLICommand -> CLICommand
withOptions opts cmd = { options := opts ++ cmd.options } cmd

||| Add a subcommand to a command.
public export
withSubcommand : CLICommand -> CLICommand -> CLICommand
withSubcommand sub parent = { subcommands $= (sub ::) } parent

||| Add positional arguments to a command.
public export
withPositionals : List String -> CLICommand -> CLICommand
withPositionals args cmd = { positionals := args } cmd

-- ============================================================================
-- Command Validation
-- ============================================================================

||| Maximum allowed subcommand nesting depth.
public export
maxSubcommandDepth : Nat
maxSubcommandDepth = 5

||| Errors detected in command definitions.
public export
data CommandDefError : Type where
  ||| Command name is empty
  EmptyCommandName    : CommandDefError
  ||| Command name contains spaces
  CommandNameHasSpaces : (name : String) -> CommandDefError
  ||| Subcommand depth exceeds the limit
  SubcommandTooDeep   : (depth : Nat) -> CommandDefError
  ||| Duplicate subcommand names at the same level
  DuplicateSubcommand : (name : String) -> CommandDefError
  ||| Option definition errors within a command
  OptionErrors        : (cmdName : String) -> List OptionDefError -> CommandDefError

public export
Show CommandDefError where
  show EmptyCommandName          = "Command name must not be empty"
  show (CommandNameHasSpaces n)  = "Command name contains spaces: '" ++ n ++ "'"
  show (SubcommandTooDeep d)     = "Subcommand depth " ++ show d
                                   ++ " exceeds limit " ++ show maxSubcommandDepth
  show (DuplicateSubcommand n)   = "Duplicate subcommand: " ++ n
  show (OptionErrors cmd errs)   = "Option errors in '" ++ cmd ++ "': " ++ show (length errs)

||| Check for duplicate subcommand names.
public export
findDuplicateSubcommand : List CLICommand -> Maybe String
findDuplicateSubcommand cmds =
  let names = map (.name) cmds
  in findDup names
  where
    findDup : List String -> Maybe String
    findDup [] = Nothing
    findDup (n :: ns) = if any (== n) ns then Just n else findDup ns

||| Validate a command tree, checking names, options, and depth.
||| The depth parameter tracks the current nesting level.
public export
validateCommand : (depth : Nat) -> CLICommand -> List CommandDefError
validateCommand depth cmd =
  let nameErrors = if cmd.name == ""
                     then [EmptyCommandName]
                     else if any (== ' ') (unpack cmd.name)
                       then [CommandNameHasSpaces cmd.name]
                       else []
      depthErrors = if depth > maxSubcommandDepth
                      then [SubcommandTooDeep depth]
                      else []
      dupSub = maybe [] (\n => [DuplicateSubcommand n])
                     (findDuplicateSubcommand cmd.subcommands)
      optErrs = let errs = validateOptions cmd.options
                in if null errs then [] else [OptionErrors cmd.name errs]
      -- Recursively validate subcommands
      subErrors = concatMap (validateCommand (S depth)) cmd.subcommands
  in nameErrors ++ depthErrors ++ dupSub ++ optErrs ++ subErrors

||| Find a subcommand by name.
public export
findSubcommand : String -> CLICommand -> Maybe CLICommand
findSubcommand name cmd = find (\sub => sub.name == name) cmd.subcommands

||| Get the full command path (e.g., ["git", "remote", "add"]).
||| Takes the breadcrumb trail of parent command names.
public export
commandPath : List String -> CLICommand -> List String
commandPath parents cmd = parents ++ [cmd.name]
