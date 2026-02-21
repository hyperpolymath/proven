-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Fluent builder for constructing safe shell commands
|||
||| Provides a DSL for building commands with compile-time safety
||| and runtime validation of arguments
module Proven.SafeCommand.Builder

import Proven.Core
import Proven.SafeCommand.Escape
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Builder Types
--------------------------------------------------------------------------------

||| Argument type classification
public export
data ArgType : Type where
  Positional : ArgType      -- Regular positional argument
  ShortOpt : ArgType        -- Short option (-x)
  LongOpt : ArgType         -- Long option (--foo)
  OptWithValue : ArgType    -- Option with value (--foo=bar or -x bar)

||| Builder argument with metadata
public export
record BuilderArg where
  constructor MkBuilderArg
  argType : ArgType
  argValue : String
  argMeta : Maybe String    -- Optional metadata (e.g., description)

||| Command builder state
public export
record CommandBuilder where
  constructor MkCommandBuilder
  builderName : String
  builderArgs : List BuilderArg
  builderEnv : List (String, String)
  builderWorkDir : Maybe String
  builderStdin : Maybe String
  builderTimeout : Maybe Nat

--------------------------------------------------------------------------------
-- Builder Creation
--------------------------------------------------------------------------------

||| Start building a command
public export
command : String -> CommandBuilder
command name = MkCommandBuilder name [] [] Nothing Nothing Nothing

||| Common command builders
public export
lsCmd : CommandBuilder
lsCmd = command "ls"

public export
cpCmd : CommandBuilder
cpCmd = command "cp"

public export
mvCmd : CommandBuilder
mvCmd = command "mv"

public export
rmCmd : CommandBuilder
rmCmd = command "rm"

public export
catCmd : CommandBuilder
catCmd = command "cat"

public export
grepCmd : CommandBuilder
grepCmd = command "grep"

public export
findCmd : CommandBuilder
findCmd = command "find"

public export
gitCmd : CommandBuilder
gitCmd = command "git"

public export
curlCmd : CommandBuilder
curlCmd = command "curl"

public export
wgetCmd : CommandBuilder
wgetCmd = command "wget"

public export
tarCmd : CommandBuilder
tarCmd = command "tar"

public export
gzipCmd : CommandBuilder
gzipCmd = command "gzip"

public export
awkCmd : CommandBuilder
awkCmd = command "awk"

public export
sedCmd : CommandBuilder
sedCmd = command "sed"

public export
sortCmd : CommandBuilder
sortCmd = command "sort"

public export
uniqCmd : CommandBuilder
uniqCmd = command "uniq"

public export
wcCmd : CommandBuilder
wcCmd = command "wc"

public export
headCmd : CommandBuilder
headCmd = command "head"

public export
tailCmd : CommandBuilder
tailCmd = command "tail"

--------------------------------------------------------------------------------
-- Argument Methods
--------------------------------------------------------------------------------

||| Add positional argument
public export
addArg : String -> CommandBuilder -> CommandBuilder
addArg val b = { builderArgs := b.builderArgs ++ [MkBuilderArg Positional val Nothing] } b

||| Add multiple positional arguments
public export
addArgs : List String -> CommandBuilder -> CommandBuilder
addArgs vals b = foldl (flip addArg) b vals

||| Add short flag (-x)
public export
addShort : Char -> CommandBuilder -> CommandBuilder
addShort c b =
  let flag = MkBuilderArg ShortOpt ("-" ++ singleton c) Nothing
  in { builderArgs := b.builderArgs ++ [flag] } b

||| Add long flag (--verbose)
public export
addLong : String -> CommandBuilder -> CommandBuilder
addLong name b =
  let flag = MkBuilderArg LongOpt ("--" ++ name) Nothing
  in { builderArgs := b.builderArgs ++ [flag] } b

||| Add short option with value (-o value)
public export
addShortOpt : Char -> String -> CommandBuilder -> CommandBuilder
addShortOpt c val b =
  let opt = MkBuilderArg ShortOpt ("-" ++ singleton c) Nothing
      arg = MkBuilderArg Positional val Nothing
  in { builderArgs := b.builderArgs ++ [opt, arg] } b

||| Add long option with value (--output=value or --output value)
public export
addLongOpt : String -> String -> CommandBuilder -> CommandBuilder
addLongOpt name val b =
  let opt = MkBuilderArg OptWithValue ("--" ++ name ++ "=" ++ val) Nothing
  in { builderArgs := b.builderArgs ++ [opt] } b

||| Add long option with separate value
public export
addLongOptSep : String -> String -> CommandBuilder -> CommandBuilder
addLongOptSep name val b =
  let opt = MkBuilderArg LongOpt ("--" ++ name) Nothing
      arg = MkBuilderArg Positional val Nothing
  in { builderArgs := b.builderArgs ++ [opt, arg] } b

--------------------------------------------------------------------------------
-- Environment and Context
--------------------------------------------------------------------------------

||| Set environment variable
public export
setEnv : String -> String -> CommandBuilder -> CommandBuilder
setEnv name val b = { builderEnv := (name, val) :: b.builderEnv } b

||| Set multiple environment variables
public export
setEnvs : List (String, String) -> CommandBuilder -> CommandBuilder
setEnvs vars b = foldl (\acc, (n,v) => setEnv n v acc) b vars

||| Set working directory
public export
setWorkDir : String -> CommandBuilder -> CommandBuilder
setWorkDir dir b = { builderWorkDir := Just dir } b

||| Set stdin content
public export
setStdin : String -> CommandBuilder -> CommandBuilder
setStdin content b = { builderStdin := Just content } b

||| Set timeout in seconds
public export
setTimeout : Nat -> CommandBuilder -> CommandBuilder
setTimeout secs b = { builderTimeout := Just secs } b

--------------------------------------------------------------------------------
-- Common Option Patterns
--------------------------------------------------------------------------------

||| Add verbose flag (-v or --verbose)
public export
verbose : CommandBuilder -> CommandBuilder
verbose = addShort 'v'

||| Add verbose long flag
public export
verboseLong : CommandBuilder -> CommandBuilder
verboseLong = addLong "verbose"

||| Add recursive flag (-r or -R)
public export
recursive : CommandBuilder -> CommandBuilder
recursive = addShort 'r'

||| Add recursive long flag
public export
recursiveLong : CommandBuilder -> CommandBuilder
recursiveLong = addLong "recursive"

||| Add force flag (-f)
public export
force : CommandBuilder -> CommandBuilder
force = addShort 'f'

||| Add quiet flag (-q)
public export
quiet : CommandBuilder -> CommandBuilder
quiet = addShort 'q'

||| Add help flag (--help)
public export
help : CommandBuilder -> CommandBuilder
help = addLong "help"

||| Add version flag (--version)
public export
version : CommandBuilder -> CommandBuilder
version = addLong "version"

||| Add output file option (-o file)
public export
output : String -> CommandBuilder -> CommandBuilder
output = addShortOpt 'o'

||| Add output file long option (--output=file)
public export
outputLong : String -> CommandBuilder -> CommandBuilder
outputLong = addLongOpt "output"

||| Add input file option (-i file)
public export
input : String -> CommandBuilder -> CommandBuilder
input = addShortOpt 'i'

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render argument to escaped string
renderBuilderArg : BuilderArg -> String
renderBuilderArg (MkBuilderArg Positional val _) = escapeShellArg val
renderBuilderArg (MkBuilderArg ShortOpt val _) = val  -- Flags not escaped
renderBuilderArg (MkBuilderArg LongOpt val _) = val   -- Flags not escaped
renderBuilderArg (MkBuilderArg OptWithValue val _) =
  -- Split on = and escape only the value part
  case break (== '=') (unpack val) of
    (name, '=' :: value) => pack name ++ "=" ++ escapeShellArg (pack value)
    _ => val

||| Build the command string
public export
build : CommandBuilder -> String
build b =
  let args = map renderBuilderArg b.builderArgs
  in unwords (b.builderName :: args)

||| Build with environment variables prefixed
public export
buildWithEnv : CommandBuilder -> String
buildWithEnv b =
  let envPart = unwords (map renderEnv b.builderEnv)
      cmdPart = build b
  in if null b.builderEnv
       then cmdPart
       else envPart ++ " " ++ cmdPart
  where
    renderEnv : (String, String) -> String
    renderEnv (name, val) = name ++ "=" ++ escapeShellArg val

||| Build as argument list
public export
buildArgList : CommandBuilder -> List String
buildArgList b = b.builderName :: map renderBuilderArg b.builderArgs

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if builder has any positional arguments
public export
hasPositionalArgs : CommandBuilder -> Bool
hasPositionalArgs b = any isPositional b.builderArgs
  where
    isPositional : BuilderArg -> Bool
    isPositional (MkBuilderArg Positional _ _) = True
    isPositional _ = False

||| Count positional arguments
public export
countPositionalArgs : CommandBuilder -> Nat
countPositionalArgs b = length (filter isPositional b.builderArgs)
  where
    isPositional : BuilderArg -> Bool
    isPositional (MkBuilderArg Positional _ _) = True
    isPositional _ = False

||| Check if command looks safe (no injection patterns)
public export
looksafe : CommandBuilder -> Bool
looksafe b =
  not (containsDangerousPattern b.builderName) &&
  all (\a => not (containsDangerousPattern a.argValue)) b.builderArgs

--------------------------------------------------------------------------------
-- Fluent Interface Operators
--------------------------------------------------------------------------------

infixl 4 |>

||| Pipe operator for fluent building
public export
(|>) : CommandBuilder -> (CommandBuilder -> CommandBuilder) -> CommandBuilder
(|>) b f = f b

--------------------------------------------------------------------------------
-- Example Builders
--------------------------------------------------------------------------------

||| Build a git clone command
public export
gitClone : String -> String -> CommandBuilder
gitClone url dest = gitCmd
  |> addArg "clone"
  |> addArg url
  |> addArg dest

||| Build a git commit command
public export
gitCommit : String -> CommandBuilder
gitCommit msg = gitCmd
  |> addArg "commit"
  |> addShortOpt 'm' msg

||| Build a curl download command
public export
curlDownload : String -> String -> CommandBuilder
curlDownload url file = curlCmd
  |> addShort 'L'  -- Follow redirects
  |> addShortOpt 'o' file
  |> addArg url

||| Build a tar extract command
public export
tarExtract : String -> String -> CommandBuilder
tarExtract archive dest = tarCmd
  |> addShort 'x'
  |> addShort 'f'
  |> addArg archive
  |> addShortOpt 'C' dest

||| Build a grep search command
public export
grepSearch : String -> List String -> CommandBuilder
grepSearch pattern files = grepCmd
  |> addArg pattern
  |> addArgs files

||| Build a find command
public export
findByName : String -> String -> CommandBuilder
findByName path pattern = findCmd
  |> addArg path
  |> addLongOptSep "name" pattern
