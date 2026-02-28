-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- CLI Help Text Generation
--
-- Generates formatted help text for commands, including usage lines,
-- option tables with aligned columns, and subcommand lists.
-- All output is pure string generation — no IO side effects.

module CLI.Help

import CLI.ArgType
import CLI.Option
import CLI.Command

%default total

-- ============================================================================
-- Column Alignment
-- ============================================================================

||| Pad a string on the right to the given width with spaces.
public export
padRight : Nat -> String -> String
padRight width s =
  let len = length s
  in if len >= width
       then s
       else s ++ replicate (minus width len) ' '

||| Calculate the maximum option display width for column alignment.
public export
maxOptionWidth : List CLIOption -> Nat
maxOptionWidth [] = 0
maxOptionWidth (opt :: rest) =
  let thisWidth = optionDisplayWidth opt
      restWidth = maxOptionWidth rest
  in if thisWidth > restWidth then thisWidth else restWidth
  where
    optionDisplayWidth : CLIOption -> Nat
    optionDisplayWidth o =
      let shortPart = maybe 0 (\_ => 4) o.shortName  -- "-x, "
          longPart  = 2 + length o.longName            -- "--name"
          typePart  = 1 + length (show o.argType)      -- " TYPE"
      in shortPart + longPart + typePart

-- ============================================================================
-- Usage Line
-- ============================================================================

||| Generate the usage line for a command.
||| Format: "Usage: cmd [OPTIONS] [SUBCOMMAND] [ARGS...]"
public export
usageLine : List String -> CLICommand -> String
usageLine parents cmd =
  let path = unwords (parents ++ [cmd.name])
      opts = if null cmd.options then "" else " [OPTIONS]"
      subs = if null cmd.subcommands then "" else " <COMMAND>"
      args = if null cmd.positionals then ""
             else " " ++ unwords (map (\a => "<" ++ a ++ ">") cmd.positionals)
  in "Usage: " ++ path ++ opts ++ subs ++ args

-- ============================================================================
-- Option Formatting
-- ============================================================================

||| Format a single option for display in the help text.
||| Returns the left column (flags) and right column (description).
public export
formatOption : CLIOption -> (String, String)
formatOption opt =
  let shortPart = maybe "    " (\c => "-" ++ singleton c ++ ", ") opt.shortName
      longPart  = "--" ++ opt.longName
      typePart  = " " ++ show opt.argType
      reqPart   = if opt.required then " (required)" else ""
      defPart   = maybe "" (\d => " [default: " ++ d ++ "]") opt.defaultVal
      left      = shortPart ++ longPart ++ typePart
      right     = opt.description ++ reqPart ++ defPart
  in (left, right)

||| Format all options as aligned lines.
public export
formatOptions : List CLIOption -> List String
formatOptions opts =
  let formatted = map formatOption opts
      maxWidth  = foldl (\w, (l, _) => if length l > w then length l else w) 0 formatted
      colWidth  = maxWidth + 2  -- 2 spaces padding
  in map (\(l, r) => "  " ++ padRight colWidth l ++ r) formatted

-- ============================================================================
-- Subcommand Formatting
-- ============================================================================

||| Format a single subcommand for display.
public export
formatSubcommand : CLICommand -> (String, String)
formatSubcommand cmd = (cmd.name, cmd.description)

||| Format all subcommands as aligned lines.
public export
formatSubcommands : List CLICommand -> List String
formatSubcommands cmds =
  let formatted = map formatSubcommand cmds
      maxWidth  = foldl (\w, (l, _) => if length l > w then length l else w) 0 formatted
      colWidth  = maxWidth + 2
  in map (\(l, r) => "  " ++ padRight colWidth l ++ r) formatted

-- ============================================================================
-- Full Help Text
-- ============================================================================

||| Generate complete help text for a command.
||| Includes: description, usage, options section, subcommands section.
public export
generateHelp : List String -> CLICommand -> String
generateHelp parents cmd =
  let sections = [ [cmd.description, ""]
                 , [usageLine parents cmd, ""]
                 ]
      optSection = if null cmd.options then []
                   else ["Options:"] ++ formatOptions cmd.options ++ [""]
      subSection = if null cmd.subcommands then []
                   else ["Commands:"] ++ formatSubcommands cmd.subcommands ++ [""]
      verSection = case cmd.version of
                     Nothing => []
                     Just v  => ["Version: " ++ v, ""]
  in unlines (concat sections ++ optSection ++ subSection ++ verSection)

||| Generate a short error message with a hint to use --help.
public export
errorWithHint : List String -> CLICommand -> String -> String
errorWithHint parents cmd errMsg =
  let path = unwords (parents ++ [cmd.name])
  in "Error: " ++ errMsg ++ "\n"
     ++ "Try '" ++ path ++ " --help' for more information."

||| Generate version text.
public export
versionText : CLICommand -> String
versionText cmd = case cmd.version of
  Nothing => cmd.name ++ " (no version)"
  Just v  => cmd.name ++ " " ++ v
