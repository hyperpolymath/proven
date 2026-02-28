-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-cli: A CLI framework that cannot crash on malformed arguments.
--
-- Architecture:
--   - ArgType: Argument types (String, Int, Bool, Float, Path, Enum) with parsers
--   - Option: Option definitions with short/long names, validation
--   - Command: Command tree with subcommands, depth limit
--   - Parser: Tokeniser and argument matcher, required option checking
--   - Help: Formatted help text generation with aligned columns
--
-- This module defines the core CLI constants and re-exports submodules.

module CLI

import public CLI.ArgType
import public CLI.Option
import public CLI.Command
import public CLI.Parser
import public CLI.Help

||| Maximum argument string length.
||| Arguments exceeding this length are rejected to prevent
||| resource exhaustion.
public export
cliMaxArgLength : Nat
cliMaxArgLength = 4096

||| Maximum number of arguments.
||| Argument lists exceeding this count are rejected.
public export
cliMaxArgs : Nat
cliMaxArgs = 256

||| Maximum subcommand nesting depth.
||| Prevents pathologically deep command trees.
public export
cliMaxSubcommandDepth : Nat
cliMaxSubcommandDepth = 5
