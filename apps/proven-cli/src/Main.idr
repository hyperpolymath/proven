-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-cli: Main entry point
--
-- A CLI framework that cannot crash on malformed arguments.
-- Uses dependent types to validate argument types, enforce required
-- options, and prevent buffer overflows from excessively long inputs.
--
-- Usage:
--   proven-cli (runs the demo)

module Main

import CLI
import CLI.ArgType
import CLI.Option
import CLI.Command
import CLI.Parser
import CLI.Help
import System

%default total

-- ============================================================================
-- Demo: Define a CLI application
-- ============================================================================

||| Build a sample CLI application with options and subcommands,
||| similar to a deployment tool.
demoCLI : CLICommand
demoCLI =
  let deployCmd = withOptions
        [ requiredString 'e' "env" "Target environment"
        , requiredString 't' "tag" "Docker image tag"
        , flag 'd' "dry-run" "Simulate without deploying"
        , optionalString 'r' "region" "Cloud region" "us-east-1"
        ]
        (withPositionals ["service"]
          (simpleCommand "deploy" "Deploy a service to an environment"))

      statusCmd = withOptions
        [ optionalString 'e' "env" "Environment to check" "all"
        , flag 'j' "json" "Output as JSON"
        ]
        (simpleCommand "status" "Show deployment status")

      rollbackCmd = withOptions
        [ requiredString 'e' "env" "Target environment"
        , requiredInt 'n' "revision" "Revision number to roll back to"
        ]
        (simpleCommand "rollback" "Roll back to a previous revision")

      configCmd = withSubcommand
        (withOptions
          [ requiredString 'k' "key" "Configuration key"
          , requiredString 'v' "value" "Configuration value"
          ]
          (simpleCommand "set" "Set a configuration value"))
        (withSubcommand
          (withOptions
            [ requiredString 'k' "key" "Configuration key" ]
            (simpleCommand "get" "Get a configuration value"))
          (simpleCommand "config" "Manage configuration"))

      root = withOption (flag 'V' "verbose" "Enable verbose output")
             (withSubcommand deployCmd
               (withSubcommand statusCmd
                 (withSubcommand rollbackCmd
                   (withSubcommand configCmd
                     (rootCommand "proven-deploy" "Deployment tool" "1.0.0")))))
  in root

-- ============================================================================
-- Demo: Show help text
-- ============================================================================

||| Display the generated help text for the demo CLI.
covering
demoHelpText : IO ()
demoHelpText = do
  putStrLn "\n--- Generated Help Text ---\n"
  putStrLn (generateHelp [] demoCLI)

  -- Show subcommand help
  case findSubcommand "deploy" demoCLI of
    Nothing  => putStrLn "ERROR: deploy subcommand not found"
    Just sub => do
      putStrLn "--- Deploy Subcommand Help ---\n"
      putStrLn (generateHelp ["proven-deploy"] sub)

-- ============================================================================
-- Demo: Parse sample arguments
-- ============================================================================

||| Demonstrate argument tokenisation and option matching.
covering
demoParsing : IO ()
demoParsing = do
  putStrLn "--- Argument Parsing Demo ---\n"

  -- Tokenise sample arguments
  let sampleArgs = ["deploy", "--env", "production", "--tag", "v2.3.1",
                     "--dry-run", "--region", "eu-west-1", "web-api"]
  let tokens = tokeniseAll sampleArgs
  putStrLn "  Input args:"
  putStrLn $ "    " ++ show sampleArgs
  putStrLn "  Tokens:"
  traverse_ (\t => putStrLn $ "    " ++ show t) tokens

  -- Demonstrate typed argument parsing
  putStrLn "\n  Argument type parsing:"
  let tests = [ ("42",          IntArg)
              , ("not-a-number", IntArg)
              , ("true",         BoolArg)
              , ("maybe",        BoolArg)
              , ("3.14",         FloatArg)
              , ("/usr/bin",     PathArg)
              , ("",             PathArg)
              , ("prod",         EnumArg ["dev", "staging", "prod"])
              , ("test",         EnumArg ["dev", "staging", "prod"])
              ]
  traverse_ (\(raw, ty) =>
    putStrLn $ "    parseTypedArg " ++ show ty ++ " " ++ show raw
               ++ " = " ++ show (parseTypedArg ty raw)
    ) tests

-- ============================================================================
-- Demo: Validate command definitions
-- ============================================================================

||| Demonstrate command definition validation.
covering
demoValidation : IO ()
demoValidation = do
  putStrLn "\n--- Command Validation Demo ---\n"

  -- Valid command
  let errs1 = validateCommand 0 demoCLI
  putStrLn $ "  Demo CLI errors: " ++ show (length errs1)

  -- Invalid: empty name, duplicate options
  let badCmd = withOptions
        [ requiredString 'v' "verbose" "Be verbose"
        , flag 'v' "verbose" "Also be verbose"  -- duplicate!
        ]
        (simpleCommand "" "A command with no name")
  let errs2 = validateCommand 0 badCmd
  putStrLn $ "  Bad command errors: " ++ show (length errs2)
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs2

  -- Version text
  putStrLn $ "\n  Version: " ++ versionText demoCLI

-- ============================================================================
-- Demo: Required option checking
-- ============================================================================

||| Demonstrate required option validation.
covering
demoRequired : IO ()
demoRequired = do
  putStrLn "\n--- Required Option Check Demo ---\n"

  case findSubcommand "deploy" demoCLI of
    Nothing  => putStrLn "ERROR: deploy not found"
    Just sub => do
      -- No options provided -> should flag required options
      let missing = checkRequired sub []
      putStrLn $ "  Missing required (no args): " ++ show (length missing)
      traverse_ (\e => putStrLn $ "    - " ++ show e) missing

      -- Provide env but not tag
      let partial = [MkParsedOption (requiredString 'e' "env" "env") (StrVal "prod")]
      let missing2 = checkRequired sub partial
      putStrLn $ "\n  Missing required (env only): " ++ show (length missing2)
      traverse_ (\e => putStrLn $ "    - " ++ show e) missing2

      -- Provide both required options
      let full = partial ++ [MkParsedOption (requiredString 't' "tag" "tag") (StrVal "v1.0")]
      let missing3 = checkRequired sub full
      putStrLn $ "\n  Missing required (env+tag): " ++ show (length missing3)

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-cli v0.1.0 — CLI framework that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"

  -- Run demos
  demoHelpText
  demoParsing
  demoValidation
  demoRequired

  putStrLn (replicate 60 '-')
  putStrLn "All argument parsing proven safe at compile time"
  putStrLn "Build with: idris2 --build proven-cli.ipkg"
  putStrLn "Run with:   ./build/exec/proven-cli"
