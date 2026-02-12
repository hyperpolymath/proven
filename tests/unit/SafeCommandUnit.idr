-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCommandUnit

import Proven.Core
import Proven.SafeCommand

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runCommandUnitTests : IO ()
runCommandUnitTests = do
  putStrLn "SafeCommand Unit Tests"
  putStrLn "======================"

  -- Command name validation tests
  putStrLn "\n[Command Name Validation]"
  assertOk "validateCommand \"ls\" valid" (validateCommand "ls")
  assertOk "validateCommand \"git\" valid" (validateCommand "git")
  assertOk "validateCommand \"/usr/bin/ls\" valid" (validateCommand "/usr/bin/ls")
  assertErr "validateCommand \"; rm -rf /\" blocked" (validateCommand "; rm -rf /")
  assertErr "validateCommand \"ls && rm\" blocked" (validateCommand "ls && rm")
  assertErr "validateCommand \"\" blocked" (validateCommand "")

  -- Argument escaping tests (POSIX)
  putStrLn "\n[POSIX Escaping]"
  assertEq "escapeArg POSIX \"hello\" = \"hello\""
           "hello" (escapeArg POSIX "hello")
  assertEq "escapeArg POSIX \"hello world\" quotes"
           "'hello world'" (escapeArg POSIX "hello world")
  assertEq "escapeArg POSIX \"it's\" escapes quote"
           "'it'\\''s'" (escapeArg POSIX "it's")
  assertEq "escapeArg POSIX \"a;b\" quotes"
           "'a;b'" (escapeArg POSIX "a;b")
  assertEq "escapeArg POSIX \"$(cmd)\" quotes"
           "'$(cmd)'" (escapeArg POSIX "$(cmd)")

  -- Argument escaping tests (Windows)
  putStrLn "\n[Windows Escaping]"
  assertEq "escapeArg Windows \"hello\" = \"hello\""
           "hello" (escapeArg Windows "hello")
  assertEq "escapeArg Windows \"hello world\" quotes"
           "\"hello world\"" (escapeArg Windows "hello world")

  -- Command builder tests
  putStrLn "\n[Command Builder]"
  let lsCmd = command "ls"
              |> withFlag "-la"
              |> withArg "/home"
              |> build POSIX
  assertOk "ls -la /home builds" lsCmd

  let gitCmd = command "git"
               |> withFlag "--no-pager"
               |> withArg "log"
               |> withOption "--oneline"
               |> withArg "-5"
               |> build POSIX
  assertOk "git command builds" gitCmd

  -- Dangerous pattern detection tests
  putStrLn "\n[Dangerous Pattern Detection]"
  assertTrue "isDangerous \"; rm -rf /\" = True"
             (isDangerous "; rm -rf /")
  assertTrue "isDangerous \"$(rm -rf /)\" = True"
             (isDangerous "$(rm -rf /)")
  assertTrue "isDangerous \"`rm -rf /`\" = True"
             (isDangerous "`rm -rf /`")
  assertTrue "isDangerous \"| cat /etc/passwd\" = True"
             (isDangerous "| cat /etc/passwd")
  assertTrue "isDangerous \"safe argument\" = False"
             (not $ isDangerous "safe argument")

  -- Pipeline representation tests
  putStrLn "\n[Pipeline Representation]"
  let pipeline = command "cat"
                 |> withArg "file.txt"
                 |> pipeTo (command "grep" |> withArg "pattern")
                 |> pipeTo (command "wc" |> withFlag "-l")
  assertOk "pipeline builds safely" (buildPipeline POSIX pipeline)

  -- Redirect representation tests
  putStrLn "\n[Redirect Representation]"
  let withRedirect = command "echo"
                     |> withArg "hello"
                     |> redirectTo "output.txt"
  assertOk "redirect builds" (buildWithRedirect POSIX withRedirect)

  -- Common command helpers tests
  putStrLn "\n[Common Commands]"
  assertOk "safeLs \"/home\" builds" (safeLs "/home")
  assertOk "safeCp \"src\" \"dst\" builds" (safeCp "src" "dst")
  assertOk "safeGit [\"status\"] builds" (safeGit ["status"])
  assertErr "safeCd \"../../../etc\" blocked" (safeCd "../../../etc")

  -- Environment variable escaping tests
  putStrLn "\n[Environment Variables]"
  assertEq "escapeEnvValue \"safe\" unchanged"
           "safe" (escapeEnvValue "safe")
  assertEq "escapeEnvValue with spaces quotes"
           "'with spaces'" (escapeEnvValue "with spaces")

  putStrLn "\n✓ SafeCommand unit tests complete"
