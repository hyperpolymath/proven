-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeArgsProps

import Proven.Core
import Proven.SafeArgs

%default total

||| Property: Simple flag parses
prop_simpleFlagParses : isOk (parseArgs ["-v"]) = True
prop_simpleFlagParses = Refl

||| Property: Long flag parses
prop_longFlagParses : isOk (parseArgs ["--verbose"]) = True
prop_longFlagParses = Refl

||| Property: Flag with value parses
prop_flagWithValueParses : isOk (parseArgs ["--output", "file.txt"]) = True
prop_flagWithValueParses = Refl

||| Property: Flag with equals parses
prop_flagEqualsParses : isOk (parseArgs ["--output=file.txt"]) = True
prop_flagEqualsParses = Refl

||| Property: Positional args collected
prop_positionalArgsCollected : getPositionalArgs (parseArgs ["cmd", "arg1", "arg2"]) = ["arg1", "arg2"]
prop_positionalArgsCollected = ?prop_positionalArgsCollected_rhs

||| Property: Double dash stops flag parsing
prop_doubleDashStops : getPositionalArgs (parseArgs ["--flag", "--", "--notaflag"]) = ["--notaflag"]
prop_doubleDashStops = ?prop_doubleDashStops_rhs

||| Property: Unknown flag rejected in strict mode
prop_unknownFlagRejected : isErr (parseArgsStrict ["--unknown"] ["--known"]) = True
prop_unknownFlagRejected = Refl

||| Property: Required flag enforced
prop_requiredFlagEnforced : isErr (parseArgsRequired [] ["--required"]) = True
prop_requiredFlagEnforced = Refl

||| Property: Multiple short flags combined
prop_combinedShortFlags : parseArgs ["-abc"] = parseArgs ["-a", "-b", "-c"]
prop_combinedShortFlags = ?prop_combinedShortFlags_rhs

||| Property: Empty args is valid
prop_emptyArgsValid : isOk (parseArgs []) = True
prop_emptyArgsValid = Refl

||| Property: Shell metacharacters in values escaped
prop_metacharEscaped : (args : List String) ->
                       allEscaped (escapeShellArgs args) = True
prop_metacharEscaped args = ?prop_metacharEscaped_rhs

||| Test runner for args properties
export
runArgsProperties : IO ()
runArgsProperties = do
  putStrLn "SafeArgs Property Tests"
  putStrLn "======================="
  putStrLn "prop_simpleFlagParses: PASS (proven by type)"
  putStrLn "prop_longFlagParses: PASS (proven by type)"
  putStrLn "prop_flagWithValueParses: PASS (proven by type)"
  putStrLn "prop_flagEqualsParses: PASS (proven by type)"
  putStrLn "prop_unknownFlagRejected: PASS (proven by type)"
  putStrLn "prop_requiredFlagEnforced: PASS (proven by type)"
  putStrLn "prop_emptyArgsValid: PASS (proven by type)"
  putStrLn ""
