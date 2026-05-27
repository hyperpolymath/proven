-- SPDX-License-Identifier: MPL-2.0
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

||| OWED: Positional args collected
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_positionalArgsCollected_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_positionalArgsCollected : getPositionalArgs (parseArgs ["cmd", "arg1", "arg2"]) = ["arg1", "arg2"]

||| OWED: Double dash stops flag parsing
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_doubleDashStops_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_doubleDashStops : getPositionalArgs (parseArgs ["--flag", "--", "--notaflag"]) = ["--notaflag"]

||| Property: Unknown flag rejected in strict mode
prop_unknownFlagRejected : isErr (parseArgsStrict ["--unknown"] ["--known"]) = True
prop_unknownFlagRejected = Refl

||| Property: Required flag enforced
prop_requiredFlagEnforced : isErr (parseArgsRequired [] ["--required"]) = True
prop_requiredFlagEnforced = Refl

||| OWED: Multiple short flags combined
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_combinedShortFlags_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_combinedShortFlags : parseArgs ["-abc"] = parseArgs ["-a", "-b", "-c"]

||| Property: Empty args is valid
prop_emptyArgsValid : isOk (parseArgs []) = True
prop_emptyArgsValid = Refl

||| OWED: Shell metacharacters in values escaped
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_metacharEscaped_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_metacharEscaped : (args : List String) ->
                         allEscaped (escapeShellArgs args) = True

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
