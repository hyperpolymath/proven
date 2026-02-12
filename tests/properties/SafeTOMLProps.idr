-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeTOMLProps

import Proven.Core
import Proven.SafeTOML

%default total

||| Property: Valid TOML parses
prop_validTOMLParses : isOk (parseTOML "key = \"value\"") = True
prop_validTOMLParses = Refl

||| Property: Section header parses
prop_sectionParses : isOk (parseTOML "[section]\nkey = \"value\"") = True
prop_sectionParses = Refl

||| Property: Nested section parses
prop_nestedSectionParses : isOk (parseTOML "[parent.child]\nkey = \"value\"") = True
prop_nestedSectionParses = Refl

||| Property: Array parses
prop_arrayParses : isOk (parseTOML "arr = [1, 2, 3]") = True
prop_arrayParses = Refl

||| Property: Inline table parses
prop_inlineTableParses : isOk (parseTOML "point = { x = 1, y = 2 }") = True
prop_inlineTableParses = Refl

||| Property: Invalid TOML fails
prop_invalidTOMLFails : isErr (parseTOML "key = [unclosed") = True
prop_invalidTOMLFails = Refl

||| Property: Duplicate key fails
prop_duplicateKeyFails : isErr (parseTOML "key = 1\nkey = 2") = True
prop_duplicateKeyFails = Refl

||| Property: Integer parses correctly
prop_integerParses : isOk (parseTOML "num = 42") = True
prop_integerParses = Refl

||| Property: Float parses correctly
prop_floatParses : isOk (parseTOML "num = 3.14") = True
prop_floatParses = Refl

||| Property: Boolean parses correctly
prop_booleanParses : isOk (parseTOML "flag = true") = True
prop_booleanParses = Refl

||| Property: DateTime parses correctly
prop_datetimeParses : isOk (parseTOML "dt = 2024-01-15T10:30:00Z") = True
prop_datetimeParses = Refl

||| Property: Multiline string parses
prop_multilineStringParses : isOk (parseTOML "str = \"\"\"\nmulti\nline\n\"\"\"") = True
prop_multilineStringParses = Refl

||| Test runner for TOML properties
export
runTOMLProperties : IO ()
runTOMLProperties = do
  putStrLn "SafeTOML Property Tests"
  putStrLn "======================="
  putStrLn "prop_validTOMLParses: PASS (proven by type)"
  putStrLn "prop_sectionParses: PASS (proven by type)"
  putStrLn "prop_nestedSectionParses: PASS (proven by type)"
  putStrLn "prop_arrayParses: PASS (proven by type)"
  putStrLn "prop_inlineTableParses: PASS (proven by type)"
  putStrLn "prop_invalidTOMLFails: PASS (proven by type)"
  putStrLn "prop_duplicateKeyFails: PASS (proven by type)"
  putStrLn "prop_integerParses: PASS (proven by type)"
  putStrLn "prop_floatParses: PASS (proven by type)"
  putStrLn "prop_booleanParses: PASS (proven by type)"
  putStrLn "prop_datetimeParses: PASS (proven by type)"
  putStrLn "prop_multilineStringParses: PASS (proven by type)"
  putStrLn ""
