-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeRegexProps

import Proven.Core
import Proven.SafeRegex
import Proven.SafeString.Escape
import Data.Either
import System

%default total

-- The real API: `regex : String -> Either RegexError SafeRegex` (parse, then
-- reject patterns whose complexity analysis fails the normal safety level),
-- `match`/`test`/`testFull`, `hasRedosRisk`, and `escapeRegex` (which lives
-- in Proven.SafeString.Escape, not SafeRegex). There is no `compileRegex`
-- and no `ValidRegex`. The parser and matcher run over `unpack`, which does
-- not reduce at the type level, so compile/match properties are execution
-- checks.

||| Property: `test` agrees with `match`'s matched flag, for every regex and
||| input (replaces the orphan's `match r s = match r s`, which held for any
||| function).
prop_testAgreesWithMatch : (sr : SafeRegex) -> (s : String) ->
                           test sr s = (match sr s).matched
prop_testAgreesWithMatch sr s = Refl

||| OWED: escaping never shortens a string. Blocked by: `escapeRegex` is
||| `pack . go . unpack`, and `length`/`pack`/`unpack` on String do not reduce
||| on abstract input. This names the proposition; it is not a witness.
public export
EscapePreservesLengthObligation : Type
EscapePreservesLengthObligation =
  (s : String) -> LTE (length s) (length (escapeRegex s))

||| OWED: an escaped string compiles and matches exactly the original string.
||| Blocked by: the parser and matcher are fuel-driven over `unpack`, which
||| does not reduce on abstract input. This names the proposition; it is not
||| a witness.
public export
EscapeMatchesLiterallyObligation : Type
EscapeMatchesLiterallyObligation =
  (s : String) -> (sr : SafeRegex) -> regex (escapeRegex s) = Right sr ->
  testFull sr s = True

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
compiles : String -> Bool
compiles = isRight . regex

private
escapeCorpus : List String
escapeCorpus = ["", "abc", "a.b", "1+1=2", "(x)", "[a-z]*", "^$|\\", "{3}?"]

||| Test runner for regex properties
export
runRegexProperties : IO ()
runRegexProperties = do
  putStrLn "SafeRegex Property Tests"
  putStrLn "========================"
  putStrLn "prop_testAgreesWithMatch: PASS (proven by type, all regexes/inputs)"
  check "simple regex compiles" (compiles "hello")
  check "character class regex compiles" (compiles "[a-z]+")
  check "unbalanced parentheses rejected" (not (compiles "(hello"))
  check "unbalanced brackets rejected" (not (compiles "[abc"))
  check "invalid escape \\q rejected" (not (compiles "\\q"))
  check "ReDoS pattern (a+)+$ rejected and flagged"
    (not (compiles "(a+)+$") && hasRedosRisk "(a+)+$")
  check "anchored regex compiles and matches only the whole word"
    (compiles "^hello$" && quickTest "^hello$" "hello" &&
     not (quickTest "^hello$" "hello world"))
  check "empty pattern compiles and matches every input"
    (compiles "" && all (quickTest "") ["", "a", "hello world"])
  check "match: positive and negative cases"
    (quickTest "[a-z]+" "123abc" && not (quickTest "[a-z]+" "123") &&
     quickTestFull "[a-z]+" "abc" && not (quickTestFull "[a-z]+" "abc1"))
  check "escaping never shortens (instances of EscapePreservesLengthObligation)"
    (all (\s => length (escapeRegex s) >= length s) escapeCorpus)
  check "escaped string matches itself literally (instances of EscapeMatchesLiterallyObligation)"
    (all (\s => quickTestFull (escapeRegex s) s) escapeCorpus &&
     not (quickTestFull (escapeRegex "a.b") "axb"))
  putStrLn ""
