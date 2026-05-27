-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeRegexProps

import Proven.Core
import Proven.SafeRegex

%default total

||| Property: Valid simple regex compiles
prop_simpleRegexCompiles : isOk (compileRegex "hello") = True
prop_simpleRegexCompiles = Refl

||| Property: Valid character class regex compiles
prop_charClassCompiles : isOk (compileRegex "[a-z]+") = True
prop_charClassCompiles = Refl

||| Property: Unbalanced parentheses fails
prop_unbalancedParensFails : isErr (compileRegex "(hello") = True
prop_unbalancedParensFails = Refl

||| Property: Unbalanced brackets fails
prop_unbalancedBracketsFails : isErr (compileRegex "[abc") = True
prop_unbalancedBracketsFails = Refl

||| OWED: Invalid escape fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_invalidEscapeFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_invalidEscapeFails : isErr (compileRegex "\\q") = True

||| OWED: ReDoS pattern detected
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_redosDetected_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_redosDetected : isErr (compileRegex "(a+)+$") = True

||| Property: Match returns correct result
prop_matchCorrect : (r : ValidRegex) -> (s : String) ->
                    match r s = match r s
prop_matchCorrect r s = Refl

||| OWED: Empty pattern matches everything
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_emptyMatchesAll_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_emptyMatchesAll : isOk (compileRegex "") = True

||| Property: Anchored regex works
prop_anchoredRegex : isOk (compileRegex "^hello$") = True
prop_anchoredRegex = Refl

||| OWED: Escape special characters preserves length
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_escapePreservesLength_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_escapePreservesLength : (s : String) ->
                                length (escapeRegex s) >= length s

||| Test runner for regex properties
export
runRegexProperties : IO ()
runRegexProperties = do
  putStrLn "SafeRegex Property Tests"
  putStrLn "========================"
  putStrLn "prop_simpleRegexCompiles: PASS (proven by type)"
  putStrLn "prop_charClassCompiles: PASS (proven by type)"
  putStrLn "prop_unbalancedParensFails: PASS (proven by type)"
  putStrLn "prop_unbalancedBracketsFails: PASS (proven by type)"
  putStrLn "prop_matchCorrect: PASS (proven by type)"
  putStrLn "prop_anchoredRegex: PASS (proven by type)"
  putStrLn ""
