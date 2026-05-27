-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeYAMLProps

import Proven.Core
import Proven.SafeYAML

%default total

||| Property: Valid YAML parses
prop_validYAMLParses : isOk (parseYAML "key: value") = True
prop_validYAMLParses = Refl

||| Property: Nested YAML parses
prop_nestedYAMLParses : isOk (parseYAML "parent:\n  child: value") = True
prop_nestedYAMLParses = Refl

||| Property: List YAML parses
prop_listYAMLParses : isOk (parseYAML "- item1\n- item2") = True
prop_listYAMLParses = Refl

||| Property: Invalid YAML fails
prop_invalidYAMLFails : isErr (parseYAML "key: [unclosed") = True
prop_invalidYAMLFails = Refl

||| OWED: YAML bomb blocked (alias expansion)
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_yamlBombBlocked_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_yamlBombBlocked : isErr (parseYAML "a: &a [*a, *a, *a, *a, *a]") = True

||| Property: Arbitrary code execution blocked
prop_codeExecBlocked : isErr (parseYAML "!!python/object:os.system ['ls']") = True
prop_codeExecBlocked = Refl

||| Property: Only safe tags allowed
prop_unsafeTagsBlocked : isErr (parseYAML "!!python/object/apply:os.system ['ls']") = True
prop_unsafeTagsBlocked = Refl

||| OWED: Max depth enforced
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_maxDepthEnforced_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_maxDepthEnforced : (yaml : String) ->
                          nestingDepth yaml > maxYAMLDepth ->
                          isErr (parseYAML yaml) = True

||| Property: Multiline string preserves content
prop_multilinePreserved : isOk (parseYAML "text: |\n  line1\n  line2") = True
prop_multilinePreserved = Refl

||| Property: Anchor and alias work safely
prop_anchorAliasSafe : isOk (parseYAML "anchor: &a value\nalias: *a") = True
prop_anchorAliasSafe = Refl

||| Test runner for YAML properties
export
runYAMLProperties : IO ()
runYAMLProperties = do
  putStrLn "SafeYAML Property Tests"
  putStrLn "======================="
  putStrLn "prop_validYAMLParses: PASS (proven by type)"
  putStrLn "prop_nestedYAMLParses: PASS (proven by type)"
  putStrLn "prop_listYAMLParses: PASS (proven by type)"
  putStrLn "prop_invalidYAMLFails: PASS (proven by type)"
  putStrLn "prop_codeExecBlocked: PASS (proven by type)"
  putStrLn "prop_unsafeTagsBlocked: PASS (proven by type)"
  putStrLn "prop_multilinePreserved: PASS (proven by type)"
  putStrLn "prop_anchorAliasSafe: PASS (proven by type)"
  putStrLn ""
