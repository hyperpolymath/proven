-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeYAMLProps

import Proven.Core
-- `Proven.SafeYAML` re-exports `Proven.SafeYAML.Parser`, which proven.ipkg does
-- not list and so is not installed. Import the installed sub-modules directly;
-- parser-level properties are stated as obligations indexed by the parser.
import Proven.SafeYAML.Types
import Proven.SafeYAML.Proofs
import Data.List
import System

%default total

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| The python object tag is classed as dangerous and is blocked by secure
||| defaults (configuration half of prop_codeExecBlocked).
prop_pythonObjectBlocked :
  (isDangerousTag "!!python/object", isBlockedTag Types.secureDefaults "!!python/object") = (True, True)
prop_pythonObjectBlocked = Refl

||| The python apply tag is blocked even by the permissive options
||| (configuration half of prop_unsafeTagsBlocked).
prop_pythonApplyBlockedEverywhere :
  ( isBlockedTag Types.secureDefaults "!!python/object/apply"
  , isBlockedTag Types.permissiveSecurity "!!python/object/apply") = (True, True)
prop_pythonApplyBlockedEverywhere = Refl

||| Every tag secure defaults block is a known dangerous tag.
prop_blockedTagsAreDangerous : all Types.isDangerousTag Types.secureDefaults.blockedTags = True
prop_blockedTagsAreDangerous = Refl

||| Standard core-schema tags are not classed as dangerous.
prop_standardTagsNotDangerous :
  any Types.isDangerousTag ["!!null", "!!bool", "!!int", "!!float", "!!str", "!!seq", "!!map"] = False
prop_standardTagsNotDangerous = Refl

||| Secure defaults disable anchors/aliases and custom tags
||| (configuration half of prop_yamlBombBlocked).
prop_secureDefaultsNoAnchorsNoCustomTags :
  (Types.secureDefaults.allowAnchors, Types.secureDefaults.allowCustomTags) = (False, False)
prop_secureDefaultsNoAnchorsNoCustomTags = Refl

||| Long-form and short-form core tags resolve to the same tag.
prop_parseTagLongForm : (parseTag "!!int", parseTag "tag:yaml.org,2002:int") = (TagInt, TagInt)
prop_parseTagLongForm = Refl

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

||| Shape of the secure parser, `Proven.SafeYAML.Parser.parseYAMLWith`.
public export
YAMLParser : Type
YAMLParser = YAMLSecurityOptions -> String -> YAMLResult YAMLValue

mutual
  ||| Nesting depth of a YAML value (collections add one level).
  public export
  yamlNestingDepth : YAMLValue -> Nat
  yamlNestingDepth (YArray xs) = S (yamlItemsDepth xs)
  yamlNestingDepth (YObject kvs) = S (yamlPairsDepth kvs)
  yamlNestingDepth _ = 0

  public export
  yamlItemsDepth : List YAMLValue -> Nat
  yamlItemsDepth [] = 0
  yamlItemsDepth (x :: xs) = max (yamlNestingDepth x) (yamlItemsDepth xs)

  public export
  yamlPairsDepth : List (String, YAMLValue) -> Nat
  yamlPairsDepth [] = 0
  yamlPairsDepth ((_, v) :: kvs) = max (yamlNestingDepth v) (yamlPairsDepth kvs)

||| OWED: a flat mapping parses to that mapping (was prop_validYAMLParses).
||| Blocked by: Proven.SafeYAML.Parser is not in proven.ipkg, so it is not
||| installed and cannot be imported here; instantiate with `parseYAMLWith` once
||| it is. This names the proposition; it is not a witness.
public export
YamlMappingParsesObligation : YAMLParser -> Type
YamlMappingParsesObligation p =
  p Types.secureDefaults "key: value" = Ok (YObject [("key", YString "value")])

||| OWED: an indented mapping nests (was prop_nestedYAMLParses).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlNestedParsesObligation : YAMLParser -> Type
YamlNestedParsesObligation p =
  p Types.secureDefaults "parent:\n  child: value"
    = Ok (YObject [("parent", YObject [("child", YString "value")])])

||| OWED: a block sequence parses to an array (was prop_listYAMLParses).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlSequenceParsesObligation : YAMLParser -> Type
YamlSequenceParsesObligation p =
  p Types.secureDefaults "- item1\n- item2" = Ok (YArray [YString "item1", YString "item2"])

||| OWED: an unclosed flow sequence is rejected (was prop_invalidYAMLFails).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlUnclosedRejectedObligation : YAMLParser -> Type
YamlUnclosedRejectedObligation p = isErr (p Types.secureDefaults "key: [unclosed") = True

||| OWED: a self-referential alias bomb is rejected both with anchors disabled
||| and with anchors enabled (was prop_yamlBombBlocked).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlBombRejectedObligation : YAMLParser -> Type
YamlBombRejectedObligation p =
  ( isErr (p Types.secureDefaults "a: &a [*a, *a, *a, *a, *a]")
  , isErr (p Types.standardSecurity "a: &a [*a, *a, *a, *a, *a]") ) = (True, True)

||| OWED: language object tags are rejected (was prop_codeExecBlocked and
||| prop_unsafeTagsBlocked). Blocked by: parser not installed.
||| This names the proposition; it is not a witness.
public export
YamlObjectTagsRejectedObligation : YAMLParser -> Type
YamlObjectTagsRejectedObligation p =
  ( isErr (p Types.secureDefaults "!!python/object:os.system ['ls']")
  , isErr (p Types.secureDefaults "!!python/object/apply:os.system ['ls']") ) = (True, True)

||| OWED: every accepted value respects the configured depth limit (was
||| prop_maxDepthEnforced; `nestingDepth`/`maxYAMLDepth` never existed).
||| Blocked by: parser not installed, and its fuelled loops over String
||| primitives would need a dedicated invariant proof.
||| This names the proposition; it is not a witness.
public export
YamlDepthBoundObligation : YAMLParser -> Type
YamlDepthBoundObligation p =
  (opts : YAMLSecurityOptions) -> (input : String) -> (v : YAMLValue) ->
  p opts input = Ok v -> LTE (yamlNestingDepth v) opts.maxDepth

||| OWED: a literal block scalar keeps its line breaks (was prop_multilinePreserved).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlBlockScalarObligation : YAMLParser -> Type
YamlBlockScalarObligation p =
  (s : String ** ( p Types.secureDefaults "text: |\n  line1\n  line2"
                     = Ok (YObject [("text", YString s)])
                 , isPrefixOf "line1\nline2" s = True ))

||| OWED: a benign anchor/alias resolves when anchors are enabled, and is
||| refused under secure defaults, which disable anchors (was
||| prop_anchorAliasSafe, which wrongly expected success under secure defaults).
||| Blocked by: parser not installed. This names the proposition; it is not a witness.
public export
YamlAnchorAliasObligation : YAMLParser -> Type
YamlAnchorAliasObligation p =
  ( p Types.standardSecurity "anchor: &a value\nalias: *a"
      = Ok (YObject [("anchor", YString "value"), ("alias", YString "value")])
  , isErr (p Types.secureDefaults "anchor: &a value\nalias: *a") = True )

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

||| Test runner for YAML properties
export
runYAMLProperties : IO ()
runYAMLProperties = do
  putStrLn "SafeYAML Property Tests"
  putStrLn "======================="
  putStrLn "prop_pythonObjectBlocked: PASS (proven by type)"
  putStrLn "prop_pythonApplyBlockedEverywhere: PASS (proven by type)"
  putStrLn "prop_blockedTagsAreDangerous: PASS (proven by type)"
  putStrLn "prop_standardTagsNotDangerous: PASS (proven by type)"
  putStrLn "prop_secureDefaultsNoAnchorsNoCustomTags: PASS (proven by type)"
  putStrLn "prop_parseTagLongForm: PASS (proven by type)"
  check "standardSecurity enables anchors but keeps the alias-depth bound"
    (Types.standardSecurity.allowAnchors && Types.standardSecurity.maxAliasDepth == 20)
  check "YAML value equality distinguishes structure"
    (YObject [("a", YArray [YInt 1])] == YObject [("a", YArray [YInt 1])] &&
     YObject [("a", YArray [YInt 1])] /= YObject [("a", YArray [YInt 2])])
  check "yamlNestingDepth counts collection levels"
    (yamlNestingDepth (YObject [("p", YObject [("c", YString "v")])]) == 2 &&
     yamlNestingDepth (YString "x") == 0)
  putStrLn ""
