-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeYAMLProps

import Proven.Core
-- Parser-level properties are stated as obligations indexed by the parser and
-- exercised on `parseYAMLWith` below. Security properties are executed on flow
-- syntax: block collections are a known defect, and a rejection caused by that
-- defect would be vacuous evidence.
import Proven.SafeYAML.Types
import Proven.SafeYAML.Proofs
import Proven.SafeYAML.Parser
import Data.List
import System
import KnownDefects

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
||| Blocked by: no proof over the parser's fuelled String loops; executed below
||| on `parseYAMLWith`. This names the proposition; it is not a witness.
public export
YamlMappingParsesObligation : YAMLParser -> Type
YamlMappingParsesObligation p =
  p Types.secureDefaults "key: value" = Ok (YObject [("key", YString "value")])

||| OWED: an indented mapping nests (was prop_nestedYAMLParses).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
YamlNestedParsesObligation : YAMLParser -> Type
YamlNestedParsesObligation p =
  p Types.secureDefaults "parent:\n  child: value"
    = Ok (YObject [("parent", YObject [("child", YString "value")])])

||| OWED: a block sequence parses to an array (was prop_listYAMLParses).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
YamlSequenceParsesObligation : YAMLParser -> Type
YamlSequenceParsesObligation p =
  p Types.secureDefaults "- item1\n- item2" = Ok (YArray [YString "item1", YString "item2"])

||| OWED: an unclosed flow sequence is rejected (was prop_invalidYAMLFails).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
YamlUnclosedRejectedObligation : YAMLParser -> Type
YamlUnclosedRejectedObligation p = isErr (p Types.secureDefaults "key: [unclosed") = True

||| OWED: a self-referential alias bomb is rejected both with anchors disabled
||| and with anchors enabled (was prop_yamlBombBlocked).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
YamlBombRejectedObligation : YAMLParser -> Type
YamlBombRejectedObligation p =
  ( isErr (p Types.secureDefaults "a: &a [*a, *a, *a, *a, *a]")
  , isErr (p Types.standardSecurity "a: &a [*a, *a, *a, *a, *a]") ) = (True, True)

||| OWED: language object tags are rejected (was prop_codeExecBlocked and
||| prop_unsafeTagsBlocked). Blocked by: no proof over the parser; executed below.
||| This names the proposition; it is not a witness.
public export
YamlObjectTagsRejectedObligation : YAMLParser -> Type
YamlObjectTagsRejectedObligation p =
  ( isErr (p Types.secureDefaults "!!python/object:os.system ['ls']")
  , isErr (p Types.secureDefaults "!!python/object/apply:os.system ['ls']") ) = (True, True)

||| OWED: every accepted value respects the configured depth limit (was
||| prop_maxDepthEnforced; `nestingDepth`/`maxYAMLDepth` never existed).
||| Blocked by: the parser's fuelled loops over String
||| primitives would need a dedicated invariant proof.
||| This names the proposition; it is not a witness.
public export
YamlDepthBoundObligation : YAMLParser -> Type
YamlDepthBoundObligation p =
  (opts : YAMLSecurityOptions) -> (input : String) -> (v : YAMLValue) ->
  p opts input = Ok v -> LTE (yamlNestingDepth v) opts.maxDepth

||| OWED: a literal block scalar keeps its line breaks (was prop_multilinePreserved).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
public export
YamlBlockScalarObligation : YAMLParser -> Type
YamlBlockScalarObligation p =
  (s : String ** ( p Types.secureDefaults "text: |\n  line1\n  line2"
                     = Ok (YObject [("text", YString s)])
                 , isPrefixOf "line1\nline2" s = True ))

||| OWED: a benign anchor/alias resolves when anchors are enabled, and is
||| refused under secure defaults, which disable anchors (was
||| prop_anchorAliasSafe, which wrongly expected success under secure defaults).
||| Blocked by: no proof over the parser; executed below. This names the proposition; it is not a witness.
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

private
yamlIs : YAMLValue -> YAMLResult YAMLValue -> Bool
yamlIs v (Ok w) = v == w
yamlIs _ (Err _) = False

private
isDangerous : YAMLResult YAMLValue -> Bool
isDangerous (Err (DangerousTag _)) = True
isDangerous _ = False

private
isSyntaxError : YAMLResult YAMLValue -> Bool
isSyntaxError (Err (SyntaxError _ _ _)) = True
isSyntaxError _ = False

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
  -- Parser, flow syntax (non-vacuous evidence)
  check "flow mapping parses to that mapping"
    (yamlIs (YObject [("key", YString "value")]) (parseYAMLWith Types.secureDefaults "{key: value}"))
  check "flow sequence parses to an array"
    (yamlIs (YArray [YString "a", YString "b"]) (parseYAMLWith Types.secureDefaults "[a, b]"))
  check "unclosed flow sequence rejected with a syntax error"
    (isSyntaxError (parseYAMLWith Types.secureDefaults "[unclosed"))
  check "language object tags rejected as dangerous (prop_codeExecBlocked)"
    (isDangerous (parseYAMLWith Types.secureDefaults "!!python/object:os.system ['ls']") &&
     isDangerous (parseYAMLWith Types.secureDefaults "!!python/object/apply:os.system ['ls']"))
  check "anchors refused under secure defaults (prop_yamlBombBlocked, configuration)"
    (isDangerous (parseYAMLWith Types.secureDefaults "[&a [x, y], *a, *a]"))
  check "benign alias resolves under standardSecurity"
    (yamlIs (YArray [YString "x", YString "x"]) (parseYAMLWith Types.standardSecurity "[&a x, *a]"))
  check "self-referential alias rejected under standardSecurity (prop_yamlBombBlocked)"
    (isErr (parseYAMLWith Types.standardSecurity "&a [*a, *a, *a]"))
  -- Block syntax: known defect (see KnownDefects); correct expectations kept.
  knownDefect YamlBlockCollectionsRejected "block mapping key: value parses"
    (yamlIs (YObject [("key", YString "value")]) (parseYAMLWith Types.secureDefaults "key: value"))
  knownDefect YamlBlockCollectionsRejected "indented block mapping nests"
    (yamlIs (YObject [("parent", YObject [("child", YString "value")])])
            (parseYAMLWith Types.secureDefaults "parent:\n  child: value"))
  knownDefect YamlBlockCollectionsRejected "block sequence parses to an array"
    (yamlIs (YArray [YString "item1", YString "item2"]) (parseYAMLWith Types.secureDefaults "- item1\n- item2"))
  knownDefect YamlBlockCollectionsRejected "literal block scalar keeps line breaks"
    (case parseYAMLWith Types.secureDefaults "text: |\n  line1\n  line2" of
       Ok (YObject [("text", YString t)]) => isPrefixOf "line1\nline2" t
       _ => False)
  putStrLn ""
