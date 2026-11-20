-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeTOMLProps

import Proven.Core
import Proven.SafeTOML
import System
import KnownDefects

%default total

-- `parseTOML`/`parseValue` are `export` (not `public export`), so their results
-- cannot reduce at the type level; document-level properties are execution
-- checks. The original `isOk (parseTOML ...)` checks only asked whether the
-- parser returned *something*; they are strengthened here to check the parsed
-- content, which is what "parses" means.

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

mutual
  ||| Mapping the identity over a TOML value is the identity (functor law).
  prop_mapValuesIdentity : (v : TOMLValue) -> mapValues Prelude.id v = v
  prop_mapValuesIdentity (TString _) = Refl
  prop_mapValuesIdentity (TInt _) = Refl
  prop_mapValuesIdentity (TFloat _) = Refl
  prop_mapValuesIdentity (TBool _) = Refl
  prop_mapValuesIdentity (TDateTime _) = Refl
  prop_mapValuesIdentity (TDate _) = Refl
  prop_mapValuesIdentity (TTime _) = Refl
  prop_mapValuesIdentity (TArray xs) = cong TArray (mapItemsIdentity xs)
  prop_mapValuesIdentity (TInlineTable kvs) = cong TInlineTable (mapPairsIdentity kvs)
  prop_mapValuesIdentity (TTable kvs) = cong TTable (mapPairsIdentity kvs)

  private
  mapItemsIdentity : (xs : List TOMLValue) -> mapValuesItems Prelude.id xs = xs
  mapItemsIdentity [] = Refl
  mapItemsIdentity (x :: xs) = cong2 (::) (prop_mapValuesIdentity x) (mapItemsIdentity xs)

  private
  mapPairsIdentity : (kvs : List (String, TOMLValue)) -> mapValuesPairs Prelude.id kvs = kvs
  mapPairsIdentity [] = Refl
  mapPairsIdentity ((k, v) :: kvs) =
    cong2 (::) (cong (MkPair k) (prop_mapValuesIdentity v)) (mapPairsIdentity kvs)

||| Typed coercion refuses a mismatched value: a string is never an integer.
prop_asIntRejectsString : (s : String) -> isErr (asInt (TString s)) = True
prop_asIntRejectsString s = Refl

||| Typed coercion refuses a mismatched value: an integer is never a boolean.
prop_asBoolRejectsInt : (i : Integer) -> isErr (asBool (TInt i)) = True
prop_asBoolRejectsInt i = Refl

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
resultIs : Eq a => a -> Result e a -> Bool
resultIs x (Ok y) = x == y
resultIs _ (Err _) = False

||| Parse `src`, then apply an accessor to the document.
private
parsedThen : String -> (TOMLDocument -> Result TOMLError a) -> Result TOMLError a
parsedThen src f = case parseTOML src of
  Ok doc => f doc
  Err e => Err e

||| Test runner for TOML properties
export
runTOMLProperties : IO ()
runTOMLProperties = do
  putStrLn "SafeTOML Property Tests"
  putStrLn "======================="
  putStrLn "prop_mapValuesIdentity: PASS (proven by type)"
  putStrLn "prop_asIntRejectsString: PASS (proven by type)"
  putStrLn "prop_asBoolRejectsInt: PASS (proven by type)"
  check "empty and comment-only documents parse to the empty document"
    (resultIs [] (parseTOML "") && resultIs [] (parseTOML "# comment\n\n"))
  check "parseValue: integer, float, boolean, basic and literal strings"
    (resultIs (TInt 42) (parseValue "42") &&
     resultIs (TInt 1000) (parseValue "1_000") &&
     resultIs (TFloat 3.14) (parseValue "3.14") &&
     resultIs (TBool True) (parseValue "true") &&
     resultIs (TString "a\tb") (parseValue "\"a\\tb\"") &&
     resultIs (TString "C:\\x") (parseValue "'C:\\x'"))
  check "parseValue rejects a bad escape and a bare word"
    (isErr (parseValue "\"\\q\"") && isErr (parseValue "nope"))
  check "accessors and dotted paths on a constructed document"
    (let doc = mkDocument [("title", TString "t"),
                           ("server", mkTable [("port", TInt 8080)])]
     in resultIs "t" (getString "title" doc) &&
        resultIs (TInt 8080) (getPath "server.port" doc) &&
        isErr (getPath "server.host" doc))
  check "mergeDocuments: second document wins on conflicts"
    (mergeDocuments [("a", TInt 1), ("b", TInt 2)] [("b", TInt 3)]
       == [("a", TInt 1), ("b", TInt 3)])
  -- Document-level parsing (original intent of every prop_*Parses / *Fails).
  -- Known defect: parseTOMLWith is a stub (see KnownDefects). Expectations are
  -- the correct ones; the run fails if any of them starts holding unrecorded.
  knownDefect TomlParserStub "prop_validTOMLParses: key = \"value\" yields key -> \"value\""
    (resultIs "value" (parsedThen "key = \"value\"" (getString "key")))
  knownDefect TomlParserStub "prop_integerParses: num = 42"
    (resultIs 42 (parsedThen "num = 42" (getInt "num")))
  knownDefect TomlParserStub "prop_floatParses: num = 3.14"
    (resultIs 3.14 (parsedThen "num = 3.14" (getFloat "num")))
  knownDefect TomlParserStub "prop_booleanParses: flag = true"
    (resultIs True (parsedThen "flag = true" (getBool "flag")))
  knownDefect TomlParserStub "prop_sectionParses: [section] key"
    (resultIs (TString "value") (parsedThen "[section]\nkey = \"value\"" (getPath "section.key")))
  knownDefect TomlParserStub "prop_nestedSectionParses: [parent.child] key"
    (resultIs (TString "value")
       (parsedThen "[parent.child]\nkey = \"value\"" (getPath "parent.child.key")))
  knownDefect TomlParserStub "prop_arrayParses: arr = [1, 2, 3]"
    (resultIs [TInt 1, TInt 2, TInt 3] (parsedThen "arr = [1, 2, 3]" (getArray "arr")))
  knownDefect TomlParserStub "prop_inlineTableParses: point = { x = 1, y = 2 }"
    (resultIs (TInt 2) (parsedThen "point = { x = 1, y = 2 }" (getPath "point.y")))
  knownDefect TomlParserStub "prop_datetimeParses: dt = 2024-01-15T10:30:00Z"
    (isOk (parsedThen "dt = 2024-01-15T10:30:00Z" (\d => getField "dt" d >>= asDateTime)))
  knownDefect TomlParserStub "prop_multilineStringParses: str = \"\"\"multi line\"\"\""
    (resultIs "multi\nline\n"
       (parsedThen "str = \"\"\"\nmulti\nline\n\"\"\"" (getString "str")))
  knownDefect TomlParserStub "prop_invalidTOMLFails: unclosed array rejected"
    (isErr (parseTOML "key = [unclosed"))
  knownDefect TomlParserStub "prop_duplicateKeyFails: duplicate key rejected"
    (isErr (parseTOML "key = 1\nkey = 2"))
  knownDefect TomlParserStub "parseValue: array literal [1, 2, 3] keeps its elements"
    (resultIs (TArray [TInt 1, TInt 2, TInt 3]) (parseValue "[1, 2, 3]"))
  putStrLn ""
