-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeArgsProps

import Proven.Core
import Proven.SafeArgs
import Data.List
import System

%default total

-- The real API is `parse : List ArgSpec -> List String -> ArgResult ParsedArgs`
-- (and parseStrict/parsePermissive). `parseArgs` is exported abstractly
-- (`export`, not `public export`), so closed parse results do not reduce at
-- the type level; the closed properties are execution checks below.

||| OWED: option values are carried verbatim (shell metacharacters are inert
||| data, never interpreted). Blocked by: `Parser.parseArgs` is `export`, not
||| `public export`, so its body is opaque to this module and the statement
||| cannot be reduced or inducted on here. This names the proposition; it is
||| not a witness.
public export
ArgsOptionValueVerbatimObligation : Type
ArgsOptionValueVerbatimObligation =
  (v : String) -> LTE (length (unpack v)) 4096 ->
  map (\a => getValue a "output") (parse [outputOption] ["--output", v])
    = Ok (Just v)

||| OWED: a bundle of known short flags parses to the same flag set as the
||| separated flags, for every list of alphabetic flag characters. Blocked by:
||| `parseArgs`/`classifyArg` are `export` (opaque) and use String/Char
||| primitives (`unpack`, `isAlpha`) that do not reduce symbolically. This
||| names the proposition; it is not a witness.
public export
ArgsBundlingEquivalenceObligation : Type
ArgsBundlingEquivalenceObligation =
  (cs : List Char) -> (c : Char) -> all isAlpha (c :: cs) = True ->
  parse (map (\x => mkFlag (singleton x) x "") (c :: cs)) ["-" ++ pack (c :: cs)]
    = parse (map (\x => mkFlag (singleton x) x "") (c :: cs))
            (map (\x => "-" ++ singleton x) (c :: cs))

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
okAnd : (ParsedArgs -> Bool) -> ArgResult ParsedArgs -> Bool
okAnd p (Ok a) = p a
okAnd _ (Err _) = False

private
abcSpecs : List ArgSpec
abcSpecs = [mkFlag "a" 'a' "", mkFlag "b" 'b' "", mkFlag "c" 'c' ""]

private
metaValues : List String
metaValues = ["; rm -rf /", "$(whoami)", "`id`", "a|b", "x && y", "*", "'\"", ""]

||| Test runner for args properties
export
runArgsProperties : IO ()
runArgsProperties = do
  putStrLn "SafeArgs Property Tests"
  putStrLn "======================="
  check "simple short flag -v parses and sets verbose"
    (okAnd (\a => isSet a "verbose") (parse [verboseFlag] ["-v"]))
  check "long flag --verbose parses and sets verbose"
    (okAnd (\a => isSet a "verbose") (parse [verboseFlag] ["--verbose"]))
  check "option with separate value: --output file.txt"
    (okAnd (\a => getValue a "output" == Just "file.txt")
           (parse [outputOption] ["--output", "file.txt"]))
  check "option with equals: --output=file.txt"
    (okAnd (\a => getValue a "output" == Just "file.txt")
           (parse [outputOption] ["--output=file.txt"]))
  check "-- stops flag parsing: later --notaflag is a rest argument"
    (okAnd (\a => isSet a "flag" && getRestArgs a == ["--notaflag"])
           (parse [mkLongFlag "flag" ""] ["--flag", "--", "--notaflag"]))
  check "unknown flag rejected in strict mode"
    (isErr (parseStrict [mkLongFlag "known" ""] ["--unknown"]))
  check "unknown flag rejected with default options"
    (isErr (parse [mkLongFlag "known" ""] ["--unknown"]))
  check "missing required option enforced"
    (isErr (parse [mkRequired "required" 'r' ""] []))
  check "required option satisfied when supplied"
    (okAnd (\a => getValue a "required" == Just "x")
           (parse [mkRequired "required" 'r' ""] ["--required", "x"]))
  check "combined short flags -abc equal -a -b -c"
    (case (parse abcSpecs ["-abc"], parse abcSpecs ["-a", "-b", "-c"]) of
       (Ok x, Ok y) => x == y && all (isSet x) ["a", "b", "c"]
       _ => False)
  check "strict mode does not bundle -abc"
    (isErr (parseStrict abcSpecs ["-abc"]))
  check "empty args with empty spec is valid"
    (isOk (parse [] []))
  check "shell metacharacters in option values carried verbatim"
    (all (\v => okAnd (\a => getValue a "output" == Just v)
                      (parse [outputOption] ["--output", v])) metaValues)
  -- Regression (fixed): positional specs are required, but checkRequired
  -- looked them up under specName "<positional>" while values are stored
  -- as "positional-N".
  check "positional args collected in order"
    (okAnd (\a => getPos a 0 == Just "cmd" && getPos a 1 == Just "arg1" &&
                  getPos a 2 == Just "arg2")
           (parse [mkPositional "cmd", mkPositional "a1", mkPositional "a2"]
                  ["cmd", "arg1", "arg2"]))
  check "missing required positional still rejected"
    (isErr (parse [mkPositional "cmd", mkPositional "a1"] ["cmd"]) &&
     isErr (parse [mkPositional "cmd"] []))
  check "positionals interleaved with flags are satisfied"
    (okAnd (\a => isSet a "verbose" && getPos a 0 == Just "x")
           (parse [verboseFlag, mkPositional "cmd"] ["-v", "x"]))
  putStrLn ""
