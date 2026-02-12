-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeRegexUnit

import Proven.Core
import Proven.SafeRegex

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

export
runRegexUnitTests : IO ()
runRegexUnitTests = do
  putStrLn "SafeRegex Unit Tests"
  putStrLn "===================="

  -- Parse tests
  putStrLn "\n[Parsing]"
  assertOk "regex \"hello\" parses literal" (regex "hello")
  assertOk "regex \"a*\" parses quantifier" (regex "a*")
  assertOk "regex \"[abc]\" parses char class" (regex "[abc]")
  assertOk "regex \"a|b\" parses alternation" (regex "a|b")
  assertOk "regex \"^start$\" parses anchors" (regex "^start$")
  assertErr "regex \"[\" rejects unclosed bracket" (regex "[")
  assertErr "regex \"*\" rejects leading quantifier" (regex "*")

  -- Matching tests
  putStrLn "\n[Matching]"
  case regex "hello" of
    Ok re => do
      assertTrue "test \"hello\" \"hello\" = True" (test re "hello")
      assertTrue "test \"hello\" \"world\" = False" (not $ test re "world")
      assertTrue "test \"hello\" \"say hello\" = True" (test re "say hello")
    Err _ => putStrLn "  ✗ Failed to compile regex"

  case regex "^[a-z]+$" of
    Ok re => do
      assertTrue "^[a-z]+$ matches \"abc\"" (test re "abc")
      assertTrue "^[a-z]+$ rejects \"ABC\"" (not $ test re "ABC")
      assertTrue "^[a-z]+$ rejects \"ab1\"" (not $ test re "ab1")
    Err _ => putStrLn "  ✗ Failed to compile regex"

  -- ReDoS detection tests
  putStrLn "\n[ReDoS Detection]"
  assertTrue "detectReDoS \"(a+)+\" = True (nested quantifiers)"
             (detectReDoS "(a+)+")
  assertTrue "detectReDoS \"(a|a)+\" = True (overlapping)"
             (detectReDoS "(a|a)+")
  assertTrue "detectReDoS \"(a*)*\" = True (nested star)"
             (detectReDoS "(a*)*")
  assertTrue "detectReDoS \"abc\" = False (safe)"
             (not $ detectReDoS "abc")
  assertTrue "detectReDoS \"a+b+c+\" = False (no nesting)"
             (not $ detectReDoS "a+b+c+")

  -- Complexity analysis tests
  putStrLn "\n[Complexity Analysis]"
  assertEq "analyzeComplexity \"abc\" = Linear" Linear (analyzeComplexity "abc")
  assertEq "analyzeComplexity \"a+\" = Linear" Linear (analyzeComplexity "a+")
  assertEq "analyzeComplexity \"(a+)+\" = Exponential" Exponential (analyzeComplexity "(a+)+")

  -- Pre-built patterns tests
  putStrLn "\n[Pre-built Patterns]"
  case emailRegex of
    Ok re => assertTrue "emailRegex matches valid email" (test re "user@example.com")
    Err _ => putStrLn "  ✗ Failed to compile email regex"

  case ipv4Regex of
    Ok re => assertTrue "ipv4Regex matches 192.168.1.1" (test re "192.168.1.1")
    Err _ => putStrLn "  ✗ Failed to compile IPv4 regex"

  case uuidRegex of
    Ok re => assertTrue "uuidRegex matches UUID" (test re "550e8400-e29b-41d4-a716-446655440000")
    Err _ => putStrLn "  ✗ Failed to compile UUID regex"

  -- Safe matching with step limits
  putStrLn "\n[Step-Limited Matching]"
  case safeRegex Normal "a*b" of
    Ok re => do
      assertOk "safeMatch with reasonable input" (safeMatch re "aaab")
      -- Long input should still work within limits
      assertOk "safeMatch with moderate input" (safeMatch re (replicate 100 'a' ++ "b"))
    Err _ => putStrLn "  ✗ Failed to compile safe regex"

  -- Replacement tests
  putStrLn "\n[Replacement]"
  case regex "world" of
    Ok re => assertEq "replaceAll \"world\" \"universe\" in \"hello world\""
                      "hello universe" (replaceAll re "universe" "hello world")
    Err _ => putStrLn "  ✗ Failed to compile regex"

  -- Split tests
  putStrLn "\n[Split]"
  case regex "," of
    Ok re => assertEq "split by comma" ["a", "b", "c"] (split re "a,b,c")
    Err _ => putStrLn "  ✗ Failed to compile regex"

  putStrLn "\n✓ SafeRegex unit tests complete"
