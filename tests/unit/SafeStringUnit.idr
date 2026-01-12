-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeStringUnit

import Proven.Core
import Proven.SafeString

%default total

||| Test helper: Assert equality
assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

||| Test helper: Assert Bool is True
assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

||| Test helper: Assert Bool is False
assertFalse : String -> Bool -> IO ()
assertFalse name False = putStrLn $ "  ✓ " ++ name
assertFalse name True = putStrLn $ "  ✗ " ++ name ++ " (expected False)"

export
runStringUnitTests : IO ()
runStringUnitTests = do
  putStrLn "SafeString Unit Tests"
  putStrLn "====================="

  -- UTF-8 validation tests
  putStrLn "\n[UTF-8 Validation]"
  assertTrue "isValidUtf8 \"hello\" = True" (isValidUtf8 "hello")
  assertTrue "isValidUtf8 \"日本語\" = True" (isValidUtf8 "日本語")
  assertTrue "isValidUtf8 \"\" = True" (isValidUtf8 "")
  assertTrue "isValidUtf8 \"emoji 🎉\" = True" (isValidUtf8 "emoji 🎉")

  -- HTML escaping tests
  putStrLn "\n[HTML Escaping]"
  assertEq "escapeHtml \"<script>\" = \"&lt;script&gt;\""
           "&lt;script&gt;" (escapeHtml "<script>")
  assertEq "escapeHtml \"a & b\" = \"a &amp; b\""
           "a &amp; b" (escapeHtml "a & b")
  assertEq "escapeHtml \"\\\"quote\\\"\" = \"&quot;quote&quot;\""
           "&quot;quote&quot;" (escapeHtml "\"quote\"")
  assertEq "escapeHtml \"safe text\" = \"safe text\""
           "safe text" (escapeHtml "safe text")

  -- URL encoding tests
  putStrLn "\n[URL Encoding]"
  assertEq "urlEncode \"hello world\" = \"hello%20world\""
           "hello%20world" (urlEncode "hello world")
  assertEq "urlEncode \"a=1&b=2\" = \"a%3D1%26b%3D2\""
           "a%3D1%26b%3D2" (urlEncode "a=1&b=2")
  assertEq "urlEncode \"safe\" = \"safe\""
           "safe" (urlEncode "safe")

  -- JavaScript escaping tests
  putStrLn "\n[JavaScript Escaping]"
  assertEq "escapeJs \"alert('xss')\" escapes quotes"
           "alert(\\'xss\\')" (escapeJs "alert('xss')")
  assertEq "escapeJs handles newlines"
           "line1\\nline2" (escapeJs "line1\nline2")

  -- SQL escaping tests
  putStrLn "\n[SQL Escaping]"
  assertEq "escapeSql \"O'Brien\" = \"O''Brien\""
           "O''Brien" (escapeSql "O'Brien")
  assertEq "escapeSql \"normal\" = \"normal\""
           "normal" (escapeSql "normal")

  -- Trim tests
  putStrLn "\n[Trim]"
  assertEq "trim \"  hello  \" = \"hello\""
           "hello" (trim "  hello  ")
  assertEq "trim \"no spaces\" = \"no spaces\""
           "no spaces" (trim "no spaces")
  assertEq "trim \"\" = \"\""
           "" (trim "")

  -- Length safety tests
  putStrLn "\n[Length Safety]"
  assertEq "safeLength \"hello\" = 5" 5 (safeLength "hello")
  assertEq "safeLength \"\" = 0" 0 (safeLength "")
  assertEq "safeLength \"日本\" = 2" 2 (safeLength "日本")

  putStrLn "\n✓ SafeString unit tests complete"
