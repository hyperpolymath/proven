-- SPDX-License-Identifier: PMPL-1.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeHtmlUnit

import Proven.Core
import Proven.SafeHtml

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runHtmlUnitTests : IO ()
runHtmlUnitTests = do
  putStrLn "SafeHtml Unit Tests"
  putStrLn "==================="

  -- Escaping tests
  putStrLn "\n[Content Escaping]"
  assertEq "escapeContent \"<script>\" = \"&lt;script&gt;\""
           "&lt;script&gt;" (escapeContent "<script>")
  assertEq "escapeContent \"a & b\" = \"a &amp; b\""
           "a &amp; b" (escapeContent "a & b")
  assertEq "escapeContent quotes"
           "&quot;quoted&quot;" (escapeContent "\"quoted\"")
  assertEq "escapeContent apostrophe"
           "it&#39;s" (escapeContent "it's")
  assertEq "escapeContent safe text unchanged"
           "Hello World" (escapeContent "Hello World")

  -- Attribute escaping tests
  putStrLn "\n[Attribute Escaping]"
  assertEq "escapeAttribute with quotes"
           "say &quot;hello&quot;" (escapeAttribute "say \"hello\"")
  assertEq "escapeAttribute with special chars"
           "a&amp;b" (escapeAttribute "a&b")

  -- URL sanitization tests
  putStrLn "\n[URL Sanitization]"
  assertOk "sanitizeUrl \"https://example.com\" allowed" (sanitizeUrl "https://example.com")
  assertOk "sanitizeUrl \"http://example.com\" allowed" (sanitizeUrl "http://example.com")
  assertErr "sanitizeUrl \"javascript:alert(1)\" blocked" (sanitizeUrl "javascript:alert(1)")
  assertErr "sanitizeUrl \"data:text/html,...\" blocked" (sanitizeUrl "data:text/html,<script>")
  assertErr "sanitizeUrl \"vbscript:...\" blocked" (sanitizeUrl "vbscript:msgbox")

  -- Element builder tests
  putStrLn "\n[Element Builder]"
  let div = elem "div"
            |> withAttr "class" "container"
            |> withAttr "id" "main"
            |> withText "Hello"
            |> build
  assertOk "elem builder creates valid HTML" div

  let link = elem "a"
             |> withAttr "href" "https://example.com"
             |> withText "Click here"
             |> build
  assertOk "link builder works" link

  -- Nested elements tests
  putStrLn "\n[Nested Elements]"
  let nested = elem "div"
               |> withChild (elem "p" |> withText "Paragraph" |> build)
               |> build
  assertOk "nested elements work" nested

  -- Sanitization config tests
  putStrLn "\n[Sanitization]"
  let dangerousHtml = "<script>alert('xss')</script><p>Safe content</p>"
  case sanitize StrictConfig dangerousHtml of
    Ok clean => assertTrue "sanitize removes script tags" (not $ contains "<script>" clean)
    Err _ => putStrLn "  ✓ sanitize rejected dangerous HTML"

  assertTrue "isBlacklistedTag \"script\" = True" (isBlacklistedTag "script")
  assertTrue "isBlacklistedTag \"style\" = True" (isBlacklistedTag "style")
  assertTrue "isBlacklistedTag \"iframe\" = True" (isBlacklistedTag "iframe")
  assertTrue "isBlacklistedTag \"p\" = False" (not $ isBlacklistedTag "p")

  -- Event handler detection tests
  putStrLn "\n[Event Handler Detection]"
  assertTrue "isEventHandler \"onclick\" = True" (isEventHandler "onclick")
  assertTrue "isEventHandler \"onmouseover\" = True" (isEventHandler "onmouseover")
  assertTrue "isEventHandler \"onload\" = True" (isEventHandler "onload")
  assertTrue "isEventHandler \"class\" = False" (not $ isEventHandler "class")

  -- Trusted HTML tests
  putStrLn "\n[Trusted HTML]"
  let trusted = trustHtml "<p>Pre-sanitized content</p>"
  assertOk "trustHtml creates TrustedHtml" trusted

  let untrusted = "<script>evil()</script>"
  assertErr "untrusted content blocked without sanitization" (trustHtml untrusted)

  putStrLn "\n✓ SafeHtml unit tests complete"
