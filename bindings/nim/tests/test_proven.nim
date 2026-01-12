# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

import std/[unittest, options]
import proven

suite "SafeMath":
  test "safeDiv divides correctly":
    check safeDiv(10, 2) == some(5'i64)

  test "safeDiv returns none on division by zero":
    check safeDiv(10, 0).isNone

  test "safeMod computes modulo correctly":
    check safeMod(10, 3) == some(1'i64)

  test "safeMod returns none on mod by zero":
    check safeMod(10, 0).isNone

  test "safeAdd adds correctly":
    check safeAdd(1, 2) == some(3'i64)

  test "safeSub subtracts correctly":
    check safeSub(5, 3) == some(2'i64)

  test "safeMul multiplies correctly":
    check safeMul(3, 4) == some(12'i64)

suite "SafeString":
  test "escapeHtml escapes HTML entities":
    check escapeHtml("<script>") == "&lt;script&gt;"
    check escapeHtml("a & b") == "a &amp; b"
    check escapeHtml("\"quoted\"") == "&quot;quoted&quot;"

  test "escapeSql escapes single quotes":
    check escapeSql("it's") == "it''s"

  test "escapeJs escapes JavaScript characters":
    check escapeJs("line\nbreak") == "line\\nbreak"
    check escapeJs("tab\there") == "tab\\there"

  test "truncateSafe truncates with suffix":
    check truncateSafe("hello world", 5) == "he..."
    check truncateSafe("hi", 10) == "hi"

suite "SafePath":
  test "hasTraversal detects traversal sequences":
    check hasTraversal("../etc/passwd") == true
    check hasTraversal("~/file") == true
    check hasTraversal("normal/path") == false

  test "isSafe validates safe paths":
    check isSafe("safe/path") == true
    check isSafe("../unsafe") == false

  test "sanitizeFilename removes dangerous characters":
    check sanitizeFilename("file<>name") == "file__name"
    check sanitizeFilename("..secret") == "__secret"

  test "safeJoin joins paths safely":
    check safeJoin("/base", "a", "b") == some("/base/a/b")
    check safeJoin("/base", "../etc").isNone

suite "SafeEmail":
  test "isValid validates email addresses":
    check isValid("user@example.com") == true
    check isValid("not-an-email") == false
    check isValid("@invalid.com") == false
    check isValid("user@.com") == false

  test "splitEmail parses email parts":
    let parts = splitEmail("user@example.com")
    check parts.isSome
    check parts.get.localPart == "user"
    check parts.get.domain == "example.com"

  test "normalize lowercases domain":
    check normalize("User@EXAMPLE.COM") == some("User@example.com")

suite "SafeNetwork":
  test "isValidIPv4 validates IPv4 addresses":
    check isValidIPv4("192.168.1.1") == true
    check isValidIPv4("invalid") == false
    check isValidIPv4("256.1.1.1") == false

  test "isPrivate detects private addresses":
    check isPrivate("192.168.1.1") == true
    check isPrivate("10.0.0.1") == true
    check isPrivate("172.16.0.1") == true
    check isPrivate("8.8.8.8") == false

  test "isLoopback detects loopback addresses":
    check isLoopback("127.0.0.1") == true
    check isLoopback("192.168.1.1") == false

  test "isPublic detects public addresses":
    check isPublic("8.8.8.8") == true
    check isPublic("192.168.1.1") == false

suite "SafeCrypto":
  test "constantTimeCompare compares strings correctly":
    check constantTimeCompare("secret", "secret") == true
    check constantTimeCompare("secret", "other!") == false
    check constantTimeCompare("", "") == true

  test "secureZero creates zeroed string":
    let zeroed = secureZero(4)
    check zeroed.len == 4
    check zeroed == "\x00\x00\x00\x00"
