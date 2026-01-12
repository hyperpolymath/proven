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

suite "SafeUuid":
  test "parseUuid parses valid UUID":
    let uuid = parseUuid("550e8400-e29b-41d4-a716-446655440000")
    check uuid.isSome
    check uuid.get.version == uvV4
    check not uuid.get.isNil

  test "parseUuid returns none for invalid UUID":
    check parseUuid("not-a-uuid").isNone
    check parseUuid("").isNone
    check parseUuid("550e8400-e29b-41d4-a716").isNone

  test "nil UUID is detected":
    let uuid = parseUuid("00000000-0000-0000-0000-000000000000")
    check uuid.isSome
    check uuid.get.isNil

  test "UUID formats correctly":
    let uuid = parseUuid("550e8400-e29b-41d4-a716-446655440000")
    check $uuid.get == "550e8400-e29b-41d4-a716-446655440000"

  test "UUID URN format":
    let uuid = parseUuid("550e8400-e29b-41d4-a716-446655440000")
    check uuid.get.toUrn == "urn:uuid:550e8400-e29b-41d4-a716-446655440000"

  test "isValidUuid validates format":
    check isValidUuid("550e8400-e29b-41d4-a716-446655440000") == true
    check isValidUuid("invalid") == false

suite "SafeCurrency":
  test "fromMajor creates correct minor units":
    let money = fromMajor(100, ccUsd)
    check money.minorUnits == 10000  # 100 dollars = 10000 cents

  test "fromMinor preserves units":
    let money = fromMinor(12345, ccUsd)
    check money.minorUnits == 12345

  test "Money formats correctly":
    let money = fromMinor(12345, ccUsd)
    check $money == "$123.45"

  test "Money addition works":
    let a = fromMajor(100, ccUsd)
    let b = fromMajor(50, ccUsd)
    let sum = a + b
    check sum.minorUnits == 15000

  test "Currency mismatch detected":
    let usd = fromMajor(100, ccUsd)
    let eur = fromMajor(100, ccEur)
    check add(usd, eur).isNone

  test "Zero-decimal currencies handled":
    let yen = fromMajor(1000, ccJpy)
    check yen.minorUnits == 1000
    check $yen == "¥1000"

  test "Currency code parsing":
    check parseCurrencyCode("USD") == some(ccUsd)
    check parseCurrencyCode("eur") == some(ccEur)
    check parseCurrencyCode("XXX").isNone

suite "SafePhone":
  test "parsePhone parses valid phone numbers":
    let phone = parsePhone("+1 555 123 4567")
    check phone.isSome
    check phone.get.countryCode == ccUs
    check phone.get.nationalNumber == "5551234567"

  test "parsePhone returns none for invalid numbers":
    check parsePhone("123").isNone
    check parsePhone("").isNone

  test "E.164 formatting":
    let phone = parsePhone("+1 555 123 4567")
    check phone.get.toE164 == "+15551234567"

  test "International formatting":
    let phone = parsePhone("+1 555 123 4567")
    check phone.get.toInternational == "+1 555 123 4567"

  test "National formatting":
    let phone = parsePhone("+1 555 123 4567")
    check phone.get.toNational == "(555) 123-4567"

  test "isValidPhone validates numbers":
    check isValidPhone("+1 555 123 4567") == true
    check isValidPhone("abc") == false

suite "SafeHex":
  test "encode produces lowercase hex":
    check encode([0xFF'u8, 0x00, 0xAB]) == "ff00ab"

  test "encodeUpper produces uppercase hex":
    check encodeUpper([0xFF'u8, 0x00, 0xAB]) == "FF00AB"

  test "decode parses hex strings":
    let decoded = decode("ff00ab")
    check decoded.isSome
    check decoded.get == @[0xFF'u8, 0x00, 0xAB]

  test "decode returns none for invalid hex":
    check decode("xyz").isNone
    check decode("abc").isNone  # Odd length

  test "isValidHex validates hex strings":
    check isValidHex("abcdef0123456789") == true
    check isValidHex("xyz") == false

  test "isValidHexBytes checks even length":
    check isValidHexBytes("aabb") == true
    check isValidHexBytes("aab") == false

  test "constantTimeEqual compares case-insensitively":
    check constantTimeEqual("aabb", "AABB") == true
    check constantTimeEqual("aabb", "aab0") == false

  test "formatSpaced adds spaces":
    let formatted = formatSpaced("aabbcc")
    check formatted.isSome
    check formatted.get == "aa bb cc"

  test "formatColons adds colons":
    let formatted = formatColons("aabbcc")
    check formatted.isSome
    check formatted.get == "aa:bb:cc"

  test "intToHex converts integers":
    check intToHex(255, 2) == "ff"
    check intToHex(255, 4) == "00ff"

  test "hexToInt parses hex integers":
    check hexToInt("ff") == some(255'u64)
    check hexToInt("invalid").isNone
