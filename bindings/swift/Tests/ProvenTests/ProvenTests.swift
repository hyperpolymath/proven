// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import XCTest
@testable import Proven

final class ProvenTests: XCTestCase {

    // MARK: - SafeMath Tests

    func testSafeDiv() {
        XCTAssertEqual(SafeMath.div(10, 2), 5)
        XCTAssertNil(SafeMath.div(10, 0))
    }

    func testSafeMod() {
        XCTAssertEqual(SafeMath.mod(10, 3), 1)
        XCTAssertNil(SafeMath.mod(10, 0))
    }

    func testSafeAdd() {
        XCTAssertEqual(SafeMath.add(1, 2), 3)
        XCTAssertNil(SafeMath.add(Int.max, 1))
    }

    func testSafeSub() {
        XCTAssertEqual(SafeMath.sub(5, 3), 2)
        XCTAssertNil(SafeMath.sub(Int.min, 1))
    }

    func testSafeMul() {
        XCTAssertEqual(SafeMath.mul(3, 4), 12)
        XCTAssertNil(SafeMath.mul(Int.max, 2))
    }

    // MARK: - SafeString Tests

    func testEscapeHtml() {
        XCTAssertEqual(SafeString.escapeHtml("<script>"), "&lt;script&gt;")
        XCTAssertEqual(SafeString.escapeHtml("a & b"), "a &amp; b")
        XCTAssertEqual(SafeString.escapeHtml("\"quoted\""), "&quot;quoted&quot;")
    }

    func testEscapeSql() {
        XCTAssertEqual(SafeString.escapeSql("it's"), "it''s")
    }

    func testEscapeJs() {
        XCTAssertEqual(SafeString.escapeJs("line\nbreak"), "line\\nbreak")
        XCTAssertEqual(SafeString.escapeJs("tab\there"), "tab\\there")
    }

    func testTruncateSafe() {
        XCTAssertEqual(SafeString.truncateSafe("hello world", maxLength: 5), "he...")
        XCTAssertEqual(SafeString.truncateSafe("hi", maxLength: 10), "hi")
    }

    // MARK: - SafePath Tests

    func testHasTraversal() {
        XCTAssertTrue(SafePath.hasTraversal("../etc/passwd"))
        XCTAssertTrue(SafePath.hasTraversal("~/file"))
        XCTAssertFalse(SafePath.hasTraversal("normal/path"))
    }

    func testIsSafe() {
        XCTAssertTrue(SafePath.isSafe("safe/path"))
        XCTAssertFalse(SafePath.isSafe("../unsafe"))
    }

    func testSanitizeFilename() {
        XCTAssertEqual(SafePath.sanitizeFilename("file<>name"), "file__name")
        XCTAssertEqual(SafePath.sanitizeFilename("..secret"), "__secret")
    }

    func testSafeJoin() {
        XCTAssertEqual(SafePath.safeJoin(base: "/base", parts: ["a", "b"]), "/base/a/b")
        XCTAssertNil(SafePath.safeJoin(base: "/base", parts: ["../etc"]))
    }

    // MARK: - SafeEmail Tests

    func testIsValidEmail() {
        XCTAssertTrue(SafeEmail.isValid("user@example.com"))
        XCTAssertFalse(SafeEmail.isValid("not-an-email"))
        XCTAssertFalse(SafeEmail.isValid("@invalid.com"))
        XCTAssertFalse(SafeEmail.isValid("user@.com"))
    }

    func testSplitEmail() {
        let parts = SafeEmail.split("user@example.com")
        XCTAssertNotNil(parts)
        XCTAssertEqual(parts?.localPart, "user")
        XCTAssertEqual(parts?.domain, "example.com")
    }

    func testNormalizeEmail() {
        XCTAssertEqual(SafeEmail.normalize("User@EXAMPLE.COM"), "User@example.com")
    }

    // MARK: - SafeUrl Tests

    func testParseUrl() {
        let parsed = SafeUrl.parse("https://example.com:8080/path?query=1#frag")
        XCTAssertNotNil(parsed)
        XCTAssertEqual(parsed?.scheme, "https")
        XCTAssertEqual(parsed?.host, "example.com")
        XCTAssertEqual(parsed?.port, 8080)
        XCTAssertEqual(parsed?.path, "/path")
        XCTAssertEqual(parsed?.query, "query=1")
        XCTAssertEqual(parsed?.fragment, "frag")
    }

    func testIsValidUrl() {
        XCTAssertTrue(SafeUrl.isValid("https://example.com"))
        XCTAssertFalse(SafeUrl.isValid("not a url"))
    }

    func testIsHttps() {
        XCTAssertTrue(SafeUrl.isHttps("https://secure.com"))
        XCTAssertFalse(SafeUrl.isHttps("http://insecure.com"))
    }

    // MARK: - SafeNetwork Tests

    func testIsValidIPv4() {
        XCTAssertTrue(SafeNetwork.isValidIPv4("192.168.1.1"))
        XCTAssertFalse(SafeNetwork.isValidIPv4("invalid"))
        XCTAssertFalse(SafeNetwork.isValidIPv4("256.1.1.1"))
    }

    func testIsPrivate() {
        XCTAssertTrue(SafeNetwork.isPrivate("192.168.1.1"))
        XCTAssertTrue(SafeNetwork.isPrivate("10.0.0.1"))
        XCTAssertTrue(SafeNetwork.isPrivate("172.16.0.1"))
        XCTAssertFalse(SafeNetwork.isPrivate("8.8.8.8"))
    }

    func testIsLoopback() {
        XCTAssertTrue(SafeNetwork.isLoopback("127.0.0.1"))
        XCTAssertFalse(SafeNetwork.isLoopback("192.168.1.1"))
    }

    func testIsPublic() {
        XCTAssertTrue(SafeNetwork.isPublic("8.8.8.8"))
        XCTAssertFalse(SafeNetwork.isPublic("192.168.1.1"))
    }

    // MARK: - SafeCrypto Tests

    func testConstantTimeCompare() {
        XCTAssertTrue(SafeCrypto.constantTimeCompare("secret", "secret"))
        XCTAssertFalse(SafeCrypto.constantTimeCompare("secret", "other"))
        XCTAssertTrue(SafeCrypto.constantTimeCompare("", ""))
    }

    func testSecureZero() {
        var data: [UInt8] = [1, 2, 3, 4]
        SafeCrypto.secureZero(&data)
        XCTAssertTrue(data.allSatisfy { $0 == 0 })
    }
}
