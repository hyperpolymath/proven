// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue
import kotlin.test.assertFalse
import kotlin.test.assertNotNull

class ProvenTests {

    // SafeMath Tests

    @Test
    fun testSafeDiv() {
        assertEquals(5, SafeMath.div(10, 2))
        assertNull(SafeMath.div(10, 0))
    }

    @Test
    fun testSafeMod() {
        assertEquals(1, SafeMath.mod(10, 3))
        assertNull(SafeMath.mod(10, 0))
    }

    @Test
    fun testSafeAdd() {
        assertEquals(3, SafeMath.add(1, 2))
        assertNull(SafeMath.add(Int.MAX_VALUE, 1))
    }

    @Test
    fun testSafeSub() {
        assertEquals(2, SafeMath.sub(5, 3))
        assertNull(SafeMath.sub(Int.MIN_VALUE, 1))
    }

    @Test
    fun testSafeMul() {
        assertEquals(12, SafeMath.mul(3, 4))
        assertNull(SafeMath.mul(Int.MAX_VALUE, 2))
    }

    // SafeString Tests

    @Test
    fun testEscapeHtml() {
        assertEquals("&lt;script&gt;", SafeString.escapeHtml("<script>"))
        assertEquals("a &amp; b", SafeString.escapeHtml("a & b"))
        assertEquals("&quot;quoted&quot;", SafeString.escapeHtml("\"quoted\""))
    }

    @Test
    fun testEscapeSql() {
        assertEquals("it''s", SafeString.escapeSql("it's"))
    }

    @Test
    fun testEscapeJs() {
        assertEquals("line\\nbreak", SafeString.escapeJs("line\nbreak"))
        assertEquals("tab\\there", SafeString.escapeJs("tab\there"))
    }

    @Test
    fun testTruncateSafe() {
        assertEquals("he...", SafeString.truncateSafe("hello world", 5))
        assertEquals("hi", SafeString.truncateSafe("hi", 10))
    }

    // SafePath Tests

    @Test
    fun testHasTraversal() {
        assertTrue(SafePath.hasTraversal("../etc/passwd"))
        assertTrue(SafePath.hasTraversal("~/file"))
        assertFalse(SafePath.hasTraversal("normal/path"))
    }

    @Test
    fun testIsSafe() {
        assertTrue(SafePath.isSafe("safe/path"))
        assertFalse(SafePath.isSafe("../unsafe"))
    }

    @Test
    fun testSanitizeFilename() {
        assertEquals("file__name", SafePath.sanitizeFilename("file<>name"))
        assertEquals("__secret", SafePath.sanitizeFilename("..secret"))
    }

    @Test
    fun testSafeJoin() {
        assertEquals("/base/a/b", SafePath.safeJoin("/base", listOf("a", "b")))
        assertNull(SafePath.safeJoin("/base", listOf("../etc")))
    }

    // SafeEmail Tests

    @Test
    fun testIsValidEmail() {
        assertTrue(SafeEmail.isValid("user@example.com"))
        assertFalse(SafeEmail.isValid("not-an-email"))
        assertFalse(SafeEmail.isValid("@invalid.com"))
        assertFalse(SafeEmail.isValid("user@.com"))
    }

    @Test
    fun testSplitEmail() {
        val parts = SafeEmail.split("user@example.com")
        assertNotNull(parts)
        assertEquals("user", parts.localPart)
        assertEquals("example.com", parts.domain)
    }

    @Test
    fun testNormalizeEmail() {
        assertEquals("User@example.com", SafeEmail.normalize("User@EXAMPLE.COM"))
    }

    // SafeUrl Tests

    @Test
    fun testParseUrl() {
        val parsed = SafeUrl.parse("https://example.com:8080/path?query=1#frag")
        assertNotNull(parsed)
        assertEquals("https", parsed.scheme)
        assertEquals("example.com", parsed.host)
        assertEquals(8080, parsed.port)
        assertEquals("/path", parsed.path)
        assertEquals("query=1", parsed.query)
        assertEquals("frag", parsed.fragment)
    }

    @Test
    fun testIsValidUrl() {
        assertTrue(SafeUrl.isValid("https://example.com"))
        assertFalse(SafeUrl.isValid("not a url"))
    }

    @Test
    fun testIsHttps() {
        assertTrue(SafeUrl.isHttps("https://secure.com"))
        assertFalse(SafeUrl.isHttps("http://insecure.com"))
    }

    // SafeNetwork Tests

    @Test
    fun testIsValidIPv4() {
        assertTrue(SafeNetwork.isValidIPv4("192.168.1.1"))
        assertFalse(SafeNetwork.isValidIPv4("invalid"))
        assertFalse(SafeNetwork.isValidIPv4("256.1.1.1"))
    }

    @Test
    fun testIsPrivate() {
        assertTrue(SafeNetwork.isPrivate("192.168.1.1"))
        assertTrue(SafeNetwork.isPrivate("10.0.0.1"))
        assertTrue(SafeNetwork.isPrivate("172.16.0.1"))
        assertFalse(SafeNetwork.isPrivate("8.8.8.8"))
    }

    @Test
    fun testIsLoopback() {
        assertTrue(SafeNetwork.isLoopback("127.0.0.1"))
        assertFalse(SafeNetwork.isLoopback("192.168.1.1"))
    }

    @Test
    fun testIsPublic() {
        assertTrue(SafeNetwork.isPublic("8.8.8.8"))
        assertFalse(SafeNetwork.isPublic("192.168.1.1"))
    }

    // SafeCrypto Tests

    @Test
    fun testConstantTimeCompare() {
        assertTrue(SafeCrypto.constantTimeCompare("secret", "secret"))
        assertFalse(SafeCrypto.constantTimeCompare("secret", "other"))
        assertTrue(SafeCrypto.constantTimeCompare("", ""))
    }

    @Test
    fun testSecureZero() {
        val data = byteArrayOf(1, 2, 3, 4)
        SafeCrypto.secureZero(data)
        assertTrue(data.all { it == 0.toByte() })
    }
}
