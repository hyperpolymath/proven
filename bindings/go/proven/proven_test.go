// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
	"testing"
)

// SafeMath Tests

func TestSafeDiv(t *testing.T) {
	if result, ok := SafeDiv(10, 2); !ok || result != 5 {
		t.Errorf("SafeDiv(10, 2) = %d, %v; want 5, true", result, ok)
	}
	if _, ok := SafeDiv(10, 0); ok {
		t.Error("SafeDiv(10, 0) should return ok=false")
	}
}

func TestSafeMod(t *testing.T) {
	if result, ok := SafeMod(10, 3); !ok || result != 1 {
		t.Errorf("SafeMod(10, 3) = %d, %v; want 1, true", result, ok)
	}
	if _, ok := SafeMod(10, 0); ok {
		t.Error("SafeMod(10, 0) should return ok=false")
	}
}

func TestSafeAdd(t *testing.T) {
	if result, ok := SafeAdd(1, 2); !ok || result != 3 {
		t.Errorf("SafeAdd(1, 2) = %d, %v; want 3, true", result, ok)
	}
	if _, ok := SafeAdd(math.MaxInt64, 1); ok {
		t.Error("SafeAdd(MaxInt64, 1) should return ok=false (overflow)")
	}
}

func TestSafeSub(t *testing.T) {
	if result, ok := SafeSub(5, 3); !ok || result != 2 {
		t.Errorf("SafeSub(5, 3) = %d, %v; want 2, true", result, ok)
	}
	if _, ok := SafeSub(math.MinInt64, 1); ok {
		t.Error("SafeSub(MinInt64, 1) should return ok=false (overflow)")
	}
}

func TestSafeMul(t *testing.T) {
	if result, ok := SafeMul(3, 4); !ok || result != 12 {
		t.Errorf("SafeMul(3, 4) = %d, %v; want 12, true", result, ok)
	}
	if _, ok := SafeMul(math.MaxInt64, 2); ok {
		t.Error("SafeMul(MaxInt64, 2) should return ok=false (overflow)")
	}
}

// SafeString Tests

func TestEscapeHTML(t *testing.T) {
	if got := EscapeHTML("<script>"); got != "&lt;script&gt;" {
		t.Errorf("EscapeHTML(<script>) = %s; want &lt;script&gt;", got)
	}
	if got := EscapeHTML("a & b"); got != "a &amp; b" {
		t.Errorf("EscapeHTML(a & b) = %s; want a &amp; b", got)
	}
}

func TestEscapeSQL(t *testing.T) {
	if got := EscapeSQL("it's"); got != "it''s" {
		t.Errorf("EscapeSQL(it's) = %s; want it''s", got)
	}
}

func TestEscapeJS(t *testing.T) {
	if got := EscapeJS("line\nbreak"); got != "line\\nbreak" {
		t.Errorf("EscapeJS(line\\nbreak) = %s; want line\\nbreak", got)
	}
}

func TestTruncateSafe(t *testing.T) {
	if got := TruncateSafeDefault("hello world", 5); got != "he..." {
		t.Errorf("TruncateSafe(hello world, 5) = %s; want he...", got)
	}
	if got := TruncateSafeDefault("hi", 10); got != "hi" {
		t.Errorf("TruncateSafe(hi, 10) = %s; want hi", got)
	}
}

// SafePath Tests

func TestHasTraversal(t *testing.T) {
	if !HasTraversal("../etc/passwd") {
		t.Error("HasTraversal(../etc/passwd) should be true")
	}
	if !HasTraversal("~/file") {
		t.Error("HasTraversal(~/file) should be true")
	}
	if HasTraversal("normal/path") {
		t.Error("HasTraversal(normal/path) should be false")
	}
}

func TestIsSafe(t *testing.T) {
	if !IsSafe("safe/path") {
		t.Error("IsSafe(safe/path) should be true")
	}
	if IsSafe("../unsafe") {
		t.Error("IsSafe(../unsafe) should be false")
	}
}

func TestSafeJoin(t *testing.T) {
	if result, ok := SafeJoin("/base", "a", "b"); !ok || result != "/base/a/b" {
		t.Errorf("SafeJoin(/base, a, b) = %s, %v; want /base/a/b, true", result, ok)
	}
	if _, ok := SafeJoin("/base", "../etc"); ok {
		t.Error("SafeJoin(/base, ../etc) should return ok=false")
	}
}

// SafeEmail Tests

func TestIsValidEmail(t *testing.T) {
	if !IsValidEmail("user@example.com") {
		t.Error("IsValidEmail(user@example.com) should be true")
	}
	if IsValidEmail("not-an-email") {
		t.Error("IsValidEmail(not-an-email) should be false")
	}
	if IsValidEmail("@invalid.com") {
		t.Error("IsValidEmail(@invalid.com) should be false")
	}
}

func TestSplitEmail(t *testing.T) {
	parts := SplitEmail("user@example.com")
	if parts == nil {
		t.Fatal("SplitEmail(user@example.com) should not be nil")
	}
	if parts.LocalPart != "user" {
		t.Errorf("LocalPart = %s; want user", parts.LocalPart)
	}
	if parts.Domain != "example.com" {
		t.Errorf("Domain = %s; want example.com", parts.Domain)
	}
}

func TestNormalizeEmail(t *testing.T) {
	if result, ok := NormalizeEmail("User@EXAMPLE.COM"); !ok || result != "User@example.com" {
		t.Errorf("NormalizeEmail(User@EXAMPLE.COM) = %s, %v; want User@example.com, true", result, ok)
	}
}

// SafeUrl Tests

func TestParseURL(t *testing.T) {
	parsed := ParseURL("https://example.com:8080/path?query=1#frag")
	if parsed == nil {
		t.Fatal("ParseURL should not be nil")
	}
	if parsed.Scheme != "https" {
		t.Errorf("Scheme = %s; want https", parsed.Scheme)
	}
	if parsed.Host != "example.com" {
		t.Errorf("Host = %s; want example.com", parsed.Host)
	}
	if parsed.Port != "8080" {
		t.Errorf("Port = %s; want 8080", parsed.Port)
	}
}

func TestIsValidURL(t *testing.T) {
	if !IsValidURL("https://example.com") {
		t.Error("IsValidURL(https://example.com) should be true")
	}
	if IsValidURL("not a url") {
		t.Error("IsValidURL(not a url) should be false")
	}
}

func TestIsHTTPS(t *testing.T) {
	if !IsHTTPS("https://secure.com") {
		t.Error("IsHTTPS(https://secure.com) should be true")
	}
	if IsHTTPS("http://insecure.com") {
		t.Error("IsHTTPS(http://insecure.com) should be false")
	}
}

// SafeNetwork Tests

func TestIsValidIPv4(t *testing.T) {
	if !IsValidIPv4("192.168.1.1") {
		t.Error("IsValidIPv4(192.168.1.1) should be true")
	}
	if IsValidIPv4("invalid") {
		t.Error("IsValidIPv4(invalid) should be false")
	}
	if IsValidIPv4("256.1.1.1") {
		t.Error("IsValidIPv4(256.1.1.1) should be false")
	}
}

func TestIsPrivate(t *testing.T) {
	if !IsPrivate("192.168.1.1") {
		t.Error("IsPrivate(192.168.1.1) should be true")
	}
	if !IsPrivate("10.0.0.1") {
		t.Error("IsPrivate(10.0.0.1) should be true")
	}
	if IsPrivate("8.8.8.8") {
		t.Error("IsPrivate(8.8.8.8) should be false")
	}
}

func TestIsLoopback(t *testing.T) {
	if !IsLoopback("127.0.0.1") {
		t.Error("IsLoopback(127.0.0.1) should be true")
	}
	if IsLoopback("192.168.1.1") {
		t.Error("IsLoopback(192.168.1.1) should be false")
	}
}

func TestIsPublic(t *testing.T) {
	if !IsPublic("8.8.8.8") {
		t.Error("IsPublic(8.8.8.8) should be true")
	}
	if IsPublic("192.168.1.1") {
		t.Error("IsPublic(192.168.1.1) should be false")
	}
}

// SafeCrypto Tests

func TestConstantTimeCompare(t *testing.T) {
	if !ConstantTimeCompareString("secret", "secret") {
		t.Error("ConstantTimeCompare(secret, secret) should be true")
	}
	if ConstantTimeCompareString("secret", "other") {
		t.Error("ConstantTimeCompare(secret, other) should be false")
	}
	if !ConstantTimeCompareString("", "") {
		t.Error("ConstantTimeCompare(empty, empty) should be true")
	}
}

func TestSecureZero(t *testing.T) {
	data := []byte{1, 2, 3, 4}
	SecureZero(data)
	for i, b := range data {
		if b != 0 {
			t.Errorf("data[%d] = %d; want 0", i, b)
		}
	}
}
