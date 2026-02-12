-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--- Test suite for Proven Lua bindings
--- Run with: busted spec/

local proven = require("proven")
local safe_math = proven.safe_math
local safe_string = proven.safe_string
local safe_path = proven.safe_path
local safe_email = proven.safe_email
local safe_network = proven.safe_network
local safe_crypto = proven.safe_crypto

describe("SafeMath", function()
    it("divides correctly", function()
        assert.are.equal(5, safe_math.div(10, 2))
    end)

    it("returns nil on division by zero", function()
        assert.is_nil(safe_math.div(10, 0))
    end)

    it("computes modulo correctly", function()
        assert.are.equal(1, safe_math.mod(10, 3))
    end)

    it("returns nil on mod by zero", function()
        assert.is_nil(safe_math.mod(10, 0))
    end)

    it("adds correctly", function()
        assert.are.equal(3, safe_math.add(1, 2))
    end)

    it("subtracts correctly", function()
        assert.are.equal(2, safe_math.sub(5, 3))
    end)

    it("multiplies correctly", function()
        assert.are.equal(12, safe_math.mul(3, 4))
    end)
end)

describe("SafeString", function()
    it("escapes HTML entities", function()
        assert.are.equal("&lt;script&gt;", safe_string.escape_html("<script>"))
        assert.are.equal("a &amp; b", safe_string.escape_html("a & b"))
        assert.are.equal("&quot;quoted&quot;", safe_string.escape_html('"quoted"'))
    end)

    it("escapes SQL single quotes", function()
        assert.are.equal("it''s", safe_string.escape_sql("it's"))
    end)

    it("escapes JavaScript special characters", function()
        assert.are.equal("line\\nbreak", safe_string.escape_js("line\nbreak"))
        assert.are.equal("tab\\there", safe_string.escape_js("tab\there"))
    end)

    it("truncates with suffix", function()
        assert.are.equal("he...", safe_string.truncate_safe("hello world", 5))
        assert.are.equal("hi", safe_string.truncate_safe("hi", 10))
    end)
end)

describe("SafePath", function()
    it("detects traversal sequences", function()
        assert.is_true(safe_path.has_traversal("../etc/passwd"))
        assert.is_true(safe_path.has_traversal("~/file"))
        assert.is_false(safe_path.has_traversal("normal/path"))
    end)

    it("validates safe paths", function()
        assert.is_true(safe_path.is_safe("safe/path"))
        assert.is_false(safe_path.is_safe("../unsafe"))
    end)

    it("sanitizes filenames", function()
        assert.are.equal("file__name", safe_path.sanitize_filename("file<>name"))
        assert.are.equal("__secret", safe_path.sanitize_filename("..secret"))
    end)

    it("joins paths safely", function()
        assert.are.equal("/base/a/b", safe_path.safe_join("/base", {"a", "b"}))
        assert.is_nil(safe_path.safe_join("/base", {"../etc"}))
    end)
end)

describe("SafeEmail", function()
    it("validates email addresses", function()
        assert.is_true(safe_email.is_valid("user@example.com"))
        assert.is_false(safe_email.is_valid("not-an-email"))
        assert.is_false(safe_email.is_valid("@invalid.com"))
        assert.is_false(safe_email.is_valid("user@.com"))
    end)

    it("splits email parts", function()
        local parts = safe_email.split("user@example.com")
        assert.are.equal("user", parts.local_part)
        assert.are.equal("example.com", parts.domain)
    end)

    it("normalizes email addresses", function()
        assert.are.equal("User@example.com", safe_email.normalize("User@EXAMPLE.COM"))
    end)
end)

describe("SafeNetwork", function()
    it("validates IPv4 addresses", function()
        assert.is_true(safe_network.is_valid_ipv4("192.168.1.1"))
        assert.is_false(safe_network.is_valid_ipv4("invalid"))
        assert.is_false(safe_network.is_valid_ipv4("256.1.1.1"))
    end)

    it("detects private addresses", function()
        assert.is_true(safe_network.is_private("192.168.1.1"))
        assert.is_true(safe_network.is_private("10.0.0.1"))
        assert.is_true(safe_network.is_private("172.16.0.1"))
        assert.is_false(safe_network.is_private("8.8.8.8"))
    end)

    it("detects loopback addresses", function()
        assert.is_true(safe_network.is_loopback("127.0.0.1"))
        assert.is_false(safe_network.is_loopback("192.168.1.1"))
    end)

    it("detects public addresses", function()
        assert.is_true(safe_network.is_public("8.8.8.8"))
        assert.is_false(safe_network.is_public("192.168.1.1"))
    end)
end)

describe("SafeCrypto", function()
    it("compares strings in constant time", function()
        assert.is_true(safe_crypto.constant_time_compare("secret", "secret"))
        assert.is_false(safe_crypto.constant_time_compare("secret", "other!"))
        assert.is_true(safe_crypto.constant_time_compare("", ""))
    end)

    it("creates zeroed strings", function()
        local zeroed = safe_crypto.secure_zero(4)
        assert.are.equal(4, #zeroed)
        assert.are.equal("\0\0\0\0", zeroed)
    end)
end)
