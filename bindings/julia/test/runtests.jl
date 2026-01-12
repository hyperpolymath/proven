# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

using Test
using Proven
using Proven.SafeMath
using Proven.SafeString
using Proven.SafePath
using Proven.SafeEmail
using Proven.SafeUrl
using Proven.SafeNetwork
using Proven.SafeCrypto

@testset "Proven.jl" begin

    @testset "SafeMath" begin
        @test safe_div(10, 2) == 5
        @test safe_div(10, 0) === nothing

        @test safe_mod(10, 3) == 1
        @test safe_mod(10, 0) === nothing

        @test safe_add(1, 2) == 3
        @test safe_sub(5, 3) == 2
        @test safe_mul(3, 4) == 12

        # Overflow detection
        @test safe_add(typemax(Int64), Int64(1)) === nothing
        @test safe_mul(typemax(Int64), Int64(2)) === nothing
    end

    @testset "SafeString" begin
        @test escape_html("<script>") == "&lt;script&gt;"
        @test escape_html("a & b") == "a &amp; b"
        @test escape_html("\"quoted\"") == "&quot;quoted&quot;"

        @test escape_sql("it's") == "it''s"

        @test escape_js("line\nbreak") == "line\\nbreak"
        @test escape_js("tab\there") == "tab\\there"

        @test truncate_safe("hello world", 5) == "he..."
        @test truncate_safe("hi", 10) == "hi"
    end

    @testset "SafePath" begin
        @test has_traversal("../etc/passwd") == true
        @test has_traversal("~/file") == true
        @test has_traversal("normal/path") == false

        @test is_safe("safe/path") == true
        @test is_safe("../unsafe") == false

        @test sanitize_filename("file<>name") == "file__name"
        @test sanitize_filename("..secret") == "__secret"

        @test safe_join("/base", ["a", "b"]) == "/base/a/b"
        @test safe_join("/base", ["../etc"]) === nothing
    end

    @testset "SafeEmail" begin
        @test is_valid("user@example.com") == true
        @test is_valid("not-an-email") == false
        @test is_valid("@invalid.com") == false
        @test is_valid("user@.com") == false

        parts = split_email("user@example.com")
        @test parts !== nothing
        @test parts.local_part == "user"
        @test parts.domain == "example.com"

        @test get_domain("user@example.com") == "example.com"
        @test get_local_part("user@example.com") == "user"
        @test normalize("User@EXAMPLE.COM") == "User@example.com"
    end

    @testset "SafeUrl" begin
        parsed = parse_url("https://example.com:8080/path?query=1#frag")
        @test parsed !== nothing
        @test parsed.scheme == "https"
        @test parsed.host == "example.com"
        @test parsed.port == 8080
        @test parsed.path == "/path"
        @test parsed.query == "query=1"
        @test parsed.fragment == "frag"

        @test is_valid_url("https://example.com") == true
        @test is_valid_url("not a url") == false

        @test get_host("https://example.com/path") == "example.com"
        @test is_https("https://secure.com") == true
        @test is_https("http://insecure.com") == false
    end

    @testset "SafeNetwork" begin
        @test is_valid_ipv4("192.168.1.1") == true
        @test is_valid_ipv4("invalid") == false
        @test is_valid_ipv4("256.1.1.1") == false

        @test is_private("192.168.1.1") == true
        @test is_private("10.0.0.1") == true
        @test is_private("172.16.0.1") == true
        @test is_private("8.8.8.8") == false

        @test is_loopback("127.0.0.1") == true
        @test is_loopback("192.168.1.1") == false

        @test is_public("8.8.8.8") == true
        @test is_public("192.168.1.1") == false

        ip = parse_ipv4("192.168.1.1")
        @test format_ipv4(ip) == "192.168.1.1"
    end

    @testset "SafeCrypto" begin
        @test constant_time_compare("secret", "secret") == true
        @test constant_time_compare("secret", "other") == false
        @test constant_time_compare("", "") == true

        data = UInt8[1, 2, 3, 4]
        secure_zero!(data)
        @test all(==(0x00), data)
    end

end
