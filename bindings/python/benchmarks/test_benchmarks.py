# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
Pytest-benchmark tests for Proven Python bindings.

Run with: pytest benchmarks/test_benchmarks.py --benchmark-only
"""

import pytest
from proven import (
    safe_add, safe_mul, safe_div,
    truncate, sanitize, validate_length, validate_utf8,
    sha256, sha512, hmac_sha256, random_bytes, constant_time_eq,
    parse_json, stringify,
    parse_ipv4, parse_ipv6, parse_cidr, validate_port, is_private_ip, IPv4,
    parse_url, encode_query_value,
    validate_email,
    parse_uuid, generate_uuid_v4,
)


# SafeMath benchmarks
class TestSafeMathBenchmarks:
    def test_safe_add(self, benchmark):
        benchmark(safe_add, 1000, 2000)

    def test_safe_mul(self, benchmark):
        benchmark(safe_mul, 100, 200)

    def test_safe_div(self, benchmark):
        benchmark(safe_div, 1000, 7)

    def test_overflow_check(self, benchmark):
        import sys
        benchmark(safe_add, sys.maxsize, 1)


# SafeString benchmarks
class TestSafeStringBenchmarks:
    TEST_STRING = "The quick brown fox jumps over the lazy dog"

    def test_truncate(self, benchmark):
        benchmark(truncate, self.TEST_STRING, 20)

    def test_sanitize(self, benchmark):
        benchmark(sanitize, "<script>alert('xss')</script>")

    def test_validate_length(self, benchmark):
        benchmark(validate_length, self.TEST_STRING, 10, 100)

    def test_validate_utf8(self, benchmark):
        benchmark(validate_utf8, self.TEST_STRING)


# SafeCrypto benchmarks
class TestSafeCryptoBenchmarks:
    TEST_DATA = "benchmark test data for cryptographic operations"

    def test_sha256(self, benchmark):
        benchmark(sha256, self.TEST_DATA)

    def test_sha512(self, benchmark):
        benchmark(sha512, self.TEST_DATA)

    def test_hmac_sha256(self, benchmark):
        benchmark(hmac_sha256, "secret-key", self.TEST_DATA)

    def test_random_bytes_32(self, benchmark):
        benchmark(random_bytes, 32)

    def test_constant_time_eq(self, benchmark):
        hash_value = sha256(self.TEST_DATA)
        benchmark(constant_time_eq, hash_value, hash_value)


# SafeJson benchmarks
class TestSafeJsonBenchmarks:
    SIMPLE_JSON = '{"name":"test","value":42}'
    NESTED_JSON = '{"user":{"name":"John","age":30},"items":[1,2,3]}'

    def test_parse_simple(self, benchmark):
        benchmark(parse_json, self.SIMPLE_JSON)

    def test_parse_nested(self, benchmark):
        benchmark(parse_json, self.NESTED_JSON)

    def test_stringify(self, benchmark):
        obj = {"name": "test", "value": 42}
        benchmark(stringify, obj)


# SafeNetwork benchmarks
class TestSafeNetworkBenchmarks:
    def test_parse_ipv4(self, benchmark):
        benchmark(parse_ipv4, "192.168.1.1")

    def test_parse_ipv6(self, benchmark):
        benchmark(parse_ipv6, "2001:0db8:85a3:0000:0000:8a2e:0370:7334")

    def test_parse_cidr(self, benchmark):
        benchmark(parse_cidr, "10.0.0.0/8")

    def test_validate_port(self, benchmark):
        benchmark(validate_port, 8080)

    def test_is_private_ip(self, benchmark):
        ip = IPv4(192, 168, 1, 1)
        benchmark(is_private_ip, ip)


# SafeUrl benchmarks
class TestSafeUrlBenchmarks:
    def test_parse_url(self, benchmark):
        benchmark(parse_url, "https://example.com/path?query=value")

    def test_encode_query_value(self, benchmark):
        benchmark(encode_query_value, "hello world & more")


# SafeEmail benchmarks
class TestSafeEmailBenchmarks:
    def test_validate_email(self, benchmark):
        benchmark(validate_email, "user+tag@mail.example.com")


# SafeUUID benchmarks
class TestSafeUUIDBenchmarks:
    def test_parse_uuid(self, benchmark):
        benchmark(parse_uuid, "550e8400-e29b-41d4-a716-446655440000")

    def test_generate_uuid_v4(self, benchmark):
        benchmark(generate_uuid_v4)
