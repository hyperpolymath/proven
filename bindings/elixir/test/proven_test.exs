# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

defmodule ProvenTest do
  use ExUnit.Case
  doctest Proven

  alias Proven.SafeMath
  alias Proven.SafeString
  alias Proven.SafePath
  alias Proven.SafeEmail
  alias Proven.SafeUrl
  alias Proven.SafeNetwork
  alias Proven.SafeCrypto

  # SafeMath Tests

  test "safe_div divides correctly" do
    assert SafeMath.safe_div(10, 2) == {:ok, 5}
  end

  test "safe_div returns error on division by zero" do
    assert SafeMath.safe_div(10, 0) == {:error, :division_by_zero}
  end

  test "safe_mod computes modulo correctly" do
    assert SafeMath.safe_mod(10, 3) == {:ok, 1}
  end

  test "safe_mod returns error on division by zero" do
    assert SafeMath.safe_mod(10, 0) == {:error, :division_by_zero}
  end

  test "safe_add adds correctly" do
    assert SafeMath.safe_add(1, 2) == {:ok, 3}
  end

  # SafeString Tests

  test "escape_html escapes HTML entities" do
    assert SafeString.escape_html("<script>") == "&lt;script&gt;"
    assert SafeString.escape_html("a & b") == "a &amp; b"
    assert SafeString.escape_html("\"quoted\"") == "&quot;quoted&quot;"
  end

  test "escape_sql escapes single quotes" do
    assert SafeString.escape_sql("it's") == "it''s"
  end

  test "escape_js escapes JavaScript special characters" do
    assert SafeString.escape_js("line\nbreak") == "line\\nbreak"
    assert SafeString.escape_js("tab\there") == "tab\\there"
  end

  test "truncate_safe truncates with suffix" do
    assert SafeString.truncate_safe("hello world", 5) == "he..."
    assert SafeString.truncate_safe("hi", 10) == "hi"
  end

  # SafePath Tests

  test "has_traversal? detects traversal sequences" do
    assert SafePath.has_traversal?("../etc/passwd") == true
    assert SafePath.has_traversal?("~/file") == true
    assert SafePath.has_traversal?("normal/path") == false
  end

  test "safe? validates safe paths" do
    assert SafePath.safe?("safe/path") == true
    assert SafePath.safe?("../unsafe") == false
  end

  test "sanitize_filename removes dangerous characters" do
    assert SafePath.sanitize_filename("file<>name") == "file__name"
    assert SafePath.sanitize_filename("..secret") == "__secret"
  end

  test "safe_join joins paths safely" do
    assert SafePath.safe_join("/base", ["a", "b"]) == {:ok, "/base/a/b"}
    assert SafePath.safe_join("/base", ["../etc"]) == {:error, :traversal_detected}
  end

  # SafeEmail Tests

  test "valid? validates email addresses" do
    assert SafeEmail.valid?("user@example.com") == true
    assert SafeEmail.valid?("not-an-email") == false
    assert SafeEmail.valid?("@invalid.com") == false
    assert SafeEmail.valid?("user@.com") == false
  end

  test "split parses email parts" do
    {:ok, parts} = SafeEmail.split("user@example.com")
    assert parts.local_part == "user"
    assert parts.domain == "example.com"
  end

  test "normalize lowercases domain" do
    assert SafeEmail.normalize("User@EXAMPLE.COM") == {:ok, "User@example.com"}
  end

  # SafeUrl Tests

  test "parse parses URL components" do
    {:ok, parsed} = SafeUrl.parse("https://example.com/path")
    assert parsed.scheme == "https"
    assert parsed.host == "example.com"
    assert parsed.path == "/path"
  end

  test "valid? validates URLs" do
    assert SafeUrl.valid?("https://example.com") == true
    assert SafeUrl.valid?("not a url") == false
  end

  test "https? checks for HTTPS" do
    assert SafeUrl.https?("https://secure.com") == true
    assert SafeUrl.https?("http://insecure.com") == false
  end

  # SafeNetwork Tests

  test "valid_ipv4? validates IPv4 addresses" do
    assert SafeNetwork.valid_ipv4?("192.168.1.1") == true
    assert SafeNetwork.valid_ipv4?("invalid") == false
    assert SafeNetwork.valid_ipv4?("256.1.1.1") == false
  end

  test "private? detects private addresses" do
    assert SafeNetwork.private?("192.168.1.1") == true
    assert SafeNetwork.private?("10.0.0.1") == true
    assert SafeNetwork.private?("172.16.0.1") == true
    assert SafeNetwork.private?("8.8.8.8") == false
  end

  test "loopback? detects loopback addresses" do
    assert SafeNetwork.loopback?("127.0.0.1") == true
    assert SafeNetwork.loopback?("192.168.1.1") == false
  end

  test "public? detects public addresses" do
    assert SafeNetwork.public?("8.8.8.8") == true
    assert SafeNetwork.public?("192.168.1.1") == false
  end

  # SafeCrypto Tests

  test "constant_time_compare compares binaries" do
    assert SafeCrypto.constant_time_compare("secret", "secret") == true
    assert SafeCrypto.constant_time_compare("secret", "other") == false
    assert SafeCrypto.constant_time_compare("", "") == true
  end

  test "secure_zero returns zeroed binary" do
    data = <<1, 2, 3, 4>>
    zeroed = SafeCrypto.secure_zero(data)
    assert zeroed == <<0, 0, 0, 0>>
    assert byte_size(zeroed) == byte_size(data)
  end
end
