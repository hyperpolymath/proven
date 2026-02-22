# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule ProvenTest do
  use ExUnit.Case
  doctest Proven

  alias Proven.SafeMath
  alias Proven.SafeString
  alias Proven.SafePath
  alias Proven.SafeEmail
  alias Proven.SafeNetwork
  alias Proven.SafeCrypto
  alias Proven.SafeFloat
  alias Proven.SafeHex
  alias Proven.SafeUuid
  alias Proven.SafeAngle
  alias Proven.SafeColor
  alias Proven.SafeGeo
  alias Proven.SafeChecksum
  alias Proven.SafeProbability
  alias Proven.SafeMl
  alias Proven.SafeJson
  alias Proven.SafeHeader
  alias Proven.SafeCookie
  alias Proven.SafeContentType
  alias Proven.SafePassword

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

  test "safe_sub subtracts correctly" do
    assert SafeMath.safe_sub(10, 3) == {:ok, 7}
  end

  test "safe_mul multiplies correctly" do
    assert SafeMath.safe_mul(3, 4) == {:ok, 12}
  end

  # SafeString Tests

  test "escape_html escapes HTML entities" do
    assert SafeString.escape_html("<script>") == {:ok, "&lt;script&gt;"}
    assert SafeString.escape_html("a & b") == {:ok, "a &amp; b"}
  end

  test "escape_sql escapes single quotes" do
    assert SafeString.escape_sql("it's") == {:ok, "it''s"}
  end

  test "escape_js escapes JavaScript special characters" do
    assert SafeString.escape_js("line\nbreak") == {:ok, "line\\nbreak"}
  end

  # SafePath Tests

  test "has_traversal? detects traversal sequences" do
    assert SafePath.has_traversal?("../etc/passwd") == true
    assert SafePath.safe?("safe/path") == true
  end

  test "safe_join joins paths safely" do
    assert SafePath.safe_join("/base", ["a", "b"]) == {:ok, "/base/a/b"}
    assert SafePath.safe_join("/base", ["../etc"]) == {:error, :traversal_detected}
  end

  # SafeEmail Tests

  test "valid? validates email addresses" do
    assert SafeEmail.valid?("user@example.com") == true
    assert SafeEmail.valid?("not-an-email") == false
  end

  # SafeNetwork Tests

  test "parse_ipv4 parses valid addresses" do
    assert SafeNetwork.valid_ipv4?("192.168.1.1") == true
    assert SafeNetwork.valid_ipv4?("invalid") == false
  end

  test "private? detects private addresses" do
    assert SafeNetwork.private?("192.168.1.1") == true
    assert SafeNetwork.private?("10.0.0.1") == true
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
  end

  test "secure_zero returns zeroed binary" do
    data = <<1, 2, 3, 4>>
    zeroed = SafeCrypto.secure_zero(data)
    assert zeroed == <<0, 0, 0, 0>>
    assert byte_size(zeroed) == byte_size(data)
  end

  # SafeFloat Tests

  test "finite? detects finite values" do
    assert SafeFloat.finite?(1.5) == true
  end

  test "safe_div handles float division" do
    assert SafeFloat.safe_div(10.0, 2.0) == {:ok, 5.0}
  end

  # SafeHex Tests

  test "encode encodes binary to hex" do
    assert SafeHex.encode(<<255, 0, 128>>) == {:ok, "ff0080"}
    assert SafeHex.encode(<<255, 0, 128>>, case: :upper) == {:ok, "FF0080"}
  end

  test "decode decodes hex to binary" do
    assert SafeHex.decode("ff0080") == {:ok, <<255, 0, 128>>}
  end

  # SafeUuid Tests

  test "generate_v4 creates valid UUID" do
    assert {:ok, uuid} = SafeUuid.generate_v4()
    assert is_binary(uuid)
    assert SafeUuid.valid?(uuid)
  end

  test "parse validates UUIDs" do
    assert SafeUuid.valid?("550e8400-e29b-41d4-a716-446655440000") == true
    assert SafeUuid.valid?("not-a-uuid") == false
  end

  # SafeAngle Tests

  test "to_radians converts degrees to radians" do
    {:ok, rad} = SafeAngle.to_radians(180.0)
    assert_in_delta rad, :math.pi(), 0.0001
  end

  test "normalize_degrees normalizes angles" do
    {:ok, norm} = SafeAngle.normalize_degrees(450.0)
    assert_in_delta norm, 90.0, 0.0001
  end

  # SafeColor Tests

  test "from_hex parses hex colors" do
    assert SafeColor.from_hex("#ff0000") == {:ok, {255, 0, 0}}
  end

  test "to_hsl converts RGB to HSL" do
    {:ok, {h, _s, _l}} = SafeColor.to_hsl({255, 0, 0})
    assert_in_delta h, 0.0, 1.0
  end

  # SafeGeo Tests

  test "validate accepts valid coordinates" do
    assert {:ok, {51.5074, -0.1278}} = SafeGeo.validate(51.5074, -0.1278)
  end

  # SafeChecksum Tests

  test "crc32 computes checksums" do
    assert {:ok, _checksum} = SafeChecksum.crc32("hello")
  end

  # SafeProbability Tests

  test "create clamps to [0, 1]" do
    assert SafeProbability.create(0.5) == 0.5
    assert SafeProbability.create(1.5) == 1.0
    assert SafeProbability.create(-0.5) == 0.0
  end

  # SafeML Tests

  test "sigmoid computes correctly" do
    result = SafeMl.sigmoid(0.0)
    assert_in_delta result, 0.5, 0.001
  end

  test "relu computes correctly" do
    assert SafeMl.relu(5.0) == 5.0
    assert SafeMl.relu(-5.0) == 0.0
  end

  # SafeJson Tests

  test "valid? validates JSON" do
    assert SafeJson.valid?("{\"key\": \"value\"}") == true
    assert SafeJson.valid?("invalid") == false
  end

  # SafeHeader Tests

  test "has_crlf? detects CRLF injection" do
    assert SafeHeader.has_crlf?("safe-value") == false
    assert SafeHeader.has_crlf?("bad\r\nvalue") == true
  end

  # SafeCookie Tests

  test "has_injection? detects cookie injection" do
    assert SafeCookie.has_injection?("safe") == false
    assert SafeCookie.has_injection?("bad;value") == true
  end

  # SafeContentType Tests

  test "can_sniff_to_dangerous? detects sniffable types" do
    assert SafeContentType.can_sniff_to_dangerous?("text/plain") == true
    assert SafeContentType.can_sniff_to_dangerous?("text/html") == false
  end

  # SafePassword Tests

  test "validate checks password strength" do
    assert {:ok, _strength} = SafePassword.validate("P@ssw0rd!Complex123")
  end

  test "common? detects common passwords" do
    assert SafePassword.common?("password") == true
  end
end
