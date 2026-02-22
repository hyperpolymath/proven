// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Gleam/Hex ecosystem)
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

//// Tests for the Proven Gleam binding.
////
//// These tests verify that the NIF bridge correctly calls through to
//// libproven. All computation is delegated to the Idris2-verified core;
//// these tests only confirm the FFI marshalling works correctly.

import gleeunit
import gleeunit/should
import proven
import proven/math
import proven/string_ops
import proven/email
import proven/network
import proven/path
import proven/float_ops
import proven/datetime
import proven/hex
import proven/uuid
import proven/json
import proven/angle
import proven/crypto

pub fn main() {
  gleeunit.main()
}

// -- Math tests ---------------------------------------------------------------

pub fn math_div_ok_test() {
  math.div(10, 2)
  |> should.equal(Ok(5))
}

pub fn math_div_by_zero_test() {
  math.div(10, 0)
  |> should.be_error()
}

pub fn math_add_checked_test() {
  math.add_checked(3, 4)
  |> should.equal(Ok(7))
}

pub fn math_sub_checked_test() {
  math.sub_checked(10, 3)
  |> should.equal(Ok(7))
}

pub fn math_mul_checked_test() {
  math.mul_checked(3, 4)
  |> should.equal(Ok(12))
}

pub fn math_modulo_test() {
  math.modulo(10, 3)
  |> should.equal(Ok(1))
}

pub fn math_modulo_by_zero_test() {
  math.modulo(10, 0)
  |> should.be_error()
}

pub fn math_clamp_test() {
  math.clamp(0, 10, 15)
  |> should.equal(10)
}

// -- String tests -------------------------------------------------------------

pub fn string_escape_html_test() {
  string_ops.escape_html("<script>alert(1)</script>")
  |> should.be_ok()
}

pub fn string_escape_sql_test() {
  string_ops.escape_sql("it's a test")
  |> should.be_ok()
}

pub fn string_escape_js_test() {
  string_ops.escape_js("hello\nworld")
  |> should.be_ok()
}

// -- Email tests --------------------------------------------------------------

pub fn email_valid_test() {
  email.is_valid("user@example.com")
  |> should.equal(Ok(True))
}

pub fn email_invalid_test() {
  email.is_valid("not-an-email")
  |> should.equal(Ok(False))
}

// -- Path tests ---------------------------------------------------------------

pub fn path_has_traversal_test() {
  path.has_traversal("../../etc/passwd")
  |> should.equal(Ok(True))
}

pub fn path_safe_test() {
  path.has_traversal("docs/readme.txt")
  |> should.equal(Ok(False))
}

pub fn path_sanitize_test() {
  path.sanitize_filename("../../evil.sh")
  |> should.be_ok()
}

// -- Network tests ------------------------------------------------------------

pub fn network_parse_ipv4_test() {
  network.parse_ipv4("192.168.1.1")
  |> should.be_ok()
}

pub fn network_parse_ipv4_invalid_test() {
  network.parse_ipv4("not-an-ip")
  |> should.be_error()
}

pub fn network_ipv4_private_test() {
  network.ipv4_is_private(192, 168, 1, 1)
  |> should.equal(True)
}

pub fn network_ipv4_public_test() {
  network.ipv4_is_private(8, 8, 8, 8)
  |> should.equal(False)
}

pub fn network_ipv4_loopback_test() {
  network.ipv4_is_loopback(127, 0, 0, 1)
  |> should.equal(True)
}

// -- Float tests --------------------------------------------------------------

pub fn float_div_ok_test() {
  float_ops.div(10.0, 2.0)
  |> should.be_ok()
}

pub fn float_div_by_zero_test() {
  float_ops.div(10.0, 0.0)
  |> should.be_error()
}

pub fn float_sqrt_ok_test() {
  float_ops.sqrt(4.0)
  |> should.be_ok()
}

pub fn float_sqrt_negative_test() {
  float_ops.sqrt(-1.0)
  |> should.be_error()
}

// -- DateTime tests -----------------------------------------------------------

pub fn datetime_leap_year_test() {
  datetime.is_leap_year(2024)
  |> should.equal(True)
}

pub fn datetime_not_leap_year_test() {
  datetime.is_leap_year(2023)
  |> should.equal(False)
}

pub fn datetime_days_in_month_test() {
  datetime.days_in_month(2024, 2)
  |> should.equal(29)
}

pub fn datetime_parse_test() {
  datetime.parse("2024-12-15T10:30:00Z")
  |> should.be_ok()
}

// -- Hex tests ----------------------------------------------------------------

pub fn hex_encode_test() {
  hex.encode(<<0xDE, 0xAD, 0xBE, 0xEF>>)
  |> should.be_ok()
}

pub fn hex_is_valid_test() {
  hex.is_valid("deadbeef")
  |> should.equal(True)
}

pub fn hex_is_valid_invalid_test() {
  hex.is_valid("xyz")
  |> should.equal(False)
}

// -- UUID tests ---------------------------------------------------------------

pub fn uuid_is_valid_test() {
  uuid.is_valid("550e8400-e29b-41d4-a716-446655440000")
  |> should.equal(True)
}

pub fn uuid_is_valid_invalid_test() {
  uuid.is_valid("not-a-uuid")
  |> should.equal(False)
}

pub fn uuid_v4_generate_test() {
  uuid.v4_generate()
  |> should.be_ok()
}

// -- JSON tests ---------------------------------------------------------------

pub fn json_is_valid_test() {
  json.is_valid("{\"key\": \"value\"}")
  |> should.equal(Ok(True))
}

pub fn json_is_valid_invalid_test() {
  json.is_valid("{broken")
  |> should.equal(Ok(False))
}

// -- Angle tests --------------------------------------------------------------

pub fn angle_deg_to_rad_test() {
  // 180 degrees = pi radians (approximately 3.14159)
  let result = angle.deg_to_rad(180.0)
  let diff = result -. 3.14159265358979
  { diff >. -0.0001 && diff <. 0.0001 }
  |> should.equal(True)
}

pub fn angle_normalize_degrees_test() {
  // 370 degrees normalized to [0, 360) = 10 degrees
  let result = angle.normalize_degrees(370.0)
  let diff = result -. 10.0
  { diff >. -0.0001 && diff <. 0.0001 }
  |> should.equal(True)
}

// -- Crypto tests -------------------------------------------------------------

pub fn crypto_random_bytes_test() {
  crypto.random_bytes(16)
  |> should.be_ok()
}

pub fn crypto_constant_time_eq_same_test() {
  crypto.constant_time_eq(<<1, 2, 3>>, <<1, 2, 3>>)
  |> should.equal(Ok(True))
}

pub fn crypto_constant_time_eq_different_test() {
  crypto.constant_time_eq(<<1, 2, 3>>, <<4, 5, 6>>)
  |> should.equal(Ok(False))
}
