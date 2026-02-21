// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import gleeunit
import gleeunit/should
import proven/safe_math
import proven/safe_string
import proven/safe_email
import proven/safe_network

pub fn main() {
  gleeunit.main()
}

pub fn div_test() {
  safe_math.div(10, 2)
  |> should.equal(Ok(5))

  safe_math.div(10, 0)
  |> should.equal(Error(Nil))
}

pub fn escape_html_test() {
  safe_string.escape_html("<script>alert(1)</script>")
  |> should.equal("&lt;script&gt;alert(1)&lt;/script&gt;")
}

pub fn email_valid_test() {
  safe_email.is_valid("user@example.com")
  |> should.equal(True)

  safe_email.is_valid("not-an-email")
  |> should.equal(False)
}

pub fn ipv4_parse_test() {
  safe_network.is_valid_ipv4("192.168.1.1")
  |> should.equal(True)

  safe_network.is_valid_ipv4("invalid")
  |> should.equal(False)
}

pub fn ipv4_private_test() {
  safe_network.is_private("192.168.1.1")
  |> should.equal(True)

  safe_network.is_private("8.8.8.8")
  |> should.equal(False)
}
