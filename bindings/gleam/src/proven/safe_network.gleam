// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeNetwork - Network operations that cannot crash.
////
//// Provides safe IP address parsing and validation.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// An IPv4 address.
pub type Ipv4 {
  Ipv4(a: Int, b: Int, c: Int, d: Int)
}

/// Parse an IPv4 address.
pub fn parse_ipv4(address: String) -> Option(Ipv4) {
  let parts = string.split(address, ".")
  case list.length(parts) == 4 {
    False -> None
    True -> {
      let parsed =
        parts
        |> list.map(fn(p) { int.parse(p) |> result.unwrap(-1) })

      let all_valid =
        list.all(parsed, fn(n) { n >= 0 && n <= 255 })

      case all_valid {
        False -> None
        True ->
          case parsed {
            [a, b, c, d] -> Some(Ipv4(a: a, b: b, c: c, d: d))
            _ -> None
          }
      }
    }
  }
}

/// Check if a string is a valid IPv4 address.
pub fn is_valid_ipv4(address: String) -> Bool {
  case parse_ipv4(address) {
    Some(_) -> True
    None -> False
  }
}

/// Check if an IPv4 address is in a private range.
pub fn is_private(address: String) -> Bool {
  case parse_ipv4(address) {
    None -> False
    Some(Ipv4(a: a, b: b, c: _, d: _)) ->
      // 10.0.0.0/8
      a == 10
      // 172.16.0.0/12
      || { a == 172 && b >= 16 && b <= 31 }
      // 192.168.0.0/16
      || { a == 192 && b == 168 }
  }
}

/// Check if an IPv4 address is a loopback address.
pub fn is_loopback(address: String) -> Bool {
  case parse_ipv4(address) {
    None -> False
    Some(Ipv4(a: a, b: _, c: _, d: _)) -> a == 127
  }
}

/// Check if an IPv4 address is a public (routable) address.
pub fn is_public(address: String) -> Bool {
  is_valid_ipv4(address) && !is_private(address) && !is_loopback(address)
}

/// Format an IPv4 address as a string.
pub fn format_ipv4(ip: Ipv4) -> String {
  int.to_string(ip.a)
  <> "."
  <> int.to_string(ip.b)
  <> "."
  <> int.to_string(ip.c)
  <> "."
  <> int.to_string(ip.d)
}
