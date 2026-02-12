# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeNetwork - Network primitive validation for Nickel
#
# Validates IPv4, IPv6, CIDR notation, ports, and hostnames.
# All operations are pure and cannot crash.

{
  # IPv4 address pattern
  ipv4_pattern = "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$",

  # Simplified IPv6 pattern (validates format, not exhaustively)
  ipv6_pattern = "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::$|^([0-9a-fA-F]{1,4}:)*:([0-9a-fA-F]{1,4}:)*[0-9a-fA-F]{1,4}$",

  # Hostname pattern (RFC 1123)
  hostname_pattern = "^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$",

  # Check if string is a valid IPv4 address
  is_ipv4
    : String -> Bool
    = fun ip =>
      std.string.is_match ipv4_pattern ip,

  # Check if string is a valid IPv6 address
  is_ipv6
    : String -> Bool
    = fun ip =>
      std.string.is_match ipv6_pattern ip,

  # Check if string is a valid IP address (v4 or v6)
  is_ip
    : String -> Bool
    = fun ip =>
      is_ipv4 ip || is_ipv6 ip,

  # Check if number is a valid port (0-65535)
  is_valid_port
    : Number -> Bool
    = fun port =>
      port >= 0 && port <= 65535 && port == std.number.truncate port,

  # Check if string is a valid hostname
  is_hostname
    : String -> Bool
    = fun host =>
      std.string.is_match hostname_pattern host
      && std.string.length host <= 253,

  # Check if string is a valid CIDR notation (IPv4)
  is_cidr_v4
    : String -> Bool
    = fun cidr =>
      let parts = std.string.split "/" cidr in
      if std.array.length parts != 2 then
        false
      else
        let ip = std.array.first parts in
        let prefix_str = std.array.last parts in
        is_ipv4 ip
        && std.string.is_match "^[0-9]+$" prefix_str,

  # Parse CIDR into IP and prefix length
  parse_cidr
    : String -> { ip : String, prefix : Number, ok : Bool }
    = fun cidr =>
      let parts = std.string.split "/" cidr in
      if std.array.length parts != 2 then
        { ip = "", prefix = 0, ok = false }
      else
        let ip = std.array.first parts in
        let prefix_str = std.array.last parts in
        if !(is_ipv4 ip) then
          { ip = "", prefix = 0, ok = false }
        else
          {
            ip = ip,
            prefix = std.string.to_number prefix_str,
            ok = true,
          },

  # Check if IP is in private range (RFC 1918)
  is_private_ip
    : String -> Bool
    = fun ip =>
      std.string.is_match "^10\\." ip
      || std.string.is_match "^172\\.(1[6-9]|2[0-9]|3[01])\\." ip
      || std.string.is_match "^192\\.168\\." ip,

  # Check if IP is loopback
  is_loopback
    : String -> Bool
    = fun ip =>
      std.string.is_match "^127\\." ip || ip == "::1",

  # IPv4 address contract
  IPv4 = std.contract.from_predicate is_ipv4,

  # IPv6 address contract
  IPv6 = std.contract.from_predicate is_ipv6,

  # Any IP address contract
  IPAddress = std.contract.from_predicate is_ip,

  # Port number contract (0-65535)
  Port = std.contract.from_predicate is_valid_port,

  # Privileged port contract (0-1023)
  PrivilegedPort = std.contract.from_predicate (fun p => p >= 0 && p <= 1023),

  # User port contract (1024-49151)
  UserPort = std.contract.from_predicate (fun p => p >= 1024 && p <= 49151),

  # Hostname contract
  Hostname = std.contract.from_predicate is_hostname,

  # CIDR notation contract
  CIDR = std.contract.from_predicate is_cidr_v4,
}
