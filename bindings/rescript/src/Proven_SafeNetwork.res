// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeNetwork - Network operations that cannot crash.
 */

type ipv4 = {octets: (int, int, int, int)}

/** Parse an IPv4 address */
let parseIpv4 = (address: string): option<ipv4> => {
  let parts = Js.String2.split(address, ".")
  if Belt.Array.length(parts) != 4 {
    None
  } else {
    let valid = ref(true)
    let octets = parts->Belt.Array.map(part => {
      switch Belt.Int.fromString(part) {
      | Some(n) if n >= 0 && n <= 255 => n
      | _ =>
        valid := false
        0
      }
    })
    if valid.contents {
      Some({
        octets: (
          Belt.Array.getUnsafe(octets, 0),
          Belt.Array.getUnsafe(octets, 1),
          Belt.Array.getUnsafe(octets, 2),
          Belt.Array.getUnsafe(octets, 3),
        ),
      })
    } else {
      None
    }
  }
}

/** Check if a string is a valid IPv4 address */
let isValidIpv4 = (address: string): bool => {
  switch parseIpv4(address) {
  | Some(_) => true
  | None => false
  }
}

/** Check if an IPv4 address is in a private range */
let isPrivate = (address: string): bool => {
  switch parseIpv4(address) {
  | None => false
  | Some({octets: (a, b, _, _)}) =>
    // 10.0.0.0/8
    a == 10 ||
    // 172.16.0.0/12
    (a == 172 && b >= 16 && b <= 31) ||
    // 192.168.0.0/16
    (a == 192 && b == 168)
  }
}

/** Check if an IPv4 address is a loopback address */
let isLoopback = (address: string): bool => {
  switch parseIpv4(address) {
  | None => false
  | Some({octets: (a, _, _, _)}) => a == 127
  }
}

/** Check if an IPv4 address is a public (routable) address */
let isPublic = (address: string): bool => {
  isValidIpv4(address) && !isPrivate(address) && !isLoopback(address)
}

/** Format an IPv4 address as a string */
let formatIpv4 = ({octets: (a, b, c, d)}: ipv4): string => {
  `${Belt.Int.toString(a)}.${Belt.Int.toString(b)}.${Belt.Int.toString(c)}.${Belt.Int.toString(d)}`
}
