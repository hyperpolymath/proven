// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCrypto - Cryptographic operations that cannot crash.
 */

/** Compare two strings in constant time */
let constantTimeEq = (a: array<int>, b: array<int>): bool => {
  if Belt.Array.length(a) != Belt.Array.length(b) {
    false
  } else {
    let diff = ref(0)
    Belt.Array.forEachWithIndex(a, (i, aVal) => {
      let bVal = Belt.Array.getUnsafe(b, i)
      diff := lor(diff.contents, lxor(aVal, bVal))
    })
    diff.contents == 0
  }
}

/** Compare two strings in constant time */
let secureCompare = (a: string, b: string): bool => {
  let aBytes = Js.String2.split(a, "")->Belt.Array.map(c => Js.String2.charCodeAt(c, 0)->Belt.Float.toInt)
  let bBytes = Js.String2.split(b, "")->Belt.Array.map(c => Js.String2.charCodeAt(c, 0)->Belt.Float.toInt)
  constantTimeEq(aBytes, bBytes)
}

@val external crypto: 'a = "globalThis.crypto"
@send external getRandomValues: ('a, Js.TypedArray2.Uint8Array.t) => Js.TypedArray2.Uint8Array.t = "getRandomValues"

/** Generate cryptographically secure random bytes */
let randomBytes = (length: int): option<Js.TypedArray2.Uint8Array.t> => {
  if length <= 0 || length > 1024 * 1024 {
    None
  } else {
    try {
      let buffer = Js.TypedArray2.Uint8Array.fromLength(length)
      Some(getRandomValues(crypto, buffer))
    } catch {
    | _ => None
    }
  }
}

/** Generate a random hex string */
let randomHex = (length: int): option<string> => {
  if length <= 0 || mod(length, 2) != 0 {
    None
  } else {
    switch randomBytes(length / 2) {
    | None => None
    | Some(bytes) =>
      let hex = ref("")
      let hexChars = "0123456789abcdef"
      for i in 0 to Js.TypedArray2.Uint8Array.length(bytes) - 1 {
        let byte = Js.TypedArray2.Uint8Array.unsafe_get(bytes, i)
        hex := hex.contents ++
          Js.String2.charAt(hexChars, lsr(byte, 4)) ++
          Js.String2.charAt(hexChars, land(byte, 0x0f))
      }
      Some(hex.contents)
    }
  }
}
