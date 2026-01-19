// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeCrypto - Cryptographic operations that cannot crash.
 *
 * Provides verified-safe cryptographic primitives:
 * - SHA3-256/512 hashing (FIPS 202)
 * - BLAKE3 hashing (fast, secure)
 * - HMAC-SHA3-256 message authentication
 * - Constant-time comparison
 * - Secure random number generation
 * - Hex encoding/decoding
 *
 * All operations return Option types to handle edge cases safely.
 */

// ============================================================================
// External bindings for crypto libraries
// ============================================================================

// js-sha3 bindings
@module("js-sha3") external sha3_256_internal: string => string = "sha3_256"
@module("js-sha3") external sha3_512_internal: string => string = "sha3_512"

// js-sha3 with array input
@module("js-sha3") @scope("sha3_256")
external sha3_256_array: array<int> => string = "array"
@module("js-sha3") @scope("sha3_512")
external sha3_512_array: array<int> => string = "array"

// HMAC from js-sha3
@module("js-sha3") @scope("sha3_256")
external hmac_sha3_256_create: string => 'a = "hmac.create"
@send external hmacUpdate: ('a, string) => 'a = "update"
@send external hmacHex: 'a => string = "hex"

// blake3 bindings (native module)
@module("blake3") external blake3Hash: Js.TypedArray2.Uint8Array.t => Js.TypedArray2.Uint8Array.t = "hash"
@module("blake3") external blake3KeyedHash: (string, Js.TypedArray2.Uint8Array.t) => Js.TypedArray2.Uint8Array.t = "keyedHash"
@module("blake3") external blake3DeriveKey: (string, Js.TypedArray2.Uint8Array.t) => Js.TypedArray2.Uint8Array.t = "deriveKey"

// Web Crypto API
@val external crypto: 'a = "globalThis.crypto"
@send external getRandomValues: ('a, Js.TypedArray2.Uint8Array.t) => Js.TypedArray2.Uint8Array.t = "getRandomValues"

// ============================================================================
// Hex encoding/decoding
// ============================================================================

/** Convert bytes to hex string */
let toHex = (bytes: Js.TypedArray2.Uint8Array.t): string => {
  let hexChars = "0123456789abcdef"
  let result = ref("")
  for i in 0 to Js.TypedArray2.Uint8Array.length(bytes) - 1 {
    let byte = Js.TypedArray2.Uint8Array.unsafe_get(bytes, i)
    result := result.contents ++
      Js.String2.charAt(hexChars, lsr(byte, 4)) ++
      Js.String2.charAt(hexChars, land(byte, 0x0f))
  }
  result.contents
}

/** Parse a single hex character to its value */
let hexCharToInt = (c: string): option<int> => {
  switch c {
  | "0" => Some(0)
  | "1" => Some(1)
  | "2" => Some(2)
  | "3" => Some(3)
  | "4" => Some(4)
  | "5" => Some(5)
  | "6" => Some(6)
  | "7" => Some(7)
  | "8" => Some(8)
  | "9" => Some(9)
  | "a" | "A" => Some(10)
  | "b" | "B" => Some(11)
  | "c" | "C" => Some(12)
  | "d" | "D" => Some(13)
  | "e" | "E" => Some(14)
  | "f" | "F" => Some(15)
  | _ => None
  }
}

/** Convert hex string to bytes */
let fromHex = (hex: string): option<Js.TypedArray2.Uint8Array.t> => {
  let len = Js.String2.length(hex)
  if mod(len, 2) != 0 {
    None
  } else {
    try {
      let bytes = Js.TypedArray2.Uint8Array.fromLength(len / 2)
      let valid = ref(true)
      for i in 0 to len / 2 - 1 {
        let high = Js.String2.charAt(hex, i * 2)
        let low = Js.String2.charAt(hex, i * 2 + 1)
        switch (hexCharToInt(high), hexCharToInt(low)) {
        | (Some(h), Some(l)) =>
          Js.TypedArray2.Uint8Array.unsafe_set(bytes, i, lor(lsl(h, 4), l))
        | _ =>
          valid := false
        }
      }
      if valid.contents {
        Some(bytes)
      } else {
        None
      }
    } catch {
    | _ => None
    }
  }
}

// ============================================================================
// SHA3 Hashing (FIPS 202)
// ============================================================================

/** Hash data using SHA3-256, returns hex string */
let sha3_256 = (data: string): option<string> => {
  if Js.String2.length(data) > 1024 * 1024 * 100 {
    // 100MB limit
    None
  } else {
    try {
      Some(sha3_256_internal(data))
    } catch {
    | _ => None
    }
  }
}

/** Hash bytes using SHA3-256, returns hex string */
let sha3_256Bytes = (data: Js.TypedArray2.Uint8Array.t): option<string> => {
  if Js.TypedArray2.Uint8Array.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      // Convert Uint8Array to regular array for js-sha3
      let arr = []
      for i in 0 to Js.TypedArray2.Uint8Array.length(data) - 1 {
        let _ = Js.Array2.push(arr, Js.TypedArray2.Uint8Array.unsafe_get(data, i))
      }
      Some(sha3_256_array(arr))
    } catch {
    | _ => None
    }
  }
}

/** Hash data using SHA3-512, returns hex string */
let sha3_512 = (data: string): option<string> => {
  if Js.String2.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      Some(sha3_512_internal(data))
    } catch {
    | _ => None
    }
  }
}

/** Hash bytes using SHA3-512, returns hex string */
let sha3_512Bytes = (data: Js.TypedArray2.Uint8Array.t): option<string> => {
  if Js.TypedArray2.Uint8Array.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      let arr = []
      for i in 0 to Js.TypedArray2.Uint8Array.length(data) - 1 {
        let _ = Js.Array2.push(arr, Js.TypedArray2.Uint8Array.unsafe_get(data, i))
      }
      Some(sha3_512_array(arr))
    } catch {
    | _ => None
    }
  }
}

// ============================================================================
// BLAKE3 Hashing
// ============================================================================

/** Hash data using BLAKE3, returns hex string */
let blake3 = (data: Js.TypedArray2.Uint8Array.t): option<string> => {
  if Js.TypedArray2.Uint8Array.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      let hash = blake3Hash(data)
      Some(toHex(hash))
    } catch {
    | _ => None
    }
  }
}

/** Hash string using BLAKE3, returns hex string */
let blake3String = (data: string): option<string> => {
  try {
    // Convert string to Uint8Array using TextEncoder
    let _ = data // Used in raw JS below
    let bytes: Js.TypedArray2.Uint8Array.t = %raw(`new TextEncoder().encode(data)`)
    blake3(bytes)
  } catch {
  | _ => None
  }
}

/** Keyed BLAKE3 hash (for MAC), returns hex string */
let blake3Keyed = (key: string, data: Js.TypedArray2.Uint8Array.t): option<string> => {
  if Js.String2.length(key) != 32 {
    // BLAKE3 requires 32-byte key
    None
  } else if Js.TypedArray2.Uint8Array.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      let hash = blake3KeyedHash(key, data)
      Some(toHex(hash))
    } catch {
    | _ => None
    }
  }
}

/** Derive key using BLAKE3, returns hex string */
let blake3DeriveKeyFromContext = (context: string, material: Js.TypedArray2.Uint8Array.t): option<string> => {
  if Js.TypedArray2.Uint8Array.length(material) == 0 {
    None
  } else {
    try {
      let derived = blake3DeriveKey(context, material)
      Some(toHex(derived))
    } catch {
    | _ => None
    }
  }
}

// ============================================================================
// Constant-time comparison
// ============================================================================

/** Compare two byte arrays in constant time */
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

/** Compare two strings in constant time (prevents timing attacks) */
let secureCompare = (a: string, b: string): bool => {
  let aBytes = Js.String2.split(a, "")->Belt.Array.map(c => Js.String2.charCodeAt(c, 0)->Belt.Float.toInt)
  let bBytes = Js.String2.split(b, "")->Belt.Array.map(c => Js.String2.charCodeAt(c, 0)->Belt.Float.toInt)
  constantTimeEq(aBytes, bBytes)
}

// ============================================================================
// HMAC-SHA3-256
// ============================================================================

/** Compute HMAC-SHA3-256, returns hex string */
let hmacSha3_256 = (key: string, data: string): option<string> => {
  if Js.String2.length(key) == 0 {
    None
  } else if Js.String2.length(data) > 1024 * 1024 * 100 {
    None
  } else {
    try {
      let hmac = hmac_sha3_256_create(key)
      let _ = hmacUpdate(hmac, data)
      Some(hmacHex(hmac))
    } catch {
    | _ => None
    }
  }
}

/** Verify HMAC-SHA3-256 in constant time */
let verifyHmacSha3_256 = (key: string, data: string, expectedMac: string): bool => {
  switch hmacSha3_256(key, data) {
  | None => false
  | Some(computed) => secureCompare(computed, expectedMac)
  }
}

// ============================================================================
// Random number generation
// ============================================================================

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
let randomHex = (byteLength: int): option<string> => {
  switch randomBytes(byteLength) {
  | None => None
  | Some(bytes) => Some(toHex(bytes))
  }
}

/** Generate a random integer in range [0, max) */
let randomInt = (max: int): option<int> => {
  if max <= 0 {
    None
  } else {
    switch randomBytes(4) {
    | None => None
    | Some(bytes) =>
      let value =
        lor(
          lor(
            lor(
              Js.TypedArray2.Uint8Array.unsafe_get(bytes, 0),
              lsl(Js.TypedArray2.Uint8Array.unsafe_get(bytes, 1), 8),
            ),
            lsl(Js.TypedArray2.Uint8Array.unsafe_get(bytes, 2), 16),
          ),
          lsl(land(Js.TypedArray2.Uint8Array.unsafe_get(bytes, 3), 0x7f), 24),
        )
      Some(mod(value, max))
    }
  }
}
