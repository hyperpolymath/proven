// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeUuid - UUID parsing and validation that cannot crash.
 *
 * Provides safe UUID operations with proper version and variant detection.
 */

/** UUID version variants */
type version =
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8
  | Unknown

/** UUID variant types */
type variant =
  | NCS
  | RFC4122
  | Microsoft
  | Future
  | Unknown

/** Validated UUID type */
type t = {
  bytes: array<int>,
  version: version,
  variant: variant,
  formatted: string,
}

/** Error types for UUID parsing */
type parseError =
  | InvalidLength
  | InvalidFormat
  | InvalidHexCharacter

/** UUID regex pattern */
let uuidRegex = %re("/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i")

/** Convert a hex character to its integer value */
let hexCharToInt = (char: string): option<int> => {
  let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
  if code >= 48 && code <= 57 {
    // 0-9
    Some(code - 48)
  } else if code >= 65 && code <= 70 {
    // A-F
    Some(code - 55)
  } else if code >= 97 && code <= 102 {
    // a-f
    Some(code - 87)
  } else {
    None
  }
}

/** Convert a hex byte string to integer */
let hexByteToInt = (hexByte: string): option<int> => {
  if Js.String2.length(hexByte) != 2 {
    None
  } else {
    switch (
      hexCharToInt(Js.String2.charAt(hexByte, 0)),
      hexCharToInt(Js.String2.charAt(hexByte, 1)),
    ) {
    | (Some(high), Some(low)) => Some(lsl(high, 4) + low)
    | _ => None
    }
  }
}

/** Determine UUID version from version byte */
let getVersion = (versionByte: int): version => {
  switch lsr(versionByte, 4) {
  | 1 => V1
  | 2 => V2
  | 3 => V3
  | 4 => V4
  | 5 => V5
  | 6 => V6
  | 7 => V7
  | 8 => V8
  | _ => Unknown
  }
}

/** Determine UUID variant from variant byte */
let getVariant = (variantByte: int): variant => {
  let highBits = lsr(variantByte, 4)
  if land(highBits, 0x8) == 0 {
    NCS
  } else if land(highBits, 0xC) == 0x8 {
    RFC4122
  } else if land(highBits, 0xE) == 0xC {
    Microsoft
  } else if land(highBits, 0xE) == 0xE {
    Future
  } else {
    Unknown
  }
}

/** Parse a UUID string into a validated UUID */
let parse = (uuidStr: string): result<t, parseError> => {
  let trimmed = Js.String2.trim(uuidStr)
  let normalized = Js.String2.toLowerCase(trimmed)

  // Check format
  if !Js.Re.test_(uuidRegex, normalized) {
    if Js.String2.length(normalized) != 36 {
      Error(InvalidLength)
    } else {
      Error(InvalidFormat)
    }
  } else {
    // Remove hyphens and parse hex bytes
    let hexOnly = Js.String2.replaceByRe(normalized, %re("/-/g"), "")
    let bytes = Belt.Array.makeBy(16, _ => 0)
    let valid = ref(true)

    for i in 0 to 15 {
      let hexByte = Js.String2.slice(hexOnly, ~from=i * 2, ~to_=(i + 1) * 2)
      switch hexByteToInt(hexByte) {
      | Some(byte) => Belt.Array.setUnsafe(bytes, i, byte)
      | None => valid := false
      }
    }

    if !valid.contents {
      Error(InvalidHexCharacter)
    } else {
      let versionByte = Belt.Array.getUnsafe(bytes, 6)
      let variantByte = Belt.Array.getUnsafe(bytes, 8)
      Ok({
        bytes: bytes,
        version: getVersion(versionByte),
        variant: getVariant(variantByte),
        formatted: normalized,
      })
    }
  }
}

/** Check if a string is a valid UUID */
let isValid = (uuidStr: string): bool => {
  switch parse(uuidStr) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Format a UUID as a lowercase hyphenated string */
let format = (uuid: t): string => uuid.formatted

/** Format a UUID as an uppercase hyphenated string */
let formatUppercase = (uuid: t): string => Js.String2.toUpperCase(uuid.formatted)

/** Format a UUID without hyphens */
let formatCompact = (uuid: t): string => Js.String2.replaceByRe(uuid.formatted, %re("/-/g"), "")

/** Get the version of a UUID */
let getVersionFromUuid = (uuid: t): version => uuid.version

/** Get the variant of a UUID */
let getVariantFromUuid = (uuid: t): variant => uuid.variant

/** Version to string representation */
let versionToString = (version: version): string => {
  switch version {
  | V1 => "1"
  | V2 => "2"
  | V3 => "3"
  | V4 => "4"
  | V5 => "5"
  | V6 => "6"
  | V7 => "7"
  | V8 => "8"
  | Unknown => "unknown"
  }
}

/** Variant to string representation */
let variantToString = (variant: variant): string => {
  switch variant {
  | NCS => "NCS"
  | RFC4122 => "RFC4122"
  | Microsoft => "Microsoft"
  | Future => "Future"
  | Unknown => "unknown"
  }
}

/** Nil UUID (all zeros) */
let nil = (): t => {
  bytes: Belt.Array.make(16, 0),
  version: Unknown,
  variant: NCS,
  formatted: "00000000-0000-0000-0000-000000000000",
}

/** Check if a UUID is nil */
let isNil = (uuid: t): bool => {
  Belt.Array.every(uuid.bytes, byte => byte == 0)
}

/** Compare two UUIDs for equality */
let equal = (uuidA: t, uuidB: t): bool => {
  Belt.Array.eq(uuidA.bytes, uuidB.bytes, (a, b) => a == b)
}

/** Compare two UUIDs (for sorting) */
let compare = (uuidA: t, uuidB: t): int => {
  let rec compareBytes = (index: int): int => {
    if index >= 16 {
      0
    } else {
      let byteA = Belt.Array.getUnsafe(uuidA.bytes, index)
      let byteB = Belt.Array.getUnsafe(uuidB.bytes, index)
      if byteA < byteB {
        -1
      } else if byteA > byteB {
        1
      } else {
        compareBytes(index + 1)
      }
    }
  }
  compareBytes(0)
}
