// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeBase64 - Base64 encoding and decoding operations that cannot crash.
 *
 * Supports standard Base64 (RFC 4648), URL-safe Base64, and variants with/without padding.
 * All operations include proper validation and return Result types on failure.
 */

/** Error types for Base64 operations */
type base64Error =
  | InvalidCharacter
  | InvalidPadding
  | InvalidLength
  | BufferTooSmall

/** Base64 encoding variants */
type variant =
  | Standard // RFC 4648 standard alphabet with padding
  | StandardNoPad // Standard alphabet without padding
  | UrlSafe // URL-safe alphabet with padding
  | UrlSafeNoPad // URL-safe alphabet without padding

/** Standard Base64 alphabet (RFC 4648) */
let standardAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/** URL-safe Base64 alphabet (RFC 4648 Section 5) */
let urlSafeAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

/** Padding character */
let padChar = "="

/** Get the alphabet for a variant */
let getAlphabet = (v: variant): string => {
  switch v {
  | Standard | StandardNoPad => standardAlphabet
  | UrlSafe | UrlSafeNoPad => urlSafeAlphabet
  }
}

/** Check if a variant uses padding */
let usesPadding = (v: variant): bool => {
  switch v {
  | Standard | UrlSafe => true
  | StandardNoPad | UrlSafeNoPad => false
  }
}

/** Calculate encoded length for given input length */
let calcEncodedLen = (inputLen: int, withPadding: bool): int => {
  if inputLen == 0 {
    0
  } else if withPadding {
    // With padding: always multiple of 4
    ((inputLen + 2) / 3) * 4
  } else {
    // No padding: exact characters needed
    let fullGroups = inputLen / 3
    let remainder = mod(inputLen, 3)
    let extraChars = switch remainder {
    | 0 => 0
    | 1 => 2
    | 2 => 3
    | _ => 0 // Unreachable
    }
    fullGroups * 4 + extraChars
  }
}

/** Calculate maximum decoded length for given encoded length */
let calcDecodedLenMax = (encodedLen: int): int => {
  if encodedLen == 0 {
    0
  } else {
    let fullGroups = encodedLen / 4
    let remainder = mod(encodedLen, 4)
    let extraBytes = switch remainder {
    | 0 => 0
    | 1 => 0 // Invalid, but don't crash
    | 2 => 1
    | 3 => 2
    | _ => 0 // Unreachable
    }
    fullGroups * 3 + extraBytes
  }
}

/** Get the index of a character in the alphabet (returns -1 if not found) */
let charToIndex = (char: string, alphabet: string): int => {
  Js.String2.indexOf(alphabet, char)
}

/** Get character at index in alphabet */
let indexToChar = (index: int, alphabet: string): string => {
  Js.String2.charAt(alphabet, index)
}

/** Encode a byte array to Base64 */
let encodeBytes = (bytes: array<int>, variant: variant): result<string, base64Error> => {
  let inputLen = Belt.Array.length(bytes)
  if inputLen == 0 {
    Ok("")
  } else {
    let alphabet = getAlphabet(variant)
    let withPadding = usesPadding(variant)
    let result = ref("")

    // Validate all bytes are in range 0-255
    let valid = ref(true)
    Belt.Array.forEach(bytes, b => {
      if b < 0 || b > 255 {
        valid := false
      }
    })

    if !valid.contents {
      Error(InvalidCharacter)
    } else {
      // Process full 3-byte groups
      let i = ref(0)
      while i.contents + 3 <= inputLen {
        let b0 = Belt.Array.getUnsafe(bytes, i.contents)
        let b1 = Belt.Array.getUnsafe(bytes, i.contents + 1)
        let b2 = Belt.Array.getUnsafe(bytes, i.contents + 2)

        let c0 = lsr(b0, 2)
        let c1 = lor(lsl(land(b0, 0x03), 4), lsr(b1, 4))
        let c2 = lor(lsl(land(b1, 0x0F), 2), lsr(b2, 6))
        let c3 = land(b2, 0x3F)

        result :=
          result.contents ++
          indexToChar(c0, alphabet) ++
          indexToChar(c1, alphabet) ++
          indexToChar(c2, alphabet) ++
          indexToChar(c3, alphabet)

        i := i.contents + 3
      }

      // Handle remaining bytes
      let remainder = inputLen - i.contents
      if remainder == 1 {
        let b0 = Belt.Array.getUnsafe(bytes, i.contents)

        let c0 = lsr(b0, 2)
        let c1 = lsl(land(b0, 0x03), 4)

        result := result.contents ++ indexToChar(c0, alphabet) ++ indexToChar(c1, alphabet)

        if withPadding {
          result := result.contents ++ "=="
        }
      } else if remainder == 2 {
        let b0 = Belt.Array.getUnsafe(bytes, i.contents)
        let b1 = Belt.Array.getUnsafe(bytes, i.contents + 1)

        let c0 = lsr(b0, 2)
        let c1 = lor(lsl(land(b0, 0x03), 4), lsr(b1, 4))
        let c2 = lsl(land(b1, 0x0F), 2)

        result :=
          result.contents ++
          indexToChar(c0, alphabet) ++
          indexToChar(c1, alphabet) ++
          indexToChar(c2, alphabet)

        if withPadding {
          result := result.contents ++ "="
        }
      }

      Ok(result.contents)
    }
  }
}

/** Encode a string to Base64 (ASCII only) */
let encode = (input: string, variant: variant): result<string, base64Error> => {
  let length = Js.String2.length(input)
  if length == 0 {
    Ok("")
  } else {
    // Convert string to bytes
    let bytes = Belt.Array.make(length, 0)
    let valid = ref(true)

    for i in 0 to length - 1 {
      if valid.contents {
        let code = Js.String2.charCodeAt(input, i)->Belt.Float.toInt
        if code > 255 {
          valid := false
        } else {
          Belt.Array.setUnsafe(bytes, i, code)
        }
      }
    }

    if !valid.contents {
      Error(InvalidCharacter)
    } else {
      encodeBytes(bytes, variant)
    }
  }
}

/** Decode Base64 to a byte array */
let decodeToBytes = (input: string, variant: variant): result<array<int>, base64Error> => {
  let trimmed = Js.String2.trim(input)
  let inputLen = Js.String2.length(trimmed)

  if inputLen == 0 {
    Ok([])
  } else {
    let alphabet = getAlphabet(variant)
    let withPadding = usesPadding(variant)

    // Count and strip padding
    let paddingCount = ref(0)
    let effectiveLen = ref(inputLen)

    if withPadding {
      while effectiveLen.contents > 0 &&
        Js.String2.charAt(trimmed, effectiveLen.contents - 1) == "=" {
        paddingCount := paddingCount.contents + 1
        effectiveLen := effectiveLen.contents - 1
        if paddingCount.contents > 2 {
          () // Can exit early
        }
      }

      // Validate padding count
      if paddingCount.contents > 2 {
        Error(InvalidPadding)
      } else if mod(inputLen, 4) != 0 {
        Error(InvalidLength)
      } else {
        // Calculate output size
        let fullGroups = effectiveLen.contents / 4
        let remainder = mod(effectiveLen.contents, 4)

        let outputLen = fullGroups * 3 + switch remainder {
        | 0 => if paddingCount.contents > 0 && fullGroups > 0 {
            -paddingCount.contents
          } else {
            0
          }
        | 2 => 1
        | 3 => 2
        | _ => 0
        }

        let result = Belt.Array.make(max(0, outputLen), 0)
        let outIdx = ref(0)
        let valid = ref(true)
        let i = ref(0)

        // Process full 4-character groups
        while i.contents + 4 <= effectiveLen.contents && valid.contents {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)
          let c2 = charToIndex(Js.String2.charAt(trimmed, i.contents + 2), alphabet)
          let c3 = charToIndex(Js.String2.charAt(trimmed, i.contents + 3), alphabet)

          if c0 < 0 || c1 < 0 || c2 < 0 || c3 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
            Belt.Array.setUnsafe(result, outIdx.contents + 1, lor(lsl(land(c1, 0x0F), 4), lsr(c2, 2)))
            Belt.Array.setUnsafe(result, outIdx.contents + 2, lor(lsl(land(c2, 0x03), 6), c3))
            outIdx := outIdx.contents + 3
          }

          i := i.contents + 4
        }

        // Handle remaining characters
        let remaining = effectiveLen.contents - i.contents
        if valid.contents && remaining == 2 {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)

          if c0 < 0 || c1 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
          }
        } else if valid.contents && remaining == 3 {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)
          let c2 = charToIndex(Js.String2.charAt(trimmed, i.contents + 2), alphabet)

          if c0 < 0 || c1 < 0 || c2 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
            Belt.Array.setUnsafe(result, outIdx.contents + 1, lor(lsl(land(c1, 0x0F), 4), lsr(c2, 2)))
          }
        } else if remaining == 1 {
          valid := false
        }

        if valid.contents {
          Ok(result)
        } else {
          Error(InvalidCharacter)
        }
      }
    } else {
      // No padding variant
      let remainder = mod(inputLen, 4)
      if remainder == 1 {
        Error(InvalidLength)
      } else {
        let fullGroups = inputLen / 4

        let outputLen = fullGroups * 3 + switch remainder {
        | 0 => 0
        | 2 => 1
        | 3 => 2
        | _ => 0
        }

        let result = Belt.Array.make(outputLen, 0)
        let outIdx = ref(0)
        let valid = ref(true)
        let i = ref(0)

        // Process full 4-character groups
        while i.contents + 4 <= inputLen && valid.contents {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)
          let c2 = charToIndex(Js.String2.charAt(trimmed, i.contents + 2), alphabet)
          let c3 = charToIndex(Js.String2.charAt(trimmed, i.contents + 3), alphabet)

          if c0 < 0 || c1 < 0 || c2 < 0 || c3 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
            Belt.Array.setUnsafe(result, outIdx.contents + 1, lor(lsl(land(c1, 0x0F), 4), lsr(c2, 2)))
            Belt.Array.setUnsafe(result, outIdx.contents + 2, lor(lsl(land(c2, 0x03), 6), c3))
            outIdx := outIdx.contents + 3
          }

          i := i.contents + 4
        }

        // Handle remaining characters
        let remaining = inputLen - i.contents
        if valid.contents && remaining == 2 {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)

          if c0 < 0 || c1 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
          }
        } else if valid.contents && remaining == 3 {
          let c0 = charToIndex(Js.String2.charAt(trimmed, i.contents), alphabet)
          let c1 = charToIndex(Js.String2.charAt(trimmed, i.contents + 1), alphabet)
          let c2 = charToIndex(Js.String2.charAt(trimmed, i.contents + 2), alphabet)

          if c0 < 0 || c1 < 0 || c2 < 0 {
            valid := false
          } else {
            Belt.Array.setUnsafe(result, outIdx.contents, lor(lsl(c0, 2), lsr(c1, 4)))
            Belt.Array.setUnsafe(result, outIdx.contents + 1, lor(lsl(land(c1, 0x0F), 4), lsr(c2, 2)))
          }
        }

        if valid.contents {
          Ok(result)
        } else {
          Error(InvalidCharacter)
        }
      }
    }
  }
}

/** Decode Base64 to a string (ASCII only) */
let decode = (input: string, variant: variant): result<string, base64Error> => {
  switch decodeToBytes(input, variant) {
  | Error(e) => Error(e)
  | Ok(bytes) =>
    let result = ref("")
    Belt.Array.forEach(bytes, b => {
      result := result.contents ++ Js.String2.fromCharCode(b)
    })
    Ok(result.contents)
  }
}

/** Validate a Base64 string */
let isValid = (input: string, variant: variant): bool => {
  let trimmed = Js.String2.trim(input)
  let length = Js.String2.length(trimmed)

  if length == 0 {
    true
  } else {
    let alphabet = getAlphabet(variant)
    let withPadding = usesPadding(variant)

    // Check padding requirements
    if withPadding && mod(length, 4) != 0 {
      false
    } else {
      let valid = ref(true)
      let paddingStarted = ref(false)

      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(trimmed, i)

          if char == "=" {
            if !withPadding {
              valid := false
            } else {
              paddingStarted := true
            }
          } else if paddingStarted.contents {
            // Non-padding char after padding
            valid := false
          } else if charToIndex(char, alphabet) < 0 {
            valid := false
          }
        }
      }

      // Validate padding count
      if valid.contents && withPadding && paddingStarted.contents {
        let padCount = ref(0)
        let idx = ref(length)
        while idx.contents > 0 && Js.String2.charAt(trimmed, idx.contents - 1) == "=" {
          padCount := padCount.contents + 1
          idx := idx.contents - 1
        }
        if padCount.contents > 2 {
          valid := false
        }
      }

      valid.contents
    }
  }
}

/** Check if a character is valid Base64 */
let isValidChar = (char: string, variant: variant): bool => {
  if char == "=" {
    usesPadding(variant)
  } else {
    let alphabet = getAlphabet(variant)
    charToIndex(char, alphabet) >= 0
  }
}

/** Convert between Base64 variants */
let convert = (input: string, from: variant, to: variant): result<string, base64Error> => {
  // Decode from source variant
  switch decodeToBytes(input, from) {
  | Error(e) => Error(e)
  | Ok(bytes) =>
    // Encode to target variant
    encodeBytes(bytes, to)
  }
}

/** Strip whitespace from Base64 string (for lenient parsing) */
let stripWhitespace = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/\\s/g"), "")
}

/** Encode a string to standard Base64 with padding (convenience function) */
let encodeStandard = (input: string): result<string, base64Error> => {
  encode(input, Standard)
}

/** Decode standard Base64 with padding (convenience function) */
let decodeStandard = (input: string): result<string, base64Error> => {
  decode(input, Standard)
}

/** Encode a string to URL-safe Base64 without padding (convenience function) */
let encodeUrlSafe = (input: string): result<string, base64Error> => {
  encode(input, UrlSafeNoPad)
}

/** Decode URL-safe Base64 without padding (convenience function) */
let decodeUrlSafe = (input: string): result<string, base64Error> => {
  decode(input, UrlSafeNoPad)
}

/** Convert error to string */
let errorToString = (err: base64Error): string => {
  switch err {
  | InvalidCharacter => "Invalid character in Base64 string"
  | InvalidPadding => "Invalid padding in Base64 string"
  | InvalidLength => "Invalid length for Base64 string"
  | BufferTooSmall => "Buffer too small for operation"
  }
}
