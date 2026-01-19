// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeChecksum - CRC and hash verification operations that cannot crash.
 *
 * Provides safe checksum calculations including CRC32, Adler-32, Luhn algorithm,
 * ISBN validation, and various other checksum algorithms.
 */

/** CRC32 lookup table (IEEE polynomial 0xEDB88320) */
let crc32Table: array<int> = {
  let table = Belt.Array.make(256, 0)
  for i in 0 to 255 {
    let c = ref(i)
    for _ in 0 to 7 {
      if land(c.contents, 1) == 1 {
        c := lxor(lsr(c.contents, 1), 0xEDB88320)
      } else {
        c := lsr(c.contents, 1)
      }
    }
    Belt.Array.setUnsafe(table, i, c.contents)
  }
  table
}

/** Calculate CRC32 checksum of a byte array */
let crc32Bytes = (data: array<int>): int => {
  let crc = ref(0xFFFFFFFF)

  Belt.Array.forEach(data, byte => {
    let index = land(lxor(crc.contents, byte), 0xFF)
    let tableValue = Belt.Array.getUnsafe(crc32Table, index)
    crc := lxor(lsr(crc.contents, 8), tableValue)
  })

  lxor(crc.contents, 0xFFFFFFFF)
}

/** Calculate CRC32 checksum of a string (ASCII) */
let crc32 = (data: string): int => {
  let bytes = Belt.Array.makeBy(Js.String2.length(data), i => {
    Js.String2.charCodeAt(data, i)->Belt.Float.toInt
  })
  crc32Bytes(bytes)
}

/** Adler-32 checksum of a byte array */
let adler32Bytes = (data: array<int>): int => {
  let modAdler = 65521
  let a = ref(1)
  let b = ref(0)

  Belt.Array.forEach(data, byte => {
    a := Pervasives.mod(a.contents + byte, modAdler)
    b := Pervasives.mod(b.contents + a.contents, modAdler)
  })

  lor(lsl(b.contents, 16), a.contents)
}

/** Adler-32 checksum of a string (ASCII) */
let adler32 = (data: string): int => {
  let bytes = Belt.Array.makeBy(Js.String2.length(data), i => {
    Js.String2.charCodeAt(data, i)->Belt.Float.toInt
  })
  adler32Bytes(bytes)
}

/** Luhn algorithm for credit card validation */
let luhnCheck = (digits: string): bool => {
  let length = Js.String2.length(digits)
  if length == 0 {
    false
  } else {
    let sum = ref(0)
    let alternate = ref(false)
    let valid = ref(true)

    // Process from right to left
    let i = ref(length - 1)
    while valid.contents && i.contents >= 0 {
      let c = Js.String2.charAt(digits, i.contents)
      let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

      if code < 48 || code > 57 {
        // Not a digit 0-9
        valid := false
      } else {
        let digit = ref(code - 48)
        if alternate.contents {
          digit := digit.contents * 2
          if digit.contents > 9 {
            digit := digit.contents - 9
          }
        }
        sum := sum.contents + digit.contents
        alternate := !alternate.contents
      }

      i := i.contents - 1
    }

    valid.contents && Pervasives.mod(sum.contents, 10) == 0
  }
}

/** Luhn check digit generator (for creating valid numbers) */
let luhnCheckDigit = (partialNumber: string): option<int> => {
  let length = Js.String2.length(partialNumber)
  if length == 0 {
    None
  } else {
    let sum = ref(0)
    let alternate = ref(true) // Start true because we're adding a digit at the end
    let valid = ref(true)

    // Process from right to left
    let i = ref(length - 1)
    while valid.contents && i.contents >= 0 {
      let c = Js.String2.charAt(partialNumber, i.contents)
      let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

      if code < 48 || code > 57 {
        valid := false
      } else {
        let digit = ref(code - 48)
        if alternate.contents {
          digit := digit.contents * 2
          if digit.contents > 9 {
            digit := digit.contents - 9
          }
        }
        sum := sum.contents + digit.contents
        alternate := !alternate.contents
      }

      i := i.contents - 1
    }

    if valid.contents {
      Some(Pervasives.mod(10 - Pervasives.mod(sum.contents, 10), 10))
    } else {
      None
    }
  }
}

/** ISBN-10 validation */
let isValidISBN10 = (isbn: string): bool => {
  // Extract digits and X
  let clean = Belt.Array.make(10, 0)
  let idx = ref(0)

  for i in 0 to Js.String2.length(isbn) - 1 {
    if idx.contents < 10 {
      let c = Js.String2.charAt(isbn, i)
      let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

      if code >= 48 && code <= 57 {
        // 0-9
        Belt.Array.setUnsafe(clean, idx.contents, code - 48)
        idx := idx.contents + 1
      } else if (c == "X" || c == "x") && idx.contents == 9 {
        Belt.Array.setUnsafe(clean, idx.contents, 10)
        idx := idx.contents + 1
      }
    }
  }

  if idx.contents != 10 {
    false
  } else {
    let sum = ref(0)
    for i in 0 to 9 {
      let digit = Belt.Array.getUnsafe(clean, i)
      sum := sum.contents + digit * (10 - i)
    }
    Pervasives.mod(sum.contents, 11) == 0
  }
}

/** ISBN-13 validation */
let isValidISBN13 = (isbn: string): bool => {
  // Extract digits only
  let clean = Belt.Array.make(13, 0)
  let idx = ref(0)

  for i in 0 to Js.String2.length(isbn) - 1 {
    if idx.contents < 13 {
      let c = Js.String2.charAt(isbn, i)
      let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

      if code >= 48 && code <= 57 {
        Belt.Array.setUnsafe(clean, idx.contents, code - 48)
        idx := idx.contents + 1
      }
    }
  }

  if idx.contents != 13 {
    false
  } else {
    let sum = ref(0)
    for i in 0 to 12 {
      let digit = Belt.Array.getUnsafe(clean, i)
      let mult = if Pervasives.mod(i, 2) == 0 {
        1
      } else {
        3
      }
      sum := sum.contents + digit * mult
    }
    Pervasives.mod(sum.contents, 10) == 0
  }
}

/** Validate ISBN (automatically detects ISBN-10 or ISBN-13) */
let isValidISBN = (isbn: string): bool => {
  // Count digits (and X for ISBN-10)
  let digitCount = ref(0)
  let hasX = ref(false)

  for i in 0 to Js.String2.length(isbn) - 1 {
    let c = Js.String2.charAt(isbn, i)
    let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt
    if code >= 48 && code <= 57 {
      digitCount := digitCount.contents + 1
    } else if c == "X" || c == "x" {
      hasX := true
    }
  }

  if digitCount.contents == 10 || (digitCount.contents == 9 && hasX.contents) {
    isValidISBN10(isbn)
  } else if digitCount.contents == 13 {
    isValidISBN13(isbn)
  } else {
    false
  }
}

/** XOR checksum of a byte array */
let xorChecksumBytes = (data: array<int>): int => {
  Belt.Array.reduce(data, 0, (acc, byte) => lxor(acc, byte))
}

/** XOR checksum of a string (ASCII) */
let xorChecksum = (data: string): int => {
  let result = ref(0)
  for i in 0 to Js.String2.length(data) - 1 {
    let code = Js.String2.charCodeAt(data, i)->Belt.Float.toInt
    result := lxor(result.contents, land(code, 0xFF))
  }
  result.contents
}

/** Fletcher-16 checksum of a byte array */
let fletcher16Bytes = (data: array<int>): int => {
  let sum1 = ref(0)
  let sum2 = ref(0)

  Belt.Array.forEach(data, byte => {
    sum1 := Pervasives.mod(sum1.contents + byte, 255)
    sum2 := Pervasives.mod(sum2.contents + sum1.contents, 255)
  })

  lor(lsl(sum2.contents, 8), sum1.contents)
}

/** Fletcher-16 checksum of a string (ASCII) */
let fletcher16 = (data: string): int => {
  let bytes = Belt.Array.makeBy(Js.String2.length(data), i => {
    land(Js.String2.charCodeAt(data, i)->Belt.Float.toInt, 0xFF)
  })
  fletcher16Bytes(bytes)
}

/** Fletcher-32 checksum of a byte array */
let fletcher32Bytes = (data: array<int>): int => {
  let sum1 = ref(0)
  let sum2 = ref(0)

  Belt.Array.forEach(data, byte => {
    sum1 := Pervasives.mod(sum1.contents + byte, 65535)
    sum2 := Pervasives.mod(sum2.contents + sum1.contents, 65535)
  })

  lor(lsl(sum2.contents, 16), sum1.contents)
}

/** Fletcher-32 checksum of a string (ASCII) */
let fletcher32 = (data: string): int => {
  let bytes = Belt.Array.makeBy(Js.String2.length(data), i => {
    land(Js.String2.charCodeAt(data, i)->Belt.Float.toInt, 0xFF)
  })
  fletcher32Bytes(bytes)
}

/** Simple sum checksum (mod 256) */
let sumChecksum = (data: string): int => {
  let sum = ref(0)
  for i in 0 to Js.String2.length(data) - 1 {
    let code = Js.String2.charCodeAt(data, i)->Belt.Float.toInt
    sum := sum.contents + land(code, 0xFF)
  }
  land(sum.contents, 0xFF)
}

/** Two's complement checksum */
let twosComplementChecksum = (data: string): int => {
  let sum = sumChecksum(data)
  land(lnot(sum) + 1, 0xFF)
}

/** Verify a checksum by checking if data + checksum sums to 0 (mod 256) */
let verifyChecksum = (data: string, checksum: int): bool => {
  let sum = sumChecksum(data)
  land(sum + checksum, 0xFF) == 0
}

/** Convert checksum to hex string (uppercase) */
let toHex = (value: int, ~width: int=8): string => {
  let hexChars = "0123456789ABCDEF"
  let result = ref("")
  let v = ref(land(value, 0xFFFFFFFF)) // Ensure 32-bit

  for _ in 0 to width - 1 {
    let nibble = land(v.contents, 0xF)
    result := Js.String2.charAt(hexChars, nibble) ++ result.contents
    v := lsr(v.contents, 4)
  }

  result.contents
}

/** Convert checksum to hex string (lowercase) */
let toHexLower = (value: int, ~width: int=8): string => {
  Js.String2.toLowerCase(toHex(value, ~width))
}

/** IBAN validation (International Bank Account Number) */
let isValidIBAN = (iban: string): bool => {
  // Remove spaces and convert to uppercase
  let clean =
    iban
    ->Js.String2.replaceByRe(%re("/\\s/g"), "")
    ->Js.String2.toUpperCase

  let length = Js.String2.length(clean)

  // IBAN must be at least 15 characters
  if length < 15 || length > 34 {
    false
  } else {
    // Move first 4 characters to end
    let rearranged =
      Js.String2.substring(clean, ~from=4, ~to_=length) ++
      Js.String2.substring(clean, ~from=0, ~to_=4)

    // Convert letters to numbers (A=10, B=11, etc.)
    let numericStr = ref("")
    let valid = ref(true)

    for i in 0 to Js.String2.length(rearranged) - 1 {
      if valid.contents {
        let c = Js.String2.charAt(rearranged, i)
        let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

        if code >= 48 && code <= 57 {
          // 0-9
          numericStr := numericStr.contents ++ c
        } else if code >= 65 && code <= 90 {
          // A-Z -> 10-35
          let num = code - 55
          numericStr := numericStr.contents ++ Belt.Int.toString(num)
        } else {
          valid := false
        }
      }
    }

    if !valid.contents {
      false
    } else {
      // Calculate mod 97 using string arithmetic (numbers too large for int)
      let remainder = ref(0)
      for i in 0 to Js.String2.length(numericStr.contents) - 1 {
        let c = Js.String2.charAt(numericStr.contents, i)
        let digit = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt - 48
        remainder := Pervasives.mod(remainder.contents * 10 + digit, 97)
      }

      remainder.contents == 1
    }
  }
}

/** Damm algorithm check digit validation */
let dammCheck = (digits: string): bool => {
  // Damm quasigroup table
  let table = [
    [0, 3, 1, 7, 5, 9, 8, 6, 4, 2],
    [7, 0, 9, 2, 1, 5, 4, 8, 6, 3],
    [4, 2, 0, 6, 8, 7, 1, 3, 5, 9],
    [1, 7, 5, 0, 9, 8, 3, 4, 2, 6],
    [6, 1, 2, 3, 0, 4, 5, 9, 7, 8],
    [3, 6, 7, 4, 2, 0, 9, 5, 8, 1],
    [5, 8, 6, 9, 7, 2, 0, 1, 3, 4],
    [8, 9, 4, 5, 3, 6, 2, 0, 1, 7],
    [9, 4, 3, 8, 6, 1, 7, 2, 0, 5],
    [2, 5, 8, 1, 4, 3, 6, 7, 9, 0],
  ]

  let interim = ref(0)
  let valid = ref(true)

  for i in 0 to Js.String2.length(digits) - 1 {
    if valid.contents {
      let c = Js.String2.charAt(digits, i)
      let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt

      if code < 48 || code > 57 {
        valid := false
      } else {
        let digit = code - 48
        let row = Belt.Array.getUnsafe(table, interim.contents)
        interim := Belt.Array.getUnsafe(row, digit)
      }
    }
  }

  valid.contents && interim.contents == 0
}

/** Verhoeff algorithm check digit validation */
let verhoeffCheck = (digits: string): bool => {
  // Multiplication table
  let d = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    [1, 2, 3, 4, 0, 6, 7, 8, 9, 5],
    [2, 3, 4, 0, 1, 7, 8, 9, 5, 6],
    [3, 4, 0, 1, 2, 8, 9, 5, 6, 7],
    [4, 0, 1, 2, 3, 9, 5, 6, 7, 8],
    [5, 9, 8, 7, 6, 0, 4, 3, 2, 1],
    [6, 5, 9, 8, 7, 1, 0, 4, 3, 2],
    [7, 6, 5, 9, 8, 2, 1, 0, 4, 3],
    [8, 7, 6, 5, 9, 3, 2, 1, 0, 4],
    [9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  ]

  // Permutation table
  let p = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    [1, 5, 7, 6, 2, 8, 3, 0, 9, 4],
    [5, 8, 0, 3, 7, 9, 6, 1, 4, 2],
    [8, 9, 1, 6, 0, 4, 3, 5, 2, 7],
    [9, 4, 5, 3, 1, 2, 6, 8, 7, 0],
    [4, 2, 8, 6, 5, 7, 3, 9, 0, 1],
    [2, 7, 9, 3, 8, 0, 6, 4, 1, 5],
    [7, 0, 4, 6, 9, 1, 3, 2, 5, 8],
  ]

  let c = ref(0)
  let length = Js.String2.length(digits)
  let valid = ref(true)

  // Process from right to left
  for i in 0 to length - 1 {
    if valid.contents {
      let char = Js.String2.charAt(digits, length - 1 - i)
      let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt

      if code < 48 || code > 57 {
        valid := false
      } else {
        let ni = code - 48
        let pRow = Belt.Array.getUnsafe(p, Pervasives.mod(i, 8))
        let pVal = Belt.Array.getUnsafe(pRow, ni)
        let dRow = Belt.Array.getUnsafe(d, c.contents)
        c := Belt.Array.getUnsafe(dRow, pVal)
      }
    }
  }

  valid.contents && c.contents == 0
}
