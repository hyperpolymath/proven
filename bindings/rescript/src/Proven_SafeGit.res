// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeGit - Git reference and object ID validation that cannot crash.
 *
 * Provides validation and manipulation of Git references (branch names, tags)
 * and object identifiers (SHA-1/SHA-256 hashes). All operations are designed
 * to fail safely without panics or undefined behavior.
 */

/** Git object ID types */
type objectIdType =
  | Sha1
  | Sha256

/** Error types for Git operations */
type gitError =
  | EmptyReference
  | InvalidCharacter
  | InvalidStructure
  | ReservedName
  | InvalidLength
  | InvalidHex
  | BufferTooSmall

/** Validated Git object ID */
type objectId = {
  bytes: array<int>,
  idType: objectIdType,
}

/** Reserved Git reference prefixes */
let reservedPrefixes = [
  "refs/heads/",
  "refs/tags/",
  "refs/remotes/",
  "refs/notes/",
  "refs/stash",
]

/** Null SHA-1 object ID (all zeros) */
let nullSha1 = (): objectId => {
  bytes: Belt.Array.make(20, 0),
  idType: Sha1,
}

/** Null SHA-256 object ID (all zeros) */
let nullSha256 = (): objectId => {
  bytes: Belt.Array.make(32, 0),
  idType: Sha256,
}

/** Get the byte length for an object ID type */
let byteLen = (idType: objectIdType): int => {
  switch idType {
  | Sha1 => 20
  | Sha256 => 32
  }
}

/** Get the hex string length for an object ID type */
let hexLen = (idType: objectIdType): int => {
  switch idType {
  | Sha1 => 40
  | Sha256 => 64
  }
}

/** Convert a hex character to its nibble value (0-15) */
let hexCharToNibble = (char: string): option<int> => {
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

/** Parse a Git object ID from hex string */
let parseObjectId = (hexString: string): result<objectId, gitError> => {
  let length = Js.String2.length(hexString)

  let idType = switch length {
  | 40 => Some(Sha1)
  | 64 => Some(Sha256)
  | _ => None
  }

  switch idType {
  | None => Error(InvalidLength)
  | Some(idType) =>
    let byteLength = byteLen(idType)
    let bytes = Belt.Array.make(byteLength, 0)
    let valid = ref(true)

    for i in 0 to byteLength - 1 {
      if valid.contents {
        let highChar = Js.String2.charAt(hexString, i * 2)
        let lowChar = Js.String2.charAt(hexString, i * 2 + 1)
        switch (hexCharToNibble(highChar), hexCharToNibble(lowChar)) {
        | (Some(high), Some(low)) =>
          Belt.Array.setUnsafe(bytes, i, lsl(high, 4) + low)
        | _ => valid := false
        }
      }
    }

    if valid.contents {
      Ok({bytes, idType})
    } else {
      Error(InvalidHex)
    }
  }
}

/** Check if a string is a valid Git object ID */
let isValidObjectId = (hexString: string): bool => {
  switch parseObjectId(hexString) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Check if an object ID is null (all zeros) */
let isNullObjectId = (oid: objectId): bool => {
  Belt.Array.every(oid.bytes, byte => byte == 0)
}

/** Get the raw bytes from an object ID */
let getBytes = (oid: objectId): array<int> => oid.bytes

/** Format an object ID as a lowercase hex string */
let formatObjectId = (oid: objectId): string => {
  let hexChars = "0123456789abcdef"
  let result = ref("")

  Belt.Array.forEach(oid.bytes, byte => {
    let high = lsr(byte, 4)
    let low = land(byte, 0x0f)
    result :=
      result.contents ++ Js.String2.charAt(hexChars, high) ++ Js.String2.charAt(hexChars, low)
  })

  result.contents
}

/** Get an abbreviated form of an object ID (first n characters) */
let abbreviateObjectId = (oid: objectId, length: int): string => {
  let fullHex = formatObjectId(oid)
  let maxLen = hexLen(oid.idType)
  let actualLen = if length > maxLen {
    maxLen
  } else if length < 1 {
    1
  } else {
    length
  }
  Js.String2.slice(fullHex, ~from=0, ~to_=actualLen)
}

/** Compare two object IDs for equality */
let equalObjectId = (oidA: objectId, oidB: objectId): bool => {
  if oidA.idType != oidB.idType {
    false
  } else {
    Belt.Array.eq(oidA.bytes, oidB.bytes, (a, b) => a == b)
  }
}

/** Constant-time comparison of two object IDs (for security-sensitive contexts) */
let constantTimeEqualObjectId = (oidA: objectId, oidB: objectId): bool => {
  if oidA.idType != oidB.idType {
    false
  } else {
    let diff = ref(0)
    Belt.Array.forEachWithIndex(oidA.bytes, (i, byteA) => {
      let byteB = Belt.Array.getUnsafe(oidB.bytes, i)
      diff := lor(diff.contents, lxor(byteA, byteB))
    })
    diff.contents == 0
  }
}

/** Check if a character is valid in a Git reference name */
let isValidRefChar = (char: string): bool => {
  let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
  // Control characters and special forbidden characters
  if code >= 0 && code <= 31 || code == 127 {
    false
  } else {
    switch char {
    | " " | "~" | "^" | ":" | "?" | "*" | "[" | "\\" => false
    | _ => true
    }
  }
}

/** Validate a Git reference name (branch name, tag name, etc.) */
let isValidRefName = (referenceName: string): bool => {
  let length = Js.String2.length(referenceName)
  if length == 0 {
    false
  } else {
    let firstChar = Js.String2.charAt(referenceName, 0)
    let lastChar = Js.String2.charAt(referenceName, length - 1)

    // Cannot start or end with a slash
    if firstChar == "/" || lastChar == "/" {
      false
    } else if firstChar == "." || lastChar == "." {
      // Cannot start or end with a dot
      false
    } else if Js.String2.endsWith(referenceName, ".lock") {
      // Cannot end with .lock
      false
    } else if referenceName == "@" {
      // Cannot be just @
      false
    } else if Js.String2.includes(referenceName, "@{") {
      // Cannot contain @{
      false
    } else {
      // Check for invalid characters and consecutive dots/slashes
      let valid = ref(true)
      let prevChar = ref("")

      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(referenceName, i)
          if !isValidRefChar(char) {
            valid := false
          } else if char == "." && prevChar.contents == "." {
            // Consecutive dots
            valid := false
          } else if char == "/" && prevChar.contents == "/" {
            // Consecutive slashes
            valid := false
          } else if char == "." && prevChar.contents == "/" {
            // Slash followed by dot
            valid := false
          }
          prevChar := char
        }
      }

      valid.contents
    }
  }
}

/** Validate a Git branch name (stricter than general reference) */
let isValidBranchName = (branchName: string): bool => {
  if !isValidRefName(branchName) {
    false
  } else {
    // Branch names cannot start with a hyphen
    let length = Js.String2.length(branchName)
    if length > 0 && Js.String2.charAt(branchName, 0) == "-" {
      false
    } else {
      true
    }
  }
}

/** Validate a Git tag name */
let isValidTagName = (tagName: string): bool => {
  isValidRefName(tagName)
}

/** Sanitize a string to be a valid Git reference name */
let sanitizeRefName = (input: string): result<string, gitError> => {
  let length = Js.String2.length(input)
  if length == 0 {
    Error(EmptyReference)
  } else {
    let result = ref("")
    let prevChar = ref("")
    let skipLeadingDots = ref(true)

    for i in 0 to length - 1 {
      let char = Js.String2.charAt(input, i)

      // Skip leading dots
      if skipLeadingDots.contents && char == "." {
        ()
      } else {
        skipLeadingDots := false

        // Replace invalid characters with hyphen
        let sanitizedChar = if isValidRefChar(char) && char != "/" {
          char
        } else {
          "-"
        }

        // Skip consecutive hyphens
        if sanitizedChar == "-" && prevChar.contents == "-" {
          ()
        } else if sanitizedChar == "." && prevChar.contents == "." {
          // Skip consecutive dots
          ()
        } else {
          result := result.contents ++ sanitizedChar
          prevChar := sanitizedChar
        }
      }
    }

    // Remove trailing dots and hyphens
    let trimmed = ref(result.contents)
    while Js.String2.length(trimmed.contents) > 0 && {
        let last = Js.String2.charAt(
          trimmed.contents,
          Js.String2.length(trimmed.contents) - 1,
        )
        last == "." || last == "-"
      } {
      trimmed := Js.String2.slice(trimmed.contents, ~from=0, ~to_=-1)
    }

    // Remove .lock suffix if present
    if Js.String2.endsWith(trimmed.contents, ".lock") {
      trimmed := Js.String2.slice(trimmed.contents, ~from=0, ~to_=-5)
    }

    if Js.String2.length(trimmed.contents) == 0 {
      Error(InvalidStructure)
    } else {
      Ok(trimmed.contents)
    }
  }
}

/** Normalize a reference name (lowercase for comparison) */
let normalizeRefName = (referenceName: string): result<string, gitError> => {
  if Js.String2.length(referenceName) == 0 {
    Error(EmptyReference)
  } else {
    Ok(Js.String2.toLowerCase(referenceName))
  }
}

/** Check if a reference name uses a reserved prefix */
let hasReservedPrefix = (referenceName: string): bool => {
  Belt.Array.some(reservedPrefixes, prefix => Js.String2.startsWith(referenceName, prefix))
}

/** Extract the short name from a fully qualified reference */
let shortName = (referenceName: string): string => {
  let found = Belt.Array.getBy(reservedPrefixes, prefix =>
    Js.String2.startsWith(referenceName, prefix)
  )
  switch found {
  | Some(prefix) => Js.String2.sliceToEnd(referenceName, ~from=Js.String2.length(prefix))
  | None => referenceName
  }
}

/** Object ID type to string representation */
let objectIdTypeToString = (idType: objectIdType): string => {
  switch idType {
  | Sha1 => "SHA-1"
  | Sha256 => "SHA-256"
  }
}

/** Git error to string representation */
let gitErrorToString = (error: gitError): string => {
  switch error {
  | EmptyReference => "Reference name is empty"
  | InvalidCharacter => "Reference contains invalid characters"
  | InvalidStructure => "Reference has invalid structure"
  | ReservedName => "Reference name is reserved"
  | InvalidLength => "Object ID has invalid length"
  | InvalidHex => "Object ID contains non-hexadecimal characters"
  | BufferTooSmall => "Buffer is too small for the operation"
  }
}
