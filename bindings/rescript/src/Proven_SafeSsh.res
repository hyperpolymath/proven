// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeSsh - SSH key and fingerprint validation that cannot crash.
 *
 * Provides utilities for validating SSH public keys, parsing fingerprints,
 * and working with known_hosts entries. Designed for security-conscious
 * SSH key management.
 */

/** SSH key types */
type keyType =
  | Rsa
  | Dsa
  | Ecdsa256
  | Ecdsa384
  | Ecdsa521
  | Ed25519
  | Ed25519Sk
  | EcdsaSk

/** Fingerprint hash algorithm */
type fingerprintAlgorithm =
  | Md5
  | Sha256

/** Error types for SSH operations */
type sshError =
  | InvalidKeyFormat
  | InvalidKeyType
  | InvalidBase64
  | InvalidFingerprint
  | InvalidKnownHostsEntry
  | KeyTooShort
  | KeyTooLong
  | UnsupportedAlgorithm

/** Parsed SSH public key */
type publicKey = {
  keyType: keyType,
  keyData: string,
  comment: option<string>,
}

/** Parsed SSH fingerprint */
type fingerprint = {
  algorithm: fingerprintAlgorithm,
  hash: string,
  keyType: option<keyType>,
}

/** Parsed known_hosts entry */
type knownHostsEntry = {
  hostnames: string,
  keyType: keyType,
  keyData: string,
  comment: option<string>,
  isHashed: bool,
}

/** Key type to algorithm string mapping */
let keyTypeToAlgorithm = (keyType: keyType): string => {
  switch keyType {
  | Rsa => "ssh-rsa"
  | Dsa => "ssh-dss"
  | Ecdsa256 => "ecdsa-sha2-nistp256"
  | Ecdsa384 => "ecdsa-sha2-nistp384"
  | Ecdsa521 => "ecdsa-sha2-nistp521"
  | Ed25519 => "ssh-ed25519"
  | Ed25519Sk => "sk-ssh-ed25519@openssh.com"
  | EcdsaSk => "sk-ecdsa-sha2-nistp256@openssh.com"
  }
}

/** Get minimum recommended key size in bits for a key type */
let minKeyBits = (keyType: keyType): int => {
  switch keyType {
  | Rsa => 2048
  | Dsa => 1024
  | Ecdsa256 => 256
  | Ecdsa384 => 384
  | Ecdsa521 => 521
  | Ed25519 => 256
  | Ed25519Sk => 256
  | EcdsaSk => 256
  }
}

/** Check if a key type is considered secure */
let isKeyTypeSecure = (keyType: keyType): bool => {
  switch keyType {
  | Dsa => false // DSA is deprecated
  | _ => true
  }
}

/** Check if a key type is recommended for new keys */
let isRecommendedKeyType = (keyType: keyType): bool => {
  switch keyType {
  | Ed25519 => true
  | Ecdsa256 | Ecdsa384 | Ecdsa521 => true
  | Rsa => true // Still acceptable with >= 3072 bits
  | Dsa => false // Deprecated
  | Ed25519Sk | EcdsaSk => true // Security keys
  }
}

/** Parse algorithm string to key type */
let parseKeyType = (algorithm: string): result<keyType, sshError> => {
  switch algorithm {
  | "ssh-rsa" => Ok(Rsa)
  | "ssh-dss" => Ok(Dsa)
  | "ecdsa-sha2-nistp256" => Ok(Ecdsa256)
  | "ecdsa-sha2-nistp384" => Ok(Ecdsa384)
  | "ecdsa-sha2-nistp521" => Ok(Ecdsa521)
  | "ssh-ed25519" => Ok(Ed25519)
  | "sk-ssh-ed25519@openssh.com" => Ok(Ed25519Sk)
  | "sk-ecdsa-sha2-nistp256@openssh.com" => Ok(EcdsaSk)
  | _ => Error(UnsupportedAlgorithm)
  }
}

/** Base64 character set */
let base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

/** Check if a string is valid base64 */
let isValidBase64 = (data: string): bool => {
  let length = Js.String2.length(data)
  if length == 0 {
    false
  } else {
    let valid = ref(true)
    for i in 0 to length - 1 {
      if valid.contents {
        let char = Js.String2.charAt(data, i)
        if !Js.String2.includes(base64Chars, char) {
          valid := false
        }
      }
    }
    valid.contents
  }
}

/** Validate and parse an SSH public key string */
let parsePublicKey = (keyString: string): result<publicKey, sshError> => {
  let length = Js.String2.length(keyString)
  if length < 16 {
    Error(KeyTooShort)
  } else if length > 16384 {
    Error(KeyTooLong)
  } else {
    let parts = Js.String2.split(keyString, " ")
    let numParts = Belt.Array.length(parts)

    if numParts < 2 {
      Error(InvalidKeyFormat)
    } else {
      let algorithm = Belt.Array.getUnsafe(parts, 0)
      let keyData = Belt.Array.getUnsafe(parts, 1)

      switch parseKeyType(algorithm) {
      | Error(e) => Error(e)
      | Ok(keyType) =>
        if !isValidBase64(keyData) {
          Error(InvalidBase64)
        } else {
          let comment = if numParts >= 3 {
            Some(Belt.Array.getUnsafe(parts, 2))
          } else {
            None
          }
          Ok({keyType, keyData, comment})
        }
      }
    }
  }
}

/** Check if a string is a valid SSH public key */
let isValidPublicKey = (keyString: string): bool => {
  switch parsePublicKey(keyString) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Check if a public key meets minimum security requirements */
let isPublicKeySecure = (key: publicKey): bool => {
  isKeyTypeSecure(key.keyType)
}

/** Fingerprint algorithm prefix */
let fingerprintPrefix = (algorithm: fingerprintAlgorithm): string => {
  switch algorithm {
  | Md5 => "MD5:"
  | Sha256 => "SHA256:"
  }
}

/** Validate MD5 fingerprint format (xx:xx:xx:...) */
let isValidMd5Fingerprint = (fingerprint: string): bool => {
  let length = Js.String2.length(fingerprint)
  if length != 47 {
    // 16 bytes * 2 hex + 15 colons
    false
  } else {
    let valid = ref(true)
    for i in 0 to length - 1 {
      if valid.contents {
        let char = Js.String2.charAt(fingerprint, i)
        if mod(i, 3) == 2 {
          if char != ":" {
            valid := false
          }
        } else {
          let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
          let isHex =
            (code >= 48 && code <= 57) ||
            (code >= 65 && code <= 70) ||
            (code >= 97 && code <= 102)
          if !isHex {
            valid := false
          }
        }
      }
    }
    valid.contents
  }
}

/** Parse an SSH fingerprint string */
let parseFingerprint = (fingerprintStr: string): result<fingerprint, sshError> => {
  let length = Js.String2.length(fingerprintStr)
  if length < 8 {
    Error(InvalidFingerprint)
  } else if Js.String2.startsWith(fingerprintStr, "SHA256:") {
    let hash = Js.String2.sliceToEnd(fingerprintStr, ~from=7)
    if !isValidBase64(hash) {
      Error(InvalidFingerprint)
    } else {
      Ok({algorithm: Sha256, hash, keyType: None})
    }
  } else if Js.String2.startsWith(fingerprintStr, "MD5:") {
    let hash = Js.String2.sliceToEnd(fingerprintStr, ~from=4)
    if !isValidMd5Fingerprint(hash) {
      Error(InvalidFingerprint)
    } else {
      Ok({algorithm: Md5, hash, keyType: None})
    }
  } else if isValidMd5Fingerprint(fingerprintStr) {
    // Legacy MD5 format without prefix
    Ok({algorithm: Md5, hash: fingerprintStr, keyType: None})
  } else {
    Error(InvalidFingerprint)
  }
}

/** Check if a string is a valid SSH fingerprint */
let isValidFingerprint = (fingerprintStr: string): bool => {
  switch parseFingerprint(fingerprintStr) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Format a fingerprint as a string */
let formatFingerprint = (fp: fingerprint): string => {
  fingerprintPrefix(fp.algorithm) ++ fp.hash
}

/** Parse a known_hosts entry */
let parseKnownHostsEntry = (entry: string): result<knownHostsEntry, sshError> => {
  let length = Js.String2.length(entry)
  if length == 0 {
    Error(InvalidKnownHostsEntry)
  } else if Js.String2.charAt(entry, 0) == "#" {
    // Skip comment lines
    Error(InvalidKnownHostsEntry)
  } else {
    let parts = Js.String2.split(entry, " ")
    let numParts = Belt.Array.length(parts)

    if numParts < 3 {
      Error(InvalidKnownHostsEntry)
    } else {
      let hostnames = Belt.Array.getUnsafe(parts, 0)
      let algorithm = Belt.Array.getUnsafe(parts, 1)
      let keyData = Belt.Array.getUnsafe(parts, 2)

      let isHashed = Js.String2.startsWith(hostnames, "|1|")

      switch parseKeyType(algorithm) {
      | Error(e) => Error(e)
      | Ok(keyType) =>
        if !isValidBase64(keyData) {
          Error(InvalidBase64)
        } else {
          let comment = if numParts >= 4 {
            Some(Belt.Array.getUnsafe(parts, 3))
          } else {
            None
          }
          Ok({hostnames, keyType, keyData, comment, isHashed})
        }
      }
    }
  }
}

/** Check if a known_hosts entry is valid */
let isValidKnownHostsEntry = (entry: string): bool => {
  switch parseKnownHostsEntry(entry) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Validate a hostname for SSH (basic check) */
let isValidSshHostname = (hostname: string): bool => {
  let length = Js.String2.length(hostname)
  if length == 0 || length > 253 {
    false
  } else {
    let firstChar = Js.String2.charAt(hostname, 0)
    let lastChar = Js.String2.charAt(hostname, length - 1)

    // Cannot start or end with hyphen
    if firstChar == "-" || lastChar == "-" {
      false
    } else {
      // Check for valid characters
      let valid = ref(true)
      for i in 0 to length - 1 {
        if valid.contents {
          let char = Js.String2.charAt(hostname, i)
          let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
          let isAlnum =
            (code >= 48 && code <= 57) ||
            (code >= 65 && code <= 90) ||
            (code >= 97 && code <= 122)
          if !isAlnum && char != "." && char != "-" && char != "_" {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Validate an SSH port number */
let isValidSshPort = (port: int): bool => {
  port > 0 && port <= 65535
}

/** Format a host:port string */
let formatHostPort = (hostname: string, port: int): result<string, sshError> => {
  if !isValidSshHostname(hostname) {
    Error(InvalidKeyFormat)
  } else if !isValidSshPort(port) {
    Error(InvalidKeyFormat)
  } else if port == 22 {
    Ok(hostname)
  } else if Js.String2.includes(hostname, ":") {
    // IPv6 address
    Ok(`[${hostname}]:${Belt.Int.toString(port)}`)
  } else {
    Ok(`${hostname}:${Belt.Int.toString(port)}`)
  }
}

/** Sanitize a key comment (remove potentially dangerous characters) */
let sanitizeComment = (comment: string): string => {
  let length = Js.String2.length(comment)
  let result = ref("")

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(comment, i)
    let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
    let isAlnum =
      (code >= 48 && code <= 57) || (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
    // Allow alphanumeric, space, @, ., -, _
    if isAlnum || char == " " || char == "@" || char == "." || char == "-" || char == "_" {
      result := result.contents ++ char
    }
  }

  result.contents
}

/** Key type to human-readable string */
let keyTypeToString = (keyType: keyType): string => {
  switch keyType {
  | Rsa => "RSA"
  | Dsa => "DSA"
  | Ecdsa256 => "ECDSA-256"
  | Ecdsa384 => "ECDSA-384"
  | Ecdsa521 => "ECDSA-521"
  | Ed25519 => "Ed25519"
  | Ed25519Sk => "Ed25519-SK"
  | EcdsaSk => "ECDSA-SK"
  }
}

/** SSH error to string representation */
let sshErrorToString = (error: sshError): string => {
  switch error {
  | InvalidKeyFormat => "Invalid key format"
  | InvalidKeyType => "Invalid key type"
  | InvalidBase64 => "Invalid base64 encoding"
  | InvalidFingerprint => "Invalid fingerprint format"
  | InvalidKnownHostsEntry => "Invalid known_hosts entry"
  | KeyTooShort => "Key is too short"
  | KeyTooLong => "Key is too long"
  | UnsupportedAlgorithm => "Unsupported algorithm"
  }
}
