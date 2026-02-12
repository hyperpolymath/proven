// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe UUID generation, parsing, and validation following RFC 4122.
module SafeUuid =
    open System
    open System.Text.RegularExpressions

    /// UUID version enumeration.
    type UuidVersion =
        | V1    // Time-based
        | V2    // DCE Security
        | V3    // Name-based (MD5)
        | V4    // Random
        | V5    // Name-based (SHA-1)
        | Nil   // Nil UUID (all zeros)
        | Unknown

    /// UUID variant enumeration.
    type UuidVariant =
        | NCS       // Reserved for NCS backward compatibility
        | RFC4122   // Standard RFC 4122 variant
        | Microsoft // Reserved for Microsoft backward compatibility
        | Future    // Reserved for future definition

    /// UUID parsing errors.
    type UuidError =
        | InvalidLength of int
        | InvalidCharacter of char * int
        | InvalidFormat of string
        | InvalidVersion of int
        | InvalidVariant
        | EmptyInput

    /// A validated UUID record.
    type Uuid = {
        Bytes: byte[]
    }

    /// Nil UUID constant (all zeros).
    let nilUuid: Uuid = { Bytes = Array.zeroCreate 16 }

    /// DNS namespace UUID for v3/v5.
    let namespaceDns: Uuid = {
        Bytes = [| 0x6buy; 0xa7uy; 0xb8uy; 0x10uy; 0x9duy; 0xaduy; 0x11uy; 0xd1uy;
                   0x80uy; 0xb4uy; 0x00uy; 0xc0uy; 0x4fuy; 0xd4uy; 0x30uy; 0xc8uy |]
    }

    /// URL namespace UUID for v3/v5.
    let namespaceUrl: Uuid = {
        Bytes = [| 0x6buy; 0xa7uy; 0xb8uy; 0x11uy; 0x9duy; 0xaduy; 0x11uy; 0xd1uy;
                   0x80uy; 0xb4uy; 0x00uy; 0xc0uy; 0x4fuy; 0xd4uy; 0x30uy; 0xc8uy |]
    }

    /// OID namespace UUID for v3/v5.
    let namespaceOid: Uuid = {
        Bytes = [| 0x6buy; 0xa7uy; 0xb8uy; 0x12uy; 0x9duy; 0xaduy; 0x11uy; 0xd1uy;
                   0x80uy; 0xb4uy; 0x00uy; 0xc0uy; 0x4fuy; 0xd4uy; 0x30uy; 0xc8uy |]
    }

    /// X.500 namespace UUID for v3/v5.
    let namespaceX500: Uuid = {
        Bytes = [| 0x6buy; 0xa7uy; 0xb8uy; 0x14uy; 0x9duy; 0xaduy; 0x11uy; 0xd1uy;
                   0x80uy; 0xb4uy; 0x00uy; 0xc0uy; 0x4fuy; 0xd4uy; 0x30uy; 0xc8uy |]
    }

    let private hexCharPattern = Regex(@"^[0-9a-fA-F]+$")
    let private uuidPattern = Regex(@"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")

    /// Check if character is valid hexadecimal.
    let private isHexChar (c: char) : bool =
        (c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F')

    /// Convert hex character to nibble value.
    let private hexCharToNibble (c: char) : int option =
        if c >= '0' && c <= '9' then Some(int c - int '0')
        elif c >= 'a' && c <= 'f' then Some(int c - int 'a' + 10)
        elif c >= 'A' && c <= 'F' then Some(int c - int 'A' + 10)
        else None

    /// Convert nibble to lowercase hex character.
    let private nibbleToHexChar (n: int) : char =
        if n < 10 then char(int '0' + n)
        else char(int 'a' + n - 10)

    /// Format byte as two hex characters.
    let private formatByte (b: byte) : string =
        let n = int b
        sprintf "%c%c" (nibbleToHexChar (n / 16)) (nibbleToHexChar (n % 16))

    /// Get the version of a UUID.
    let getVersion (uuid: Uuid) : UuidVersion =
        if uuid.Bytes.Length < 7 then Unknown
        else
            let versionNibble = (int uuid.Bytes.[6] >>> 4) &&& 0x0F
            match versionNibble with
            | 0 when Array.forall ((=) 0uy) uuid.Bytes -> Nil
            | 1 -> V1
            | 2 -> V2
            | 3 -> V3
            | 4 -> V4
            | 5 -> V5
            | _ -> Unknown

    /// Get the variant of a UUID.
    let getVariant (uuid: Uuid) : UuidVariant =
        if uuid.Bytes.Length < 9 then Future
        else
            let variantByte = int uuid.Bytes.[8]
            if (variantByte >>> 7) = 0 then NCS
            elif (variantByte >>> 6) = 2 then RFC4122
            elif (variantByte >>> 5) = 6 then Microsoft
            else Future

    /// Check if UUID is the nil UUID.
    let isNil (uuid: Uuid) : bool =
        uuid.Bytes.Length = 16 && Array.forall ((=) 0uy) uuid.Bytes

    /// Parse UUID from canonical format (8-4-4-4-12).
    let parse (input: string) : Result<Uuid, UuidError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            let s = input.Trim()
            if s.Length <> 36 then
                Error(InvalidLength s.Length)
            elif not (uuidPattern.IsMatch(s)) then
                // Find first invalid character for better error message
                let mutable invalidChar = None
                for i in 0 .. s.Length - 1 do
                    let c = s.[i]
                    if invalidChar.IsNone then
                        if i = 8 || i = 13 || i = 18 || i = 23 then
                            if c <> '-' then invalidChar <- Some(c, i)
                        else
                            if not (isHexChar c) then invalidChar <- Some(c, i)
                match invalidChar with
                | Some(c, pos) -> Error(InvalidCharacter(c, pos))
                | None -> Error(InvalidFormat "invalid UUID format")
            else
                // Remove hyphens and parse hex
                let hexPart = s.Replace("-", "")
                let bytes = Array.zeroCreate<byte> 16
                let mutable valid = true
                for i in 0 .. 15 do
                    if valid then
                        let h1 = hexCharToNibble hexPart.[i * 2]
                        let h2 = hexCharToNibble hexPart.[i * 2 + 1]
                        match h1, h2 with
                        | Some v1, Some v2 -> bytes.[i] <- byte(v1 * 16 + v2)
                        | _ -> valid <- false
                if valid then Ok { Bytes = bytes }
                else Error(InvalidFormat "hex parsing failed")

    /// Parse UUID from string, returning Option.
    let tryParse (input: string) : Uuid option =
        match parse input with
        | Ok uuid -> Some uuid
        | Error _ -> None

    /// Format UUID in canonical form (8-4-4-4-12 with lowercase).
    let format (uuid: Uuid) : string =
        if uuid.Bytes.Length <> 16 then ""
        else
            let hex = uuid.Bytes |> Array.map formatByte |> String.concat ""
            sprintf "%s-%s-%s-%s-%s"
                (hex.Substring(0, 8))
                (hex.Substring(8, 4))
                (hex.Substring(12, 4))
                (hex.Substring(16, 4))
                (hex.Substring(20, 12))

    /// Format UUID as URN (urn:uuid:...).
    let formatUrn (uuid: Uuid) : string =
        "urn:uuid:" + format uuid

    /// Format UUID without hyphens.
    let formatCompact (uuid: Uuid) : string =
        if uuid.Bytes.Length <> 16 then ""
        else uuid.Bytes |> Array.map formatByte |> String.concat ""

    /// Format UUID with uppercase.
    let formatUppercase (uuid: Uuid) : string =
        (format uuid).ToUpperInvariant()

    /// Validate UUID string format.
    let isValid (input: string) : bool =
        match parse input with
        | Ok _ -> true
        | Error _ -> false

    /// Quick check if string looks like a UUID.
    let looksLikeUuid (input: string) : bool =
        input.Length = 36 &&
        input.[8] = '-' &&
        input.[13] = '-' &&
        input.[18] = '-' &&
        input.[23] = '-'

    /// Create UUID from System.Guid.
    let fromGuid (guid: Guid) : Uuid =
        { Bytes = guid.ToByteArray() }

    /// Convert UUID to System.Guid.
    let toGuid (uuid: Uuid) : Guid option =
        if uuid.Bytes.Length = 16 then Some(Guid(uuid.Bytes))
        else None

    /// Generate a random v4 UUID.
    let generateV4 () : Uuid =
        let bytes = Array.zeroCreate<byte> 16
        use rng = System.Security.Cryptography.RandomNumberGenerator.Create()
        rng.GetBytes(bytes)
        // Set version (4) in byte 6
        bytes.[6] <- (bytes.[6] &&& 0x0Fuy) ||| 0x40uy
        // Set variant (RFC4122) in byte 8
        bytes.[8] <- (bytes.[8] &&& 0x3Fuy) ||| 0x80uy
        { Bytes = bytes }

    /// Generate a v5 UUID from namespace and name using SHA-1.
    let generateV5 (namespace': Uuid) (name: string) : Uuid =
        use sha1 = System.Security.Cryptography.SHA1.Create()
        let nameBytes = System.Text.Encoding.UTF8.GetBytes(name)
        let combined = Array.append namespace'.Bytes nameBytes
        let hash = sha1.ComputeHash(combined)
        let bytes = Array.sub hash 0 16
        // Set version (5) in byte 6
        bytes.[6] <- (bytes.[6] &&& 0x0Fuy) ||| 0x50uy
        // Set variant (RFC4122) in byte 8
        bytes.[8] <- (bytes.[8] &&& 0x3Fuy) ||| 0x80uy
        { Bytes = bytes }

    /// Generate a v3 UUID from namespace and name using MD5.
    let generateV3 (namespace': Uuid) (name: string) : Uuid =
        use md5 = System.Security.Cryptography.MD5.Create()
        let nameBytes = System.Text.Encoding.UTF8.GetBytes(name)
        let combined = Array.append namespace'.Bytes nameBytes
        let hash = md5.ComputeHash(combined)
        let bytes = Array.sub hash 0 16
        // Set version (3) in byte 6
        bytes.[6] <- (bytes.[6] &&& 0x0Fuy) ||| 0x30uy
        // Set variant (RFC4122) in byte 8
        bytes.[8] <- (bytes.[8] &&& 0x3Fuy) ||| 0x80uy
        { Bytes = bytes }

    /// Compare two UUIDs for equality.
    let equals (a: Uuid) (b: Uuid) : bool =
        a.Bytes.Length = b.Bytes.Length &&
        Array.forall2 (=) a.Bytes b.Bytes

    /// Compare two UUIDs.
    let compare (a: Uuid) (b: Uuid) : int =
        let rec compareBytes i =
            if i >= 16 then 0
            elif i >= a.Bytes.Length || i >= b.Bytes.Length then
                Operators.compare a.Bytes.Length b.Bytes.Length
            else
                let cmp = Operators.compare a.Bytes.[i] b.Bytes.[i]
                if cmp <> 0 then cmp else compareBytes (i + 1)
        compareBytes 0
