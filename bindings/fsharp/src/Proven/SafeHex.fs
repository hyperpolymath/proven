// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe hexadecimal encoding, decoding, and manipulation utilities.
module SafeHex =
    open System
    open System.Text

    /// Hex encoding/decoding errors.
    type HexError =
        | InvalidCharacter of char * int  // Invalid char at position
        | OddLength of int                // Hex string has odd length
        | EmptyInput
        | DecodingFailed of string

    /// A validated hexadecimal string.
    type HexString = private { Value: string }

    /// Get the raw string value from HexString.
    let toString (hex: HexString) : string = hex.Value

    /// Check if character is valid hexadecimal (0-9, a-f, A-F).
    let isHexChar (c: char) : bool =
        (c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F')

    /// Convert hex character to numeric value (0-15).
    let hexCharToNibble (c: char) : int option =
        if c >= '0' && c <= '9' then Some(int c - int '0')
        elif c >= 'a' && c <= 'f' then Some(int c - int 'a' + 10)
        elif c >= 'A' && c <= 'F' then Some(int c - int 'A' + 10)
        else None

    /// Convert nibble (0-15) to lowercase hex character.
    let nibbleToHexChar (n: int) : char =
        if n < 10 then char(int '0' + n)
        else char(int 'a' + n - 10)

    /// Convert nibble (0-15) to uppercase hex character.
    let nibbleToHexCharUpper (n: int) : char =
        if n < 10 then char(int '0' + n)
        else char(int 'A' + n - 10)

    /// Validate a string as hexadecimal.
    let validate (input: string) : Result<HexString, HexError> =
        if String.IsNullOrEmpty(input) then
            Error EmptyInput
        else
            let mutable invalidChar = None
            for i in 0 .. input.Length - 1 do
                if invalidChar.IsNone && not (isHexChar input.[i]) then
                    invalidChar <- Some(input.[i], i)
            match invalidChar with
            | Some(c, pos) -> Error(InvalidCharacter(c, pos))
            | None -> Ok { Value = input }

    /// Validate hex string and check for even length (required for byte decoding).
    let validateBytes (input: string) : Result<HexString, HexError> =
        let len = input.Length
        if len % 2 <> 0 then
            Error(OddLength len)
        else
            validate input

    /// Check if string is valid hex.
    let isValidHex (input: string) : bool =
        match validate input with
        | Ok _ -> true
        | Error _ -> false

    /// Check if string is valid hex with even length.
    let isValidHexBytes (input: string) : bool =
        match validateBytes input with
        | Ok _ -> true
        | Error _ -> false

    /// Encode a single byte to two hex characters (lowercase).
    let encodeByte (b: byte) : string =
        let n = int b
        sprintf "%c%c" (nibbleToHexChar (n / 16)) (nibbleToHexChar (n % 16))

    /// Encode a single byte to two hex characters (uppercase).
    let encodeByteUpper (b: byte) : string =
        let n = int b
        sprintf "%c%c" (nibbleToHexCharUpper (n / 16)) (nibbleToHexCharUpper (n % 16))

    /// Encode bytes to hex string (lowercase).
    let encode (bytes: byte[]) : HexString =
        let sb = StringBuilder(bytes.Length * 2)
        for b in bytes do
            sb.Append(encodeByte b) |> ignore
        { Value = sb.ToString() }

    /// Encode bytes to hex string (uppercase).
    let encodeUpper (bytes: byte[]) : HexString =
        let sb = StringBuilder(bytes.Length * 2)
        for b in bytes do
            sb.Append(encodeByteUpper b) |> ignore
        { Value = sb.ToString() }

    /// Decode two hex characters to a byte.
    let private decodeHexByte (h1: char) (h2: char) : byte option =
        match hexCharToNibble h1, hexCharToNibble h2 with
        | Some v1, Some v2 -> Some(byte(v1 * 16 + v2))
        | _ -> None

    /// Decode hex string to bytes.
    let decode (hex: HexString) : Result<byte[], HexError> =
        let s = hex.Value
        let len = s.Length
        if len % 2 <> 0 then
            Error(OddLength len)
        else
            let bytes = Array.zeroCreate<byte>(len / 2)
            let mutable valid = true
            let mutable i = 0
            while valid && i < len / 2 do
                match decodeHexByte s.[i * 2] s.[i * 2 + 1] with
                | Some b -> bytes.[i] <- b
                | None -> valid <- false
                i <- i + 1
            if valid then Ok bytes
            else Error(DecodingFailed "invalid hex characters")

    /// Decode hex string to bytes, returning Option.
    let tryDecode (hex: HexString) : byte[] option =
        match decode hex with
        | Ok bytes -> Some bytes
        | Error _ -> None

    /// Parse and decode hex string in one step.
    let parseHex (input: string) : Result<byte[], HexError> =
        match validateBytes input with
        | Ok hex -> decode hex
        | Error e -> Error e

    /// Parse and decode hex string, returning Option.
    let tryParseHex (input: string) : byte[] option =
        match parseHex input with
        | Ok bytes -> Some bytes
        | Error _ -> None

    /// Get length of hex string in characters.
    let length (hex: HexString) : int = hex.Value.Length

    /// Get number of bytes represented by hex string.
    let byteCount (hex: HexString) : int = hex.Value.Length / 2

    /// Convert hex string to lowercase.
    let toLower (hex: HexString) : HexString =
        { Value = hex.Value.ToLowerInvariant() }

    /// Convert hex string to uppercase.
    let toUpper (hex: HexString) : HexString =
        { Value = hex.Value.ToUpperInvariant() }

    /// Concatenate two hex strings.
    let concat (a: HexString) (b: HexString) : HexString =
        { Value = a.Value + b.Value }

    /// Take first n bytes from hex string (2n characters).
    let take (n: int) (hex: HexString) : HexString =
        let chars = min (n * 2) hex.Value.Length
        { Value = hex.Value.Substring(0, chars) }

    /// Drop first n bytes from hex string (2n characters).
    let drop (n: int) (hex: HexString) : HexString =
        let chars = min (n * 2) hex.Value.Length
        { Value = hex.Value.Substring(chars) }

    /// Format hex string with spaces between bytes (e.g., "01 02 03").
    let formatSpaced (hex: HexString) : string =
        let s = hex.Value
        let sb = StringBuilder()
        for i in 0 .. 2 .. s.Length - 1 do
            if i > 0 then sb.Append(' ') |> ignore
            if i + 1 < s.Length then
                sb.Append(s.[i]).Append(s.[i + 1]) |> ignore
            else
                sb.Append(s.[i]) |> ignore
        sb.ToString()

    /// Format hex string with colons between bytes (e.g., "01:02:03").
    let formatColons (hex: HexString) : string =
        let s = hex.Value
        let sb = StringBuilder()
        for i in 0 .. 2 .. s.Length - 1 do
            if i > 0 then sb.Append(':') |> ignore
            if i + 1 < s.Length then
                sb.Append(s.[i]).Append(s.[i + 1]) |> ignore
            else
                sb.Append(s.[i]) |> ignore
        sb.ToString()

    /// Format hex string with 0x prefix (e.g., "0x0102").
    let format0x (hex: HexString) : string =
        "0x" + hex.Value

    /// Format hex string as C array literal.
    let formatCArray (hex: HexString) : string =
        match tryDecode hex with
        | None -> "{ /* invalid */ }"
        | Some bytes ->
            let formatted = bytes |> Array.map (fun b -> "0x" + encodeByteUpper b)
            "{ " + String.Join(", ", formatted) + " }"

    /// Check equality of two hex strings (case-insensitive).
    let equals (a: HexString) (b: HexString) : bool =
        String.Equals(a.Value, b.Value, StringComparison.OrdinalIgnoreCase)

    /// Constant-time comparison of hex strings (timing-attack resistant).
    let constantTimeEquals (a: HexString) (b: HexString) : bool =
        let la = a.Value.ToLowerInvariant()
        let lb = b.Value.ToLowerInvariant()
        if la.Length <> lb.Length then false
        else
            let mutable acc = 0
            for i in 0 .. la.Length - 1 do
                acc <- acc ||| (int la.[i] ^^^ int lb.[i])
            acc = 0

    /// Constant-time comparison of byte arrays (timing-attack resistant).
    let constantTimeEqualsBytes (a: byte[]) (b: byte[]) : bool =
        if a.Length <> b.Length then false
        else
            let mutable acc = 0
            for i in 0 .. a.Length - 1 do
                acc <- acc ||| (int a.[i] ^^^ int b.[i])
            acc = 0

    /// Convert integer to hex string with minimum width.
    let intToHex (n: int64) (minWidth: int) : HexString =
        let absN = abs n
        let hex = absN.ToString("x")
        let padded =
            if hex.Length >= minWidth then hex
            else String('0', minWidth - hex.Length) + hex
        { Value = padded }

    /// Parse hex string to integer.
    let hexToInt (hex: HexString) : int64 =
        let s = hex.Value.ToLowerInvariant()
        let mutable result = 0L
        for c in s do
            match hexCharToNibble c with
            | Some n -> result <- result * 16L + int64 n
            | None -> ()  // Should not happen with validated HexString
        result

    /// Create empty hex string.
    let empty : HexString = { Value = "" }

    /// Check if hex string is empty.
    let isEmpty (hex: HexString) : bool = String.IsNullOrEmpty(hex.Value)

    /// Create HexString from validated string (internal use).
    let private fromValidatedString (s: string) : HexString = { Value = s }

    /// Encode a string (UTF-8) to hex.
    let encodeString (input: string) : HexString =
        encode (Encoding.UTF8.GetBytes(input))

    /// Decode hex to string (UTF-8).
    let decodeToString (hex: HexString) : Result<string, HexError> =
        match decode hex with
        | Ok bytes ->
            try
                Ok(Encoding.UTF8.GetString(bytes))
            with
            | _ -> Error(DecodingFailed "invalid UTF-8 sequence")
        | Error e -> Error e

    /// Try to decode hex to string (UTF-8).
    let tryDecodeToString (hex: HexString) : string option =
        match decodeToString hex with
        | Ok s -> Some s
        | Error _ -> None
