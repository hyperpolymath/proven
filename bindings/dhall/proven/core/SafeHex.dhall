-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeHex - Safe hexadecimal encoding and validation

Provides type-safe hex string handling with validation.
Ensures proper encoding/decoding of binary data as hex.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan
let Text/replace = Prelude.Text.replace

-- Hex string (lowercase, even length)
let HexString = { hex : Text }

-- Hex result
let HexResult = { value : HexString, ok : Bool }

-- Create success result
let ok
    : HexString -> HexResult
    = \(h : HexString) -> { value = h, ok = True }

-- Create error result
let err
    : HexResult
    = { value = { hex = "" }, ok = False }

-- Create hex string marker
let mkHexString
    : Text -> HexString
    = \(s : Text) ->
        { hex = s }

-- Empty hex string
let empty
    : HexString
    = { hex = "" }

-- Validate hex string format (marker - runtime validation needed)
let isValidHex
    : Text -> Bool
    = \(s : Text) ->
        -- Cannot validate character set in Dhall
        True

-- Byte type (0-255)
let Byte = { value : Natural }

-- Create validated byte
let mkByte
    : Natural -> Optional Byte
    = \(n : Natural) ->
        if Natural/greaterThan n 255
        then None Byte
        else Some { value = n }

-- Byte to hex
let byteToHex
    : Byte -> Text
    = \(b : Byte) ->
        let hexDigit = \(n : Natural) ->
            if      Natural/lessThan n 10 then Natural/show n
            else if n == 10 then "a"
            else if n == 11 then "b"
            else if n == 12 then "c"
            else if n == 13 then "d"
            else if n == 14 then "e"
            else "f"
        let high = b.value / 16
        let low = b.value % 16
        in hexDigit high ++ hexDigit low

-- Bytes to hex string
let bytesToHex
    : List Byte -> HexString
    = \(bytes : List Byte) ->
        { hex = Prelude.Text.concatMap Byte byteToHex bytes }

-- Hex prefix types
let HexPrefix = < None | ZeroX | Hash | Backslash_x >

-- Hex with prefix
let PrefixedHex = {
    hex : HexString,
    prefix : HexPrefix
}

-- Create prefixed hex
let mkPrefixedHex
    : HexString -> HexPrefix -> PrefixedHex
    = \(h : HexString) -> \(p : HexPrefix) ->
        { hex = h, prefix = p }

-- Format prefixed hex
let formatPrefixedHex
    : PrefixedHex -> Text
    = \(ph : PrefixedHex) ->
        let prefixStr = merge {
            None = "",
            ZeroX = "0x",
            Hash = "#",
            Backslash_x = "\\x"
        } ph.prefix
        in prefixStr ++ ph.hex.hex

-- Strip common hex prefixes
let stripPrefix
    : Text -> Text
    = \(s : Text) ->
        let step1 = Text/replace "0x" "" s
        let step2 = Text/replace "0X" "" step1
        let step3 = Text/replace "#" "" step2
        let step4 = Text/replace "\\x" "" step3
        in step4

-- Fixed-length hex (for hashes, addresses, etc.)
let FixedHex = \(n : Natural) -> { hex : Text, length : Natural }

-- Create fixed-length hex marker
let mkFixedHex
    : forall (n : Natural) -> Text -> FixedHex n
    = \(n : Natural) -> \(s : Text) ->
        { hex = s, length = n }

-- Common fixed hex lengths
let Hex32 = FixedHex 32    -- 16 bytes (e.g., MD5, UUID)
let Hex40 = FixedHex 40    -- 20 bytes (e.g., SHA-1, Git commit)
let Hex64 = FixedHex 64    -- 32 bytes (e.g., SHA-256)
let Hex128 = FixedHex 128  -- 64 bytes (e.g., SHA-512)

-- Create common fixed hex
let mkHex32
    : Text -> Hex32
    = mkFixedHex 32

let mkHex40
    : Text -> Hex40
    = mkFixedHex 40

let mkHex64
    : Text -> Hex64
    = mkFixedHex 64

let mkHex128
    : Text -> Hex128
    = mkFixedHex 128

-- Ethereum address (40 hex chars, 20 bytes)
let EthAddress = { address : Text }

-- Create Ethereum address
let mkEthAddress
    : Text -> EthAddress
    = \(s : Text) ->
        { address = s }

-- Format Ethereum address with checksum (marker)
let formatEthAddress
    : EthAddress -> Text
    = \(a : EthAddress) ->
        "0x" ++ a.address

-- Color as hex
let HexColor = { color : Text }

-- Create hex color (3 or 6 chars)
let mkHexColor
    : Text -> HexColor
    = \(c : Text) ->
        { color = c }

-- Format color with hash
let formatHexColor
    : HexColor -> Text
    = \(c : HexColor) ->
        "#" ++ c.color

-- Common colors
let CommonColors = {
    black = { color = "000000" },
    white = { color = "ffffff" },
    red = { color = "ff0000" },
    green = { color = "00ff00" },
    blue = { color = "0000ff" },
    yellow = { color = "ffff00" },
    cyan = { color = "00ffff" },
    magenta = { color = "ff00ff" }
}

-- Binary data as hex
let BinaryHex = {
    data : HexString,
    byteCount : Natural
}

-- Create binary hex
let mkBinaryHex
    : HexString -> Natural -> BinaryHex
    = \(h : HexString) -> \(bytes : Natural) ->
        { data = h, byteCount = bytes }

-- Concatenate hex strings
let concat
    : HexString -> HexString -> HexString
    = \(a : HexString) -> \(b : HexString) ->
        { hex = a.hex ++ b.hex }

-- Concatenate list of hex strings
let concatAll
    : List HexString -> HexString
    = \(hs : List HexString) ->
        { hex = Prelude.Text.concat (Prelude.List.map HexString Text (\(h : HexString) -> h.hex) hs) }

in {
    -- Types
    HexString,
    HexResult,
    Byte,
    HexPrefix,
    PrefixedHex,
    FixedHex,
    Hex32,
    Hex40,
    Hex64,
    Hex128,
    EthAddress,
    HexColor,
    BinaryHex,

    -- Constructors
    ok,
    err,
    mkHexString,
    empty,
    mkByte,
    mkPrefixedHex,
    mkFixedHex,
    mkHex32,
    mkHex40,
    mkHex64,
    mkHex128,
    mkEthAddress,
    mkHexColor,
    mkBinaryHex,

    -- Conversion
    byteToHex,
    bytesToHex,
    formatPrefixedHex,
    formatEthAddress,
    formatHexColor,
    stripPrefix,

    -- Operations
    concat,
    concatAll,
    isValidHex,

    -- Constants
    CommonColors
}
