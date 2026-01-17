-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeCrypto exposing
    ( Hash, hashToString
    , sha256, sha512, md5
    , hmacSha256, hmacSha512
    , constantTimeEquals
    , hexEncode, hexDecode
    , base64Encode, base64Decode
    , Error(..)
    )

{-| Safe cryptographic operations.

Provides secure hashing and HMAC functions with constant-time comparison.

Note: Actual cryptographic operations should be delegated to JavaScript
via ports in a real application. This module provides the type-safe interface.


# Hash Type

@docs Hash, hashToString


# Hashing (Stub implementations)

@docs sha256, sha512, md5


# HMAC (Stub implementations)

@docs hmacSha256, hmacSha512


# Comparison

@docs constantTimeEquals


# Encoding

@docs hexEncode, hexDecode
@docs base64Encode, base64Decode


# Errors

@docs Error

-}


{-| Error types for crypto operations.
-}
type Error
    = InvalidHash
    | InvalidKey
    | DecodingError



-- ============================================================================
-- HASH TYPE
-- ============================================================================


{-| A cryptographic hash value.
-}
type Hash
    = Hash String


{-| Convert a hash to its hex string representation.
-}
hashToString : Hash -> String
hashToString (Hash h) =
    h



-- ============================================================================
-- HASHING (STUB IMPLEMENTATIONS)
-- ============================================================================


{-| SHA-256 hash (stub - returns placeholder).

In a real implementation, this would use a port to JavaScript.

-}
sha256 : String -> Hash
sha256 input =
    -- Stub implementation - would use ports in real app
    Hash ("sha256:" ++ String.left 64 (simpleHash input))


{-| SHA-512 hash (stub - returns placeholder).
-}
sha512 : String -> Hash
sha512 input =
    Hash ("sha512:" ++ String.left 128 (simpleHash input))


{-| MD5 hash (stub - returns placeholder).

Note: MD5 is cryptographically broken and should not be used for security.

-}
md5 : String -> Hash
md5 input =
    Hash ("md5:" ++ String.left 32 (simpleHash input))


simpleHash : String -> String
simpleHash input =
    let
        hash =
            String.foldl
                (\c acc ->
                    let
                        code =
                            Char.toCode c
                    in
                    modBy 0xFFFFFFFF (acc * 31 + code)
                )
                0
                input
    in
    String.repeat 64 (toHexPadded hash)


toHexPadded : Int -> String
toHexPadded n =
    let
        hex =
            toHex n
    in
    String.padLeft 8 '0' hex


toHex : Int -> String
toHex n =
    if n == 0 then
        "0"

    else
        toHexHelper n ""


toHexHelper : Int -> String -> String
toHexHelper n acc =
    if n == 0 then
        acc

    else
        let
            digit =
                modBy 16 n

            char =
                if digit < 10 then
                    String.fromChar (Char.fromCode (digit + 48))

                else
                    String.fromChar (Char.fromCode (digit - 10 + 97))
        in
        toHexHelper (n // 16) (char ++ acc)



-- ============================================================================
-- HMAC (STUB IMPLEMENTATIONS)
-- ============================================================================


{-| HMAC-SHA256 (stub - returns placeholder).
-}
hmacSha256 : String -> String -> Hash
hmacSha256 key message =
    Hash ("hmac-sha256:" ++ String.left 64 (simpleHash (key ++ message)))


{-| HMAC-SHA512 (stub - returns placeholder).
-}
hmacSha512 : String -> String -> Hash
hmacSha512 key message =
    Hash ("hmac-sha512:" ++ String.left 128 (simpleHash (key ++ message)))



-- ============================================================================
-- COMPARISON
-- ============================================================================


{-| Constant-time string comparison to prevent timing attacks.
-}
constantTimeEquals : String -> String -> Bool
constantTimeEquals a b =
    if String.length a /= String.length b then
        False

    else
        let
            pairs =
                List.map2 Tuple.pair (String.toList a) (String.toList b)

            diff =
                List.foldl
                    (\( c1, c2 ) acc ->
                        Bitwise.xor acc (Bitwise.xor (Char.toCode c1) (Char.toCode c2))
                    )
                    0
                    pairs
        in
        diff == 0



-- ============================================================================
-- ENCODING
-- ============================================================================


{-| Encode bytes as hexadecimal string.
-}
hexEncode : List Int -> String
hexEncode bytes =
    bytes
        |> List.map
            (\b ->
                let
                    high =
                        b // 16

                    low =
                        modBy 16 b
                in
                hexDigitChar high ++ hexDigitChar low
            )
        |> String.concat


hexDigitChar : Int -> String
hexDigitChar n =
    if n < 10 then
        String.fromChar (Char.fromCode (n + 48))

    else
        String.fromChar (Char.fromCode (n - 10 + 97))


{-| Decode hexadecimal string to bytes.
-}
hexDecode : String -> Maybe (List Int)
hexDecode hex =
    if modBy 2 (String.length hex) /= 0 then
        Nothing

    else
        hexDecodeHelper (String.toList (String.toLower hex)) []


hexDecodeHelper : List Char -> List Int -> Maybe (List Int)
hexDecodeHelper chars acc =
    case chars of
        [] ->
            Just (List.reverse acc)

        [ _ ] ->
            Nothing

        c1 :: c2 :: rest ->
            case ( hexValue c1, hexValue c2 ) of
                ( Just h1, Just h2 ) ->
                    hexDecodeHelper rest ((h1 * 16 + h2) :: acc)

                _ ->
                    Nothing


hexValue : Char -> Maybe Int
hexValue c =
    let
        code =
            Char.toCode c
    in
    if code >= 48 && code <= 57 then
        Just (code - 48)

    else if code >= 97 && code <= 102 then
        Just (code - 87)

    else
        Nothing


{-| Encode string as base64.
-}
base64Encode : String -> String
base64Encode input =
    let
        bytes =
            String.toList input
                |> List.map Char.toCode
    in
    base64EncodeBytes bytes


base64EncodeBytes : List Int -> String
base64EncodeBytes bytes =
    let
        alphabet =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    in
    encodeGroups bytes alphabet ""


encodeGroups : List Int -> String -> String -> String
encodeGroups bytes alphabet acc =
    case bytes of
        [] ->
            acc

        [ b1 ] ->
            let
                i1 =
                    Bitwise.shiftRightBy 2 b1

                i2 =
                    Bitwise.and 0x3F (Bitwise.shiftLeftBy 4 b1)
            in
            acc ++ charAt i1 alphabet ++ charAt i2 alphabet ++ "=="

        [ b1, b2 ] ->
            let
                i1 =
                    Bitwise.shiftRightBy 2 b1

                i2 =
                    Bitwise.and 0x3F (Bitwise.or (Bitwise.shiftLeftBy 4 b1) (Bitwise.shiftRightBy 4 b2))

                i3 =
                    Bitwise.and 0x3F (Bitwise.shiftLeftBy 2 b2)
            in
            acc ++ charAt i1 alphabet ++ charAt i2 alphabet ++ charAt i3 alphabet ++ "="

        b1 :: b2 :: b3 :: rest ->
            let
                i1 =
                    Bitwise.shiftRightBy 2 b1

                i2 =
                    Bitwise.and 0x3F (Bitwise.or (Bitwise.shiftLeftBy 4 b1) (Bitwise.shiftRightBy 4 b2))

                i3 =
                    Bitwise.and 0x3F (Bitwise.or (Bitwise.shiftLeftBy 2 b2) (Bitwise.shiftRightBy 6 b3))

                i4 =
                    Bitwise.and 0x3F b3
            in
            encodeGroups rest alphabet (acc ++ charAt i1 alphabet ++ charAt i2 alphabet ++ charAt i3 alphabet ++ charAt i4 alphabet)


charAt : Int -> String -> String
charAt i s =
    String.slice i (i + 1) s


{-| Decode base64 string.
-}
base64Decode : String -> Maybe String
base64Decode input =
    let
        cleanInput =
            String.filter (\c -> c /= '\n' && c /= '\u{000D}' && c /= ' ') input
    in
    base64DecodeBytes cleanInput
        |> Maybe.map (List.map Char.fromCode >> String.fromList)


base64DecodeBytes : String -> Maybe (List Int)
base64DecodeBytes input =
    let
        alphabet =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

        indexOf c =
            String.indices (String.fromChar c) alphabet
                |> List.head
    in
    if modBy 4 (String.length input) /= 0 then
        Nothing

    else
        decodeBase64Groups (String.toList input) indexOf []


decodeBase64Groups : List Char -> (Char -> Maybe Int) -> List Int -> Maybe (List Int)
decodeBase64Groups chars indexOf acc =
    case chars of
        [] ->
            Just (List.reverse acc)

        [ _, _, _, _ ] ->
            case chars of
                [ c1, c2, '=', '=' ] ->
                    Maybe.map2
                        (\i1 i2 ->
                            let
                                b1 =
                                    Bitwise.or (Bitwise.shiftLeftBy 2 i1) (Bitwise.shiftRightBy 4 i2)
                            in
                            List.reverse (b1 :: acc)
                        )
                        (indexOf c1)
                        (indexOf c2)

                [ c1, c2, c3, '=' ] ->
                    Maybe.map3
                        (\i1 i2 i3 ->
                            let
                                b1 =
                                    Bitwise.or (Bitwise.shiftLeftBy 2 i1) (Bitwise.shiftRightBy 4 i2)

                                b2 =
                                    Bitwise.and 0xFF (Bitwise.or (Bitwise.shiftLeftBy 4 i2) (Bitwise.shiftRightBy 2 i3))
                            in
                            List.reverse (b2 :: b1 :: acc)
                        )
                        (indexOf c1)
                        (indexOf c2)
                        (indexOf c3)

                [ c1, c2, c3, c4 ] ->
                    Maybe.map4
                        (\i1 i2 i3 i4 ->
                            let
                                b1 =
                                    Bitwise.or (Bitwise.shiftLeftBy 2 i1) (Bitwise.shiftRightBy 4 i2)

                                b2 =
                                    Bitwise.and 0xFF (Bitwise.or (Bitwise.shiftLeftBy 4 i2) (Bitwise.shiftRightBy 2 i3))

                                b3 =
                                    Bitwise.and 0xFF (Bitwise.or (Bitwise.shiftLeftBy 6 i3) i4)
                            in
                            List.reverse (b3 :: b2 :: b1 :: acc)
                        )
                        (indexOf c1)
                        (indexOf c2)
                        (indexOf c3)
                        (indexOf c4)

                _ ->
                    Nothing

        c1 :: c2 :: c3 :: c4 :: rest ->
            Maybe.map4
                (\i1 i2 i3 i4 ->
                    let
                        b1 =
                            Bitwise.or (Bitwise.shiftLeftBy 2 i1) (Bitwise.shiftRightBy 4 i2)

                        b2 =
                            Bitwise.and 0xFF (Bitwise.or (Bitwise.shiftLeftBy 4 i2) (Bitwise.shiftRightBy 2 i3))

                        b3 =
                            Bitwise.and 0xFF (Bitwise.or (Bitwise.shiftLeftBy 6 i3) i4)
                    in
                    ( rest, b3 :: b2 :: b1 :: acc )
                )
                (indexOf c1)
                (indexOf c2)
                (indexOf c3)
                (indexOf c4)
                |> Maybe.andThen (\( r, a ) -> decodeBase64Groups r indexOf a)

        _ ->
            Nothing
