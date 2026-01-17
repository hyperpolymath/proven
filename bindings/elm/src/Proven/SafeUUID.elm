-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeUUID exposing
    ( UUID, uuid, fromUUID, uuidToString
    , isValid, version, variant
    , nil, isNil
    , Error(..)
    )

{-| Safe UUID validation and parsing.

Provides RFC 4122 compliant UUID validation and parsing.


# UUID Type

@docs UUID, uuid, fromUUID, uuidToString


# Validation

@docs isValid, version, variant


# Nil UUID

@docs nil, isNil


# Errors

@docs Error

-}

import Regex exposing (Regex)


{-| Error types for UUID operations.
-}
type Error
    = InvalidUUID
    | InvalidFormat



-- ============================================================================
-- UUID TYPE
-- ============================================================================


{-| A validated UUID.
-}
type UUID
    = UUID String


{-| UUID regex pattern.
-}
uuidRegex : Maybe Regex
uuidRegex =
    Regex.fromString "^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"


{-| Parse a UUID string.
-}
uuid : String -> Maybe UUID
uuid str =
    let
        normalized =
            String.toLower str
    in
    if isValid normalized then
        Just (UUID normalized)

    else
        Nothing


{-| Get the raw bytes/string from a UUID.
-}
fromUUID : UUID -> String
fromUUID (UUID u) =
    u


{-| Convert a UUID to its string representation.
-}
uuidToString : UUID -> String
uuidToString (UUID u) =
    u



-- ============================================================================
-- VALIDATION
-- ============================================================================


{-| Check if a string is a valid UUID.
-}
isValid : String -> Bool
isValid str =
    let
        normalized =
            String.toLower str
    in
    case uuidRegex of
        Just regex ->
            Regex.contains regex normalized

        Nothing ->
            False


{-| Get the version of a UUID (1-5).
-}
version : UUID -> Int
version (UUID u) =
    let
        versionChar =
            String.slice 14 15 u
    in
    case String.toInt versionChar of
        Just v ->
            v

        Nothing ->
            0


{-| Get the variant of a UUID.
-}
variant : UUID -> String
variant (UUID u) =
    let
        variantChar =
            String.slice 19 20 u
    in
    if List.member variantChar [ "8", "9", "a", "b" ] then
        "RFC 4122"

    else if List.member variantChar [ "c", "d" ] then
        "Microsoft"

    else if List.member variantChar [ "0", "1", "2", "3", "4", "5", "6", "7" ] then
        "NCS"

    else
        "Reserved"



-- ============================================================================
-- NIL UUID
-- ============================================================================


{-| The nil UUID (all zeros).
-}
nil : UUID
nil =
    UUID "00000000-0000-0000-0000-000000000000"


{-| Check if a UUID is the nil UUID.
-}
isNil : UUID -> Bool
isNil (UUID u) =
    u == "00000000-0000-0000-0000-000000000000"
