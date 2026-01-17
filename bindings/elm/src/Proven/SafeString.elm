-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeString exposing
    ( escapeHtml, escapeUrl, escapeSql, escapeJson, escapeRegex
    , truncate, truncateWords, ellipsis
    , isBlank, isNotBlank, isAscii, isAlphanumeric, isNumeric
    , normalize, trim, trimLeft, trimRight
    , NonEmptyString, nonEmpty, fromNonEmpty, mapNonEmpty
    , SafeId, safeId, fromSafeId
    )

{-| Safe string operations with injection prevention.

Provides safe string manipulation including HTML/SQL/URL escaping
to prevent injection attacks.


# Escaping

@docs escapeHtml, escapeUrl, escapeSql, escapeJson, escapeRegex


# Truncation

@docs truncate, truncateWords, ellipsis


# Validation

@docs isBlank, isNotBlank, isAscii, isAlphanumeric, isNumeric


# Normalization

@docs normalize, trim, trimLeft, trimRight


# Non-Empty Strings

@docs NonEmptyString, nonEmpty, fromNonEmpty, mapNonEmpty


# Safe Identifiers

@docs SafeId, safeId, fromSafeId

-}

import Regex exposing (Regex)



-- ============================================================================
-- ESCAPING
-- ============================================================================


{-| Escape HTML special characters to prevent XSS attacks.
-}
escapeHtml : String -> String
escapeHtml str =
    str
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&#x27;"


{-| Escape a string for use in URLs.
-}
escapeUrl : String -> String
escapeUrl str =
    String.toList str
        |> List.map escapeUrlChar
        |> String.concat


escapeUrlChar : Char -> String
escapeUrlChar c =
    if Char.isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '~' then
        String.fromChar c

    else
        let
            code =
                Char.toCode c
        in
        "%" ++ toHex code


toHex : Int -> String
toHex n =
    let
        hexDigit d =
            if d < 10 then
                String.fromChar (Char.fromCode (d + 48))

            else
                String.fromChar (Char.fromCode (d - 10 + 65))
    in
    if n < 16 then
        "0" ++ hexDigit n

    else
        hexDigit (n // 16) ++ hexDigit (modBy 16 n)


{-| Escape SQL special characters to prevent SQL injection.
-}
escapeSql : String -> String
escapeSql str =
    str
        |> String.replace "'" "''"
        |> String.replace "\\" "\\\\"
        |> String.replace "\u{0000}" ""


{-| Escape a string for use in JSON.
-}
escapeJson : String -> String
escapeJson str =
    str
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"
        |> String.replace "\t" "\\t"


{-| Escape regex special characters.
-}
escapeRegex : String -> String
escapeRegex str =
    let
        specialChars =
            [ "\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}" ]
    in
    List.foldl (\char s -> String.replace char ("\\" ++ char) s) str specialChars



-- ============================================================================
-- TRUNCATION
-- ============================================================================


{-| Truncate a string to a maximum length.
-}
truncate : Int -> String -> String
truncate maxLength str =
    if String.length str <= maxLength then
        str

    else
        String.left maxLength str


{-| Truncate to a maximum number of words.
-}
truncateWords : Int -> String -> String
truncateWords maxWords str =
    String.words str
        |> List.take maxWords
        |> String.join " "


{-| Truncate with an ellipsis suffix.
-}
ellipsis : Int -> String -> String
ellipsis maxLength str =
    if String.length str <= maxLength then
        str

    else if maxLength <= 3 then
        String.left maxLength str

    else
        String.left (maxLength - 3) str ++ "..."



-- ============================================================================
-- VALIDATION
-- ============================================================================


{-| Check if a string is blank (empty or only whitespace).
-}
isBlank : String -> Bool
isBlank str =
    String.trim str == ""


{-| Check if a string is not blank.
-}
isNotBlank : String -> Bool
isNotBlank str =
    not (isBlank str)


{-| Check if a string contains only ASCII characters.
-}
isAscii : String -> Bool
isAscii str =
    String.all (\c -> Char.toCode c < 128) str


{-| Check if a string contains only alphanumeric characters.
-}
isAlphanumeric : String -> Bool
isAlphanumeric str =
    String.all Char.isAlphaNum str


{-| Check if a string contains only numeric characters.
-}
isNumeric : String -> Bool
isNumeric str =
    String.all Char.isDigit str



-- ============================================================================
-- NORMALIZATION
-- ============================================================================


{-| Normalize whitespace (collapse multiple spaces to single).
-}
normalize : String -> String
normalize str =
    String.words str
        |> String.join " "


{-| Trim whitespace from both ends.
-}
trim : String -> String
trim =
    String.trim


{-| Trim whitespace from the left.
-}
trimLeft : String -> String
trimLeft =
    String.trimLeft


{-| Trim whitespace from the right.
-}
trimRight : String -> String
trimRight =
    String.trimRight



-- ============================================================================
-- NON-EMPTY STRINGS
-- ============================================================================


{-| A string that is guaranteed to be non-empty.
-}
type NonEmptyString
    = NonEmptyString String


{-| Create a non-empty string. Returns Nothing if empty.
-}
nonEmpty : String -> Maybe NonEmptyString
nonEmpty str =
    if String.isEmpty str then
        Nothing

    else
        Just (NonEmptyString str)


{-| Get the string value from a NonEmptyString.
-}
fromNonEmpty : NonEmptyString -> String
fromNonEmpty (NonEmptyString str) =
    str


{-| Map a function over a NonEmptyString.
-}
mapNonEmpty : (String -> String) -> NonEmptyString -> Maybe NonEmptyString
mapNonEmpty f (NonEmptyString str) =
    nonEmpty (f str)



-- ============================================================================
-- SAFE IDENTIFIERS
-- ============================================================================


{-| A safe identifier containing only alphanumeric characters and underscores.
-}
type SafeId
    = SafeId String


{-| Create a safe ID. Returns Nothing if invalid.
-}
safeId : String -> Maybe SafeId
safeId str =
    if String.isEmpty str then
        Nothing

    else if String.all (\c -> Char.isAlphaNum c || c == '_') str then
        Just (SafeId str)

    else
        Nothing


{-| Get the string value from a SafeId.
-}
fromSafeId : SafeId -> String
fromSafeId (SafeId str) =
    str
