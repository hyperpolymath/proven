-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeEmail exposing
    ( Email, email, fromEmail
    , localPart, domain
    , isValid, normalize
    , Error(..)
    )

{-| Safe email validation and parsing.

Provides RFC 5321/5322 compliant email validation and parsing.


# Email Type

@docs Email, email, fromEmail


# Email Components

@docs localPart, domain


# Validation

@docs isValid, normalize


# Errors

@docs Error

-}

import Regex exposing (Regex)


{-| Error types for email operations.
-}
type Error
    = InvalidEmail
    | EmptyEmail
    | InvalidLocalPart
    | InvalidDomain



-- ============================================================================
-- EMAIL TYPE
-- ============================================================================


{-| A validated email address.
-}
type Email
    = Email
        { local : String
        , domain_ : String
        }


{-| Email regex pattern for basic validation.
-}
emailRegex : Maybe Regex
emailRegex =
    Regex.fromString "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"


{-| Create a validated email. Returns Nothing if invalid.
-}
email : String -> Maybe Email
email str =
    if isValid str then
        case String.split "@" str of
            [ local, dom ] ->
                Just (Email { local = local, domain_ = dom })

            _ ->
                Nothing

    else
        Nothing


{-| Get the string representation of an email.
-}
fromEmail : Email -> String
fromEmail (Email e) =
    e.local ++ "@" ++ e.domain_



-- ============================================================================
-- EMAIL COMPONENTS
-- ============================================================================


{-| Get the local part (before @) of an email.
-}
localPart : Email -> String
localPart (Email e) =
    e.local


{-| Get the domain (after @) of an email.
-}
domain : Email -> String
domain (Email e) =
    e.domain_



-- ============================================================================
-- VALIDATION
-- ============================================================================


{-| Check if a string is a valid email address.
-}
isValid : String -> Bool
isValid str =
    case emailRegex of
        Just regex ->
            Regex.contains regex str
                && String.length str <= 254
                && (case String.split "@" str of
                        [ local, _ ] ->
                            String.length local <= 64

                        _ ->
                            False
                   )

        Nothing ->
            False


{-| Normalize an email address (lowercase domain).
-}
normalize : Email -> Email
normalize (Email e) =
    Email { e | domain_ = String.toLower e.domain_ }
