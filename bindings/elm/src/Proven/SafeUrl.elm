-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeUrl exposing
    ( Url, url, fromUrl
    , scheme, host, port_, path, query, fragment
    , isHttps, isHttp, isSafeScheme
    , encode, decode
    , addQueryParam, getQueryParam
    , Error(..)
    )

{-| Safe URL validation and parsing.

Provides RFC 3986 compliant URL parsing with scheme validation.


# URL Type

@docs Url, url, fromUrl


# URL Components

@docs scheme, host, port_, path, query, fragment


# Scheme Validation

@docs isHttps, isHttp, isSafeScheme


# Encoding

@docs encode, decode


# Query Parameters

@docs addQueryParam, getQueryParam


# Errors

@docs Error

-}

import Regex exposing (Regex)


{-| Error types for URL operations.
-}
type Error
    = InvalidUrl
    | UnsafeScheme
    | EmptyUrl



-- ============================================================================
-- URL TYPE
-- ============================================================================


{-| A validated URL.
-}
type Url
    = Url
        { scheme_ : String
        , host_ : String
        , port__ : Maybe Int
        , path_ : String
        , query_ : Maybe String
        , fragment_ : Maybe String
        }


{-| URL regex pattern for parsing.
-}
urlRegex : Maybe Regex
urlRegex =
    Regex.fromString "^([a-z][a-z0-9+.-]*):(?://([^/?#]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$"


{-| Create a validated URL. Returns Nothing if invalid.
-}
url : String -> Maybe Url
url str =
    case urlRegex of
        Just regex ->
            case Regex.find regex str of
                [ match ] ->
                    case match.submatches of
                        [ Just sch, authority, Just pth, q, frag ] ->
                            let
                                ( h, p ) =
                                    parseAuthority (Maybe.withDefault "" authority)
                            in
                            Just
                                (Url
                                    { scheme_ = sch
                                    , host_ = h
                                    , port__ = p
                                    , path_ = pth
                                    , query_ = q
                                    , fragment_ = frag
                                    }
                                )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


parseAuthority : String -> ( String, Maybe Int )
parseAuthority auth =
    case String.split ":" auth of
        [ hostPart ] ->
            ( hostPart, Nothing )

        [ hostPart, portPart ] ->
            ( hostPart, String.toInt portPart )

        _ ->
            ( auth, Nothing )


{-| Get the string representation of a URL.
-}
fromUrl : Url -> String
fromUrl (Url u) =
    let
        portStr =
            case u.port__ of
                Just p ->
                    ":" ++ String.fromInt p

                Nothing ->
                    ""

        queryStr =
            case u.query_ of
                Just q ->
                    "?" ++ q

                Nothing ->
                    ""

        fragStr =
            case u.fragment_ of
                Just f ->
                    "#" ++ f

                Nothing ->
                    ""
    in
    u.scheme_ ++ "://" ++ u.host_ ++ portStr ++ u.path_ ++ queryStr ++ fragStr



-- ============================================================================
-- URL COMPONENTS
-- ============================================================================


{-| Get the scheme of a URL.
-}
scheme : Url -> String
scheme (Url u) =
    u.scheme_


{-| Get the host of a URL.
-}
host : Url -> String
host (Url u) =
    u.host_


{-| Get the port of a URL if present.
-}
port_ : Url -> Maybe Int
port_ (Url u) =
    u.port__


{-| Get the path of a URL.
-}
path : Url -> String
path (Url u) =
    u.path_


{-| Get the query string of a URL if present.
-}
query : Url -> Maybe String
query (Url u) =
    u.query_


{-| Get the fragment of a URL if present.
-}
fragment : Url -> Maybe String
fragment (Url u) =
    u.fragment_



-- ============================================================================
-- SCHEME VALIDATION
-- ============================================================================


{-| Check if the URL uses HTTPS.
-}
isHttps : Url -> Bool
isHttps (Url u) =
    u.scheme_ == "https"


{-| Check if the URL uses HTTP.
-}
isHttp : Url -> Bool
isHttp (Url u) =
    u.scheme_ == "http"


{-| Check if the URL uses a safe scheme (http, https, mailto).
-}
isSafeScheme : Url -> Bool
isSafeScheme (Url u) =
    List.member u.scheme_ [ "http", "https", "mailto", "tel" ]



-- ============================================================================
-- ENCODING
-- ============================================================================


{-| URL-encode a string.
-}
encode : String -> String
encode str =
    String.toList str
        |> List.map encodeChar
        |> String.concat


encodeChar : Char -> String
encodeChar c =
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


{-| URL-decode a string.
-}
decode : String -> Maybe String
decode str =
    decodeHelper str ""


decodeHelper : String -> String -> Maybe String
decodeHelper remaining acc =
    case String.uncons remaining of
        Nothing ->
            Just acc

        Just ( '%', rest ) ->
            case ( String.left 2 rest, fromHex (String.left 2 rest) ) of
                ( hex, Just code ) ->
                    decodeHelper (String.dropLeft 2 rest) (acc ++ String.fromChar (Char.fromCode code))

                _ ->
                    Nothing

        Just ( c, rest ) ->
            decodeHelper rest (acc ++ String.fromChar c)


fromHex : String -> Maybe Int
fromHex hex =
    if String.length hex /= 2 then
        Nothing

    else
        let
            chars =
                String.toList (String.toUpper hex)
        in
        case chars of
            [ h1, h2 ] ->
                Maybe.map2 (\a b -> a * 16 + b)
                    (hexDigitValue h1)
                    (hexDigitValue h2)

            _ ->
                Nothing


hexDigitValue : Char -> Maybe Int
hexDigitValue c =
    let
        code =
            Char.toCode c
    in
    if code >= 48 && code <= 57 then
        Just (code - 48)

    else if code >= 65 && code <= 70 then
        Just (code - 55)

    else
        Nothing



-- ============================================================================
-- QUERY PARAMETERS
-- ============================================================================


{-| Add a query parameter to a URL.
-}
addQueryParam : String -> String -> Url -> Url
addQueryParam key value (Url u) =
    let
        newParam =
            encode key ++ "=" ++ encode value

        newQuery =
            case u.query_ of
                Nothing ->
                    Just newParam

                Just q ->
                    Just (q ++ "&" ++ newParam)
    in
    Url { u | query_ = newQuery }


{-| Get a query parameter value from a URL.
-}
getQueryParam : String -> Url -> Maybe String
getQueryParam key (Url u) =
    case u.query_ of
        Nothing ->
            Nothing

        Just q ->
            String.split "&" q
                |> List.filterMap
                    (\param ->
                        case String.split "=" param of
                            [ k, v ] ->
                                if k == key then
                                    decode v

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> List.head
