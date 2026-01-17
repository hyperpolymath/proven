-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafeNetwork exposing
    ( IPv4, ipv4, fromIPv4, ipv4ToString
    , IPv6, ipv6, fromIPv6, ipv6ToString
    , Port, port_, fromPort, isWellKnownPort, isRegisteredPort, isDynamicPort
    , CIDR, cidr, containsIp
    , isPrivateIp, isLoopback, isLinkLocal
    , Error(..)
    )

{-| Safe network address validation and parsing.

Provides IPv4, IPv6, port, and CIDR notation validation.


# IPv4

@docs IPv4, ipv4, fromIPv4, ipv4ToString


# IPv6

@docs IPv6, ipv6, fromIPv6, ipv6ToString


# Ports

@docs Port, port_, fromPort, isWellKnownPort, isRegisteredPort, isDynamicPort


# CIDR

@docs CIDR, cidr, containsIp


# IP Classification

@docs isPrivateIp, isLoopback, isLinkLocal


# Errors

@docs Error

-}


{-| Error types for network operations.
-}
type Error
    = InvalidIPv4
    | InvalidIPv6
    | InvalidPort
    | InvalidCIDR



-- ============================================================================
-- IPv4
-- ============================================================================


{-| A validated IPv4 address.
-}
type IPv4
    = IPv4 Int Int Int Int


{-| Parse an IPv4 address string.
-}
ipv4 : String -> Maybe IPv4
ipv4 str =
    case String.split "." str of
        [ a, b, c, d ] ->
            Maybe.map4 IPv4
                (parseOctet a)
                (parseOctet b)
                (parseOctet c)
                (parseOctet d)

        _ ->
            Nothing


parseOctet : String -> Maybe Int
parseOctet str =
    String.toInt str
        |> Maybe.andThen
            (\n ->
                if n >= 0 && n <= 255 then
                    Just n

                else
                    Nothing
            )


{-| Get the octets from an IPv4 address.
-}
fromIPv4 : IPv4 -> ( Int, Int, Int, Int )
fromIPv4 (IPv4 a b c d) =
    ( a, b, c, d )


{-| Convert an IPv4 address to a string.
-}
ipv4ToString : IPv4 -> String
ipv4ToString (IPv4 a b c d) =
    String.fromInt a
        ++ "."
        ++ String.fromInt b
        ++ "."
        ++ String.fromInt c
        ++ "."
        ++ String.fromInt d



-- ============================================================================
-- IPv6
-- ============================================================================


{-| A validated IPv6 address.
-}
type IPv6
    = IPv6 (List Int)


{-| Parse an IPv6 address string.
-}
ipv6 : String -> Maybe IPv6
ipv6 str =
    let
        normalized =
            expandIPv6 str
    in
    case normalized of
        Just groups ->
            if List.length groups == 8 && List.all (\n -> n >= 0 && n <= 65535) groups then
                Just (IPv6 groups)

            else
                Nothing

        Nothing ->
            Nothing


expandIPv6 : String -> Maybe (List Int)
expandIPv6 str =
    if String.contains "::" str then
        let
            parts =
                String.split "::" str

            parseGroups s =
                String.split ":" s
                    |> List.filter (String.isEmpty >> not)
                    |> List.filterMap parseHexGroup
        in
        case parts of
            [ left, right ] ->
                let
                    leftGroups =
                        parseGroups left

                    rightGroups =
                        parseGroups right

                    zerosNeeded =
                        8 - List.length leftGroups - List.length rightGroups
                in
                if zerosNeeded >= 0 then
                    Just (leftGroups ++ List.repeat zerosNeeded 0 ++ rightGroups)

                else
                    Nothing

            _ ->
                Nothing

    else
        let
            groups =
                String.split ":" str
                    |> List.filterMap parseHexGroup
        in
        if List.length groups == 8 then
            Just groups

        else
            Nothing


parseHexGroup : String -> Maybe Int
parseHexGroup str =
    if String.length str > 4 then
        Nothing

    else
        String.toList (String.toLower str)
            |> List.foldl
                (\c acc ->
                    acc
                        |> Maybe.andThen
                            (\n ->
                                hexDigitValue c
                                    |> Maybe.map (\d -> n * 16 + d)
                            )
                )
                (Just 0)


hexDigitValue : Char -> Maybe Int
hexDigitValue c =
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


{-| Get the groups from an IPv6 address.
-}
fromIPv6 : IPv6 -> List Int
fromIPv6 (IPv6 groups) =
    groups


{-| Convert an IPv6 address to a string.
-}
ipv6ToString : IPv6 -> String
ipv6ToString (IPv6 groups) =
    groups
        |> List.map toHexString
        |> String.join ":"


toHexString : Int -> String
toHexString n =
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
-- PORTS
-- ============================================================================


{-| A validated network port (1-65535).
-}
type Port
    = Port Int


{-| Create a validated port.
-}
port_ : Int -> Maybe Port
port_ n =
    if n >= 1 && n <= 65535 then
        Just (Port n)

    else
        Nothing


{-| Get the port number.
-}
fromPort : Port -> Int
fromPort (Port n) =
    n


{-| Check if port is in the well-known range (1-1023).
-}
isWellKnownPort : Port -> Bool
isWellKnownPort (Port n) =
    n >= 1 && n <= 1023


{-| Check if port is in the registered range (1024-49151).
-}
isRegisteredPort : Port -> Bool
isRegisteredPort (Port n) =
    n >= 1024 && n <= 49151


{-| Check if port is in the dynamic/private range (49152-65535).
-}
isDynamicPort : Port -> Bool
isDynamicPort (Port n) =
    n >= 49152 && n <= 65535



-- ============================================================================
-- CIDR
-- ============================================================================


{-| A CIDR notation network.
-}
type CIDR
    = CIDR IPv4 Int


{-| Parse a CIDR notation string.
-}
cidr : String -> Maybe CIDR
cidr str =
    case String.split "/" str of
        [ ipStr, maskStr ] ->
            Maybe.map2 CIDR
                (ipv4 ipStr)
                (String.toInt maskStr
                    |> Maybe.andThen
                        (\m ->
                            if m >= 0 && m <= 32 then
                                Just m

                            else
                                Nothing
                        )
                )

        _ ->
            Nothing


{-| Check if an IP address is within a CIDR range.
-}
containsIp : IPv4 -> CIDR -> Bool
containsIp (IPv4 a1 b1 c1 d1) (CIDR (IPv4 a2 b2 c2 d2) mask) =
    let
        ip1 =
            a1 * 16777216 + b1 * 65536 + c1 * 256 + d1

        ip2 =
            a2 * 16777216 + b2 * 65536 + c2 * 256 + d2

        maskBits =
            if mask == 0 then
                0

            else
                0xFFFFFFFF
                    |> Bitwise.shiftLeftBy (32 - mask)
                    |> Bitwise.and 0xFFFFFFFF
    in
    Bitwise.and ip1 maskBits == Bitwise.and ip2 maskBits



-- ============================================================================
-- IP CLASSIFICATION
-- ============================================================================


{-| Check if an IPv4 address is private (RFC 1918).
-}
isPrivateIp : IPv4 -> Bool
isPrivateIp (IPv4 a b _ _) =
    a == 10 || (a == 172 && b >= 16 && b <= 31) || (a == 192 && b == 168)


{-| Check if an IPv4 address is loopback.
-}
isLoopback : IPv4 -> Bool
isLoopback (IPv4 a _ _ _) =
    a == 127


{-| Check if an IPv4 address is link-local.
-}
isLinkLocal : IPv4 -> Bool
isLinkLocal (IPv4 a b _ _) =
    a == 169 && b == 254
