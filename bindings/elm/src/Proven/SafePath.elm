-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath


module Proven.SafePath exposing
    ( SafePath, safePath, fromSafePath
    , hasTraversal, normalize, join, split
    , extension, filename, dirname, basename
    , isAbsolute, isRelative
    , Error(..)
    )

{-| Safe filesystem path operations with traversal prevention.

Provides path manipulation that prevents directory traversal attacks
and ensures safe path handling.


# Safe Paths

@docs SafePath, safePath, fromSafePath


# Validation

@docs hasTraversal, normalize, join, split


# Path Components

@docs extension, filename, dirname, basename


# Path Type Checks

@docs isAbsolute, isRelative


# Errors

@docs Error

-}


{-| Error types for path operations.
-}
type Error
    = PathTraversal
    | EmptyPath
    | InvalidPath String



-- ============================================================================
-- SAFE PATHS
-- ============================================================================


{-| A path that has been validated to be safe (no traversal).
-}
type SafePath
    = SafePath String


{-| Create a safe path. Returns Nothing if the path contains traversal.
-}
safePath : String -> Maybe SafePath
safePath path =
    if String.isEmpty path then
        Nothing

    else if hasTraversal path then
        Nothing

    else
        Just (SafePath (normalize path))


{-| Get the string value from a SafePath.
-}
fromSafePath : SafePath -> String
fromSafePath (SafePath path) =
    path



-- ============================================================================
-- VALIDATION
-- ============================================================================


{-| Check if a path contains directory traversal sequences.
-}
hasTraversal : String -> Bool
hasTraversal path =
    String.contains ".." path
        || String.contains "\u{0000}" path


{-| Normalize a path (remove redundant slashes, etc.).
-}
normalize : String -> String
normalize path =
    path
        |> String.split "/"
        |> List.filter (\segment -> segment /= "" && segment /= ".")
        |> resolveDoubleDots
        |> String.join "/"
        |> (\p ->
                if String.startsWith "/" path then
                    "/" ++ p

                else
                    p
           )


resolveDoubleDots : List String -> List String
resolveDoubleDots segments =
    List.foldl
        (\segment acc ->
            if segment == ".." then
                case acc of
                    [] ->
                        []

                    _ :: rest ->
                        rest

            else
                segment :: acc
        )
        []
        segments
        |> List.reverse


{-| Join two paths together safely.
-}
join : String -> String -> Maybe String
join base path =
    if hasTraversal path then
        Nothing

    else
        let
            cleanBase =
                if String.endsWith "/" base then
                    String.dropRight 1 base

                else
                    base

            cleanPath =
                if String.startsWith "/" path then
                    String.dropLeft 1 path

                else
                    path
        in
        Just (cleanBase ++ "/" ++ cleanPath)


{-| Split a path into its components.
-}
split : String -> List String
split path =
    path
        |> String.split "/"
        |> List.filter (String.isEmpty >> not)



-- ============================================================================
-- PATH COMPONENTS
-- ============================================================================


{-| Get the file extension (without the dot).
-}
extension : String -> Maybe String
extension path =
    let
        base =
            filename path
    in
    case String.indices "." base of
        [] ->
            Nothing

        indices ->
            let
                lastDot =
                    List.foldl max 0 indices
            in
            if lastDot == 0 then
                Nothing

            else
                Just (String.dropLeft (lastDot + 1) base)


{-| Get the filename from a path.
-}
filename : String -> String
filename path =
    path
        |> String.split "/"
        |> List.filter (String.isEmpty >> not)
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""


{-| Get the directory name from a path.
-}
dirname : String -> String
dirname path =
    path
        |> String.split "/"
        |> List.filter (String.isEmpty >> not)
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"
        |> (\d ->
                if String.startsWith "/" path then
                    "/" ++ d

                else
                    d
           )


{-| Get the basename (filename without extension).
-}
basename : String -> String
basename path =
    let
        name =
            filename path
    in
    case extension path of
        Nothing ->
            name

        Just ext ->
            String.dropRight (String.length ext + 1) name



-- ============================================================================
-- PATH TYPE CHECKS
-- ============================================================================


{-| Check if a path is absolute.
-}
isAbsolute : String -> Bool
isAbsolute path =
    String.startsWith "/" path


{-| Check if a path is relative.
-}
isRelative : String -> Bool
isRelative path =
    not (isAbsolute path)
