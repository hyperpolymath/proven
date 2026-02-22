{-
   SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

   Proven Safety Library - Elm binding

   Elm CANNOT call C FFI directly. This binding uses Elm ports to
   communicate with the JavaScript FFI binding, which loads libproven
   (Idris 2 + Zig) via Deno.dlopen.

   ALL computation is performed in the verified Idris 2 core.
   This module provides ONLY:
     - Type definitions for Proven results
     - Status code constants matching proven.h
     - Helper functions for interpreting port responses

   For the actual FFI port declarations, see: Proven.Ports
   For the JavaScript interop glue, see: interop/proven-elm-ports.js

   Architecture:
     Elm app -> Proven.Ports (outgoing port)
       -> proven-elm-ports.js
         -> bindings/javascript/src/ffi.js
           -> libproven.so (Idris 2 + Zig)
         <- result
       <- Proven.Ports (incoming port)
     <- Elm app (Msg update)
-}


module Proven exposing
    ( ProvenStatus(..)
    , statusFromInt
    , statusToString
    , isSuccess
    , version
    )


{-| Status codes returned by Proven operations, matching proven.h.
-}
type ProvenStatus
    = Ok
    | ErrNullPointer
    | ErrInvalidArgument
    | ErrOverflow
    | ErrUnderflow
    | ErrDivisionByZero
    | ErrParseFailure
    | ErrValidationFailed
    | ErrOutOfBounds
    | ErrEncodingError
    | ErrAllocationFailed
    | ErrNotImplemented
    | ErrUnknown Int


{-| Convert an integer status code from a port response to ProvenStatus.
-}
statusFromInt : Int -> ProvenStatus
statusFromInt code =
    case code of
        0 ->
            Ok

        -1 ->
            ErrNullPointer

        -2 ->
            ErrInvalidArgument

        -3 ->
            ErrOverflow

        -4 ->
            ErrUnderflow

        -5 ->
            ErrDivisionByZero

        -6 ->
            ErrParseFailure

        -7 ->
            ErrValidationFailed

        -8 ->
            ErrOutOfBounds

        -9 ->
            ErrEncodingError

        -10 ->
            ErrAllocationFailed

        -99 ->
            ErrNotImplemented

        other ->
            ErrUnknown other


{-| Convert a status code to a human-readable string.
-}
statusToString : ProvenStatus -> String
statusToString status =
    case status of
        Ok ->
            "Ok"

        ErrNullPointer ->
            "Null pointer"

        ErrInvalidArgument ->
            "Invalid argument"

        ErrOverflow ->
            "Overflow"

        ErrUnderflow ->
            "Underflow"

        ErrDivisionByZero ->
            "Division by zero"

        ErrParseFailure ->
            "Parse failure"

        ErrValidationFailed ->
            "Validation failed"

        ErrOutOfBounds ->
            "Out of bounds"

        ErrEncodingError ->
            "Encoding error"

        ErrAllocationFailed ->
            "Allocation failed"

        ErrNotImplemented ->
            "Not implemented"

        ErrUnknown code ->
            "Unknown error (" ++ String.fromInt code ++ ")"


{-| Check if a status code indicates success.
-}
isSuccess : Int -> Bool
isSuccess code =
    code == 0


{-| Library version.
-}
version : String
version =
    "0.9.0"
