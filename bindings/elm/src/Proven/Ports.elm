{-
   SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

   Proven Safety Library - Elm Ports Module

   Elm cannot call C FFI directly. This module declares ports that
   communicate with the JavaScript FFI binding (bindings/javascript/).
   The JavaScript side loads libproven via Deno.dlopen and relays
   results back through incoming ports.

   ALL computation is performed in the verified Idris 2 core via FFI.
   This module does NOT reimplement any logic.

   Usage:
     1. Wire these ports in your Elm application's main
     2. Include the proven-elm-ports.js interop file
     3. The JS interop calls bindings/javascript/src/ which calls libproven

   See: bindings/elm/interop/proven-elm-ports.js for the JavaScript side.
-}


port module Proven.Ports exposing
    ( -- Outgoing requests (Elm -> JS -> libproven)
      requestMathDiv
    , requestMathMod
    , requestMathAdd
    , requestMathSub
    , requestMathMul
    , requestMathAbs
    , requestMathClamp
    , requestStringIsValidUtf8
    , requestStringEscapeHtml
    , requestStringEscapeSql
    , requestStringEscapeJs
    , requestPathHasTraversal
    , requestPathSanitizeFilename
    , requestCryptoConstantTimeEq
    , requestEmailIsValid
    , requestUrlParse
    , requestNetworkParseIpv4
    , requestJsonIsValid
    , requestJsonGetType
    , requestUuidV4
    , requestUuidParse
    , requestFloatDiv
    , requestFloatSqrt
    , requestFloatLn
    , requestDatetimeParse
    , requestDatetimeIsLeapYear
    , requestHexEncode
    , requestHexDecode
    , requestVersionParse
    , requestPasswordValidate
    , requestCalculatorEval

    -- Incoming results (libproven -> JS -> Elm)
    , receiveIntResult
    , receiveBoolResult
    , receiveStringResult
    , receiveFloatResult
    )

import Json.Encode as Encode


-- ============================================================================
-- OUTGOING PORTS: Elm -> JavaScript -> libproven FFI
-- ============================================================================

-- SafeMath
port requestMathDiv : { id : String, a : Int, b : Int } -> Cmd msg
port requestMathMod : { id : String, a : Int, b : Int } -> Cmd msg
port requestMathAdd : { id : String, a : Int, b : Int } -> Cmd msg
port requestMathSub : { id : String, a : Int, b : Int } -> Cmd msg
port requestMathMul : { id : String, a : Int, b : Int } -> Cmd msg
port requestMathAbs : { id : String, n : Int } -> Cmd msg
port requestMathClamp : { id : String, lo : Int, hi : Int, value : Int } -> Cmd msg

-- SafeString
port requestStringIsValidUtf8 : { id : String, input : String } -> Cmd msg
port requestStringEscapeHtml : { id : String, input : String } -> Cmd msg
port requestStringEscapeSql : { id : String, input : String } -> Cmd msg
port requestStringEscapeJs : { id : String, input : String } -> Cmd msg

-- SafePath
port requestPathHasTraversal : { id : String, path : String } -> Cmd msg
port requestPathSanitizeFilename : { id : String, filename : String } -> Cmd msg

-- SafeCrypto
port requestCryptoConstantTimeEq : { id : String, a : String, b : String } -> Cmd msg

-- SafeEmail
port requestEmailIsValid : { id : String, email : String } -> Cmd msg

-- SafeUrl
port requestUrlParse : { id : String, url : String } -> Cmd msg

-- SafeNetwork
port requestNetworkParseIpv4 : { id : String, ip : String } -> Cmd msg

-- SafeJson
port requestJsonIsValid : { id : String, json : String } -> Cmd msg
port requestJsonGetType : { id : String, json : String } -> Cmd msg

-- SafeUUID
port requestUuidV4 : { id : String } -> Cmd msg
port requestUuidParse : { id : String, uuid : String } -> Cmd msg

-- SafeFloat
port requestFloatDiv : { id : String, a : Float, b : Float } -> Cmd msg
port requestFloatSqrt : { id : String, x : Float } -> Cmd msg
port requestFloatLn : { id : String, x : Float } -> Cmd msg

-- SafeDateTime
port requestDatetimeParse : { id : String, input : String } -> Cmd msg
port requestDatetimeIsLeapYear : { id : String, year : Int } -> Cmd msg

-- SafeHex
port requestHexEncode : { id : String, input : String } -> Cmd msg
port requestHexDecode : { id : String, hex : String } -> Cmd msg

-- SafeVersion
port requestVersionParse : { id : String, version : String } -> Cmd msg

-- SafePassword
port requestPasswordValidate : { id : String, password : String } -> Cmd msg

-- SafeCalculator
port requestCalculatorEval : { id : String, expr : String } -> Cmd msg


-- ============================================================================
-- INCOMING PORTS: libproven FFI -> JavaScript -> Elm
-- ============================================================================

{-| Receive integer result from libproven FFI.
    JSON: { "id": "<request-id>", "status": 0, "value": 42 }
    status=0 means success, non-zero is a ProvenStatus error code.
-}
port receiveIntResult : ({ id : String, status : Int, value : Int } -> msg) -> Sub msg

{-| Receive boolean result from libproven FFI. -}
port receiveBoolResult : ({ id : String, status : Int, value : Bool } -> msg) -> Sub msg

{-| Receive string result from libproven FFI. -}
port receiveStringResult : ({ id : String, status : Int, value : String } -> msg) -> Sub msg

{-| Receive float result from libproven FFI. -}
port receiveFloatResult : ({ id : String, status : Int, value : Float } -> msg) -> Sub msg
