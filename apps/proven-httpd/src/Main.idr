-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-httpd: Main entry point
--
-- An HTTP/1.1 server implementation that cannot crash on malformed requests.
-- Uses proven's type-safe approach to ensure all request parsing, routing,
-- and response construction is total.
--
-- Usage:
--   idris2 --build proven-httpd.ipkg
--   ./build/exec/proven-httpd

module Main

import HTTP
import HTTP.Method
import HTTP.Status
import HTTP.Request
import HTTP.Response
import HTTP.Router

%default total

-- ============================================================================
-- Demo handlers
-- ============================================================================

||| Handler for the root path. Returns a welcome message.
indexHandler : Handler
indexHandler _ _ = textResponse OK "Welcome to proven-httpd!"
                   |> withServer serverIdent

||| Handler for /health endpoint. Returns server health status.
healthHandler : Handler
healthHandler _ _ = jsonResponse OK "{\"status\": \"healthy\", \"server\": \"proven-httpd\"}"
                    |> withServer serverIdent

||| Handler for /users/:id endpoint. Extracts the captured user ID.
userHandler : Handler
userHandler req params =
  case lookupParam "id" params of
    Nothing => errorResponse InternalError
    Just uid => jsonResponse OK ("{\"user_id\": \"" ++ uid ++ "\"}")
                |> withServer serverIdent

||| Handler for /echo endpoint. Echoes back the request body.
echoHandler : Handler
echoHandler req _ = textResponse OK req.body
                    |> withServer serverIdent

-- ============================================================================
-- Route table
-- ============================================================================

||| The application route table. Evaluated at compile time.
routes : RouteTable
routes =
  [ get "/" indexHandler
  , get "/health" healthHandler
  , routeWith GET [Exact "users", Capture "id"] userHandler
  , post "/echo" echoHandler
  ]

-- ============================================================================
-- Demo: show routing in action
-- ============================================================================

||| Demonstrate request routing with several example requests.
covering
demoRouting : IO ()
demoRouting = do
  putStrLn "\n--- HTTP Routing Demo (proven dispatch) ---\n"

  -- Demo 1: GET /
  let req1 = simpleGet "/" "localhost"
  let resp1 = dispatch routes req1
  putStrLn $ "Request:  " ++ show req1
  putStrLn $ "Response: " ++ show resp1
  putStrLn ""

  -- Demo 2: GET /health
  let req2 = simpleGet "/health" "localhost"
  let resp2 = dispatch routes req2
  putStrLn $ "Request:  " ++ show req2
  putStrLn $ "Response: " ++ show resp2
  putStrLn ""

  -- Demo 3: GET /users/42 (parameterised route)
  let req3 = simpleGet "/users/42" "localhost"
  let resp3 = dispatch routes req3
  putStrLn $ "Request:  " ++ show req3
  putStrLn $ "Response: " ++ show resp3
  putStrLn ""

  -- Demo 4: POST /echo with body
  let req4 = simplePost "/echo" "localhost" "text/plain" "Hello, proven-httpd!"
  let resp4 = dispatch routes req4
  putStrLn $ "Request:  " ++ show req4
  putStrLn $ "Response: " ++ show resp4
  putStrLn ""

  -- Demo 5: GET /nonexistent (404)
  let req5 = simpleGet "/nonexistent" "localhost"
  let resp5 = dispatch routes req5
  putStrLn $ "Request:  " ++ show req5
  putStrLn $ "Response: " ++ show resp5
  putStrLn ""

  -- Demo 6: DELETE / (405 Method Not Allowed)
  let req6 = MkRequest DELETE "/" "HTTP/1.1" [MkHeader "host" "localhost"] ""
  let resp6 = dispatch routes req6
  putStrLn $ "Request:  " ++ show req6
  putStrLn $ "Response: " ++ show resp6

-- ============================================================================
-- Demo: response serialisation
-- ============================================================================

||| Demonstrate response serialisation to wire format.
covering
demoSerialisation : IO ()
demoSerialisation = do
  putStrLn "\n--- HTTP Response Serialisation Demo ---\n"

  let resp = htmlResponse OK "<html><body><h1>Hello</h1></body></html>"
             |> withServer serverIdent
             |> addHeader "x-powered-by" "proven"

  putStrLn "Serialised response:"
  putStrLn "----"
  putStr (serialiseResponse resp)
  putStrLn "----"
  putStrLn $ "\nEstimated size: " ++ show (estimatedSize resp) ++ " bytes"

-- ============================================================================
-- Demo: method properties
-- ============================================================================

||| Demonstrate method safety and idempotency classification.
covering
demoMethods : IO ()
demoMethods = do
  putStrLn "\n--- HTTP Method Properties ---\n"
  putStrLn "Method    Safe  Idempotent  Body"
  putStrLn "------    ----  ----------  ----"
  traverse_ showMethod allMethods
  where
    covering
    showMethod : Method -> IO ()
    showMethod m = putStrLn $
      padRight 10 ' ' (show m)
      ++ padRight 6 ' ' (if isSafe m then "yes" else "no")
      ++ padRight 12 ' ' (if isIdempotent m then "yes" else "no")
      ++ (if hasRequestBody m then "yes" else "no")
    padRight : Nat -> Char -> String -> String
    padRight n c s = s ++ pack (replicate (minus n (length s)) c)

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-httpd v0.1.0 -- HTTP/1.1 that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "HTTP version: " ++ httpVersion
  putStrLn $ "Default port: " ++ show (cast {to=Nat} defaultPort)
  putStrLn $ "Max header size: " ++ show maxHeaderSize ++ " bytes"
  putStrLn $ "Max body size: " ++ show maxBodySize ++ " bytes"

  demoMethods
  demoRouting
  demoSerialisation

  putStrLn "\n--- All dispatch proven safe at compile time ---"
  putStrLn "Build with: idris2 --build proven-httpd.ipkg"
  putStrLn "Run with:   ./build/exec/proven-httpd"
