-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-coap skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import CoAP

%default total

||| All CoAP method constructors for demonstration.
allMethods : List Method
allMethods = [Get, Post, Put, Delete]

||| All CoAP response code constructors for demonstration.
allResponseCodes : List ResponseCode
allResponseCodes =
  [ Created, Deleted, Valid, Changed, Content
  , BadRequest, Unauthorized, BadOption, Forbidden, NotFound
  , MethodNotAllowed, NotAcceptable, PreconditionFailed
  , RequestEntityTooLarge, UnsupportedContentFormat
  , InternalServerError, NotImplemented, BadGateway
  , ServiceUnavailable, GatewayTimeout, ProxyingNotSupported ]

||| All CoAP message type constructors for demonstration.
allMessageTypes : List MessageType
allMessageTypes = [Confirmable, NonConfirmable, Acknowledgement, Reset]

||| All CoAP content format constructors for demonstration.
allContentFormats : List ContentFormat
allContentFormats = [TextPlain, LinkFormat, XML, OctetStream, EXI, JSON, CBOR]

main : IO ()
main = do
  putStrLn "proven-coap: RFC 7252 Constrained Application Protocol"
  putStrLn $ "  CoAP port:        " ++ show coapPort
  putStrLn $ "  CoAPS port:       " ++ show coapsPort
  putStrLn $ "  Max token length: " ++ show maxTokenLength
  putStrLn $ "  Max payload size: " ++ show maxPayloadSize
  putStrLn $ "  Methods:          " ++ show allMethods
  putStrLn $ "  Response codes:   " ++ show allResponseCodes
  putStrLn $ "  Message types:    " ++ show allMessageTypes
  putStrLn $ "  Content formats:  " ++ show allContentFormats
