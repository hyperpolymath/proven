-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebSocket Opening Handshake (RFC 6455 Section 4)
--
-- Defines the HTTP Upgrade handshake required to establish a WebSocket
-- connection.  Validates the client's request (correct headers, version,
-- Sec-WebSocket-Key) and constructs the server's accept response.
-- The accept key computation is modelled (the actual SHA-1 + base64
-- would be performed by proven's SafeHash).

module WS.Handshake

%default total

-- ============================================================================
-- HTTP Header Representation
-- ============================================================================

||| A single HTTP header as a name-value pair.
||| Header names are case-insensitive per HTTP/1.1 (RFC 7230).
public export
record HttpHeader where
  constructor MkHttpHeader
  ||| Header name (stored lowercase for comparison)
  name  : String
  ||| Header value (preserved as-is)
  value : String

public export
Show HttpHeader where
  show h = h.name ++ ": " ++ h.value

-- ============================================================================
-- Handshake Request (Client -> Server)
-- ============================================================================

||| A parsed WebSocket upgrade request from the client.
public export
record HandshakeRequest where
  constructor MkHandshakeRequest
  ||| HTTP method (MUST be GET)
  method          : String
  ||| Request URI (e.g., "/chat")
  requestUri      : String
  ||| HTTP version (MUST be at least 1.1)
  httpVersion     : String
  ||| All HTTP headers
  headers         : List HttpHeader
  ||| Extracted Sec-WebSocket-Key (24-char base64)
  webSocketKey    : Maybe String
  ||| Extracted Sec-WebSocket-Version (MUST be "13")
  webSocketVersion : Maybe String
  ||| Extracted Sec-WebSocket-Protocol (optional)
  subprotocols    : List String

public export
Show HandshakeRequest where
  show req = req.method ++ " " ++ req.requestUri ++ " HTTP/" ++ req.httpVersion

-- ============================================================================
-- Handshake Validation (RFC 6455 Section 4.2.1)
-- ============================================================================

||| Reasons a handshake request can be rejected.
public export
data HandshakeError : Type where
  ||| Method is not GET
  NotGetMethod          : (method : String) -> HandshakeError
  ||| HTTP version is below 1.1
  HttpVersionTooLow     : (version : String) -> HandshakeError
  ||| Missing "Upgrade: websocket" header
  MissingUpgradeHeader  : HandshakeError
  ||| Missing "Connection: Upgrade" header
  MissingConnectionHeader : HandshakeError
  ||| Missing Sec-WebSocket-Key header
  MissingWebSocketKey   : HandshakeError
  ||| Sec-WebSocket-Key is not valid base64 (must decode to 16 bytes)
  InvalidWebSocketKey   : (key : String) -> HandshakeError
  ||| Sec-WebSocket-Version is not "13"
  UnsupportedVersion    : (version : String) -> HandshakeError
  ||| Missing Sec-WebSocket-Version header
  MissingVersion        : HandshakeError

public export
Show HandshakeError where
  show (NotGetMethod m)         = "Method must be GET, got: " ++ m
  show (HttpVersionTooLow v)    = "HTTP version too low: " ++ v
  show MissingUpgradeHeader     = "Missing 'Upgrade: websocket' header"
  show MissingConnectionHeader  = "Missing 'Connection: Upgrade' header"
  show MissingWebSocketKey      = "Missing Sec-WebSocket-Key header"
  show (InvalidWebSocketKey k)  = "Invalid Sec-WebSocket-Key: " ++ k
  show (UnsupportedVersion v)   = "Unsupported WebSocket version: " ++ v
  show MissingVersion           = "Missing Sec-WebSocket-Version header"

||| Look up a header by name (case-insensitive).
public export
findHeader : String -> List HttpHeader -> Maybe String
findHeader name = map (.value) . find (\h => h.name == name)

||| The WebSocket GUID used in the accept key computation (RFC 6455 Section 4.2.2).
public export
wsGUID : String
wsGUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

||| Validate a handshake request per RFC 6455 Section 4.2.1.
||| Returns the first validation error found, or the request unchanged.
public export
validateHandshake : HandshakeRequest -> Either HandshakeError HandshakeRequest
validateHandshake req =
  -- 1. MUST be GET
  if req.method /= "GET"
    then Left (NotGetMethod req.method)
  -- 2. HTTP version MUST be >= 1.1
  else if req.httpVersion /= "1.1" && req.httpVersion /= "2" && req.httpVersion /= "2.0"
    then Left (HttpVersionTooLow req.httpVersion)
  -- 3. MUST include "Upgrade: websocket"
  else case findHeader "upgrade" req.headers of
    Nothing => Left MissingUpgradeHeader
    Just v  => if v /= "websocket" && v /= "WebSocket"
                 then Left MissingUpgradeHeader
  -- 4. MUST include "Connection: Upgrade"
  else case findHeader "connection" req.headers of
    Nothing => Left MissingConnectionHeader
    Just _  =>
  -- 5. MUST include Sec-WebSocket-Key
  case req.webSocketKey of
    Nothing => Left MissingWebSocketKey
    Just key =>
      -- Key must be 24 characters (base64 of 16 bytes)
      if length key /= 24
        then Left (InvalidWebSocketKey key)
  -- 6. MUST include Sec-WebSocket-Version: 13
  else case req.webSocketVersion of
    Nothing  => Left MissingVersion
    Just ver => if ver /= "13"
                  then Left (UnsupportedVersion ver)
                  else Right req

-- ============================================================================
-- Handshake Response (Server -> Client)
-- ============================================================================

||| A WebSocket handshake accept response.
public export
record HandshakeResponse where
  constructor MkHandshakeResponse
  ||| HTTP status code (101 for Switching Protocols)
  statusCode      : Nat
  ||| HTTP status text
  statusText      : String
  ||| Response headers
  headers         : List HttpHeader
  ||| The computed Sec-WebSocket-Accept value
  acceptKey       : String
  ||| Selected subprotocol (if any)
  selectedProtocol : Maybe String

public export
Show HandshakeResponse where
  show resp = "HTTP/1.1 " ++ show resp.statusCode ++ " " ++ resp.statusText

||| Construct a handshake accept response.
||| The `acceptKey` parameter should be computed as:
|||   Base64(SHA-1(Sec-WebSocket-Key + wsGUID))
||| The actual computation is performed by proven's SafeHash module.
public export
makeAcceptResponse : (acceptKey : String) -> (protocol : Maybe String)
                   -> HandshakeResponse
makeAcceptResponse key proto = MkHandshakeResponse
  { statusCode      = 101
  , statusText      = "Switching Protocols"
  , headers         = [ MkHttpHeader "upgrade" "websocket"
                       , MkHttpHeader "connection" "Upgrade"
                       , MkHttpHeader "sec-websocket-accept" key
                       ] ++ maybe [] (\p => [MkHttpHeader "sec-websocket-protocol" p]) proto
  , acceptKey       = key
  , selectedProtocol = proto
  }

||| Construct an error response for a failed handshake.
public export
makeErrorResponse : HandshakeError -> (Nat, String)
makeErrorResponse (NotGetMethod _)        = (405, "Method Not Allowed")
makeErrorResponse (HttpVersionTooLow _)   = (505, "HTTP Version Not Supported")
makeErrorResponse MissingUpgradeHeader    = (400, "Missing Upgrade Header")
makeErrorResponse MissingConnectionHeader = (400, "Missing Connection Header")
makeErrorResponse MissingWebSocketKey     = (400, "Missing Sec-WebSocket-Key")
makeErrorResponse (InvalidWebSocketKey _) = (400, "Invalid Sec-WebSocket-Key")
makeErrorResponse (UnsupportedVersion _)  = (426, "Upgrade Required")
makeErrorResponse MissingVersion          = (400, "Missing Sec-WebSocket-Version")
