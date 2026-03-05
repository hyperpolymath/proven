-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for RTSP (RFC 7826 - Real Time Streaming Protocol).
||| All types are closed sum types with Show instances.
module RTSP.Types

%default total

---------------------------------------------------------------------------
-- RTSP Method (RFC 7826 Section 13)
---------------------------------------------------------------------------

||| RTSP request methods as defined in RFC 7826.
public export
data Method : Type where
  Describe     : Method
  Setup        : Method
  Play         : Method
  Pause        : Method
  Teardown     : Method
  GetParameter : Method
  SetParameter : Method
  Options      : Method
  Announce     : Method
  Record       : Method
  Redirect     : Method

public export
Show Method where
  show Describe     = "DESCRIBE"
  show Setup        = "SETUP"
  show Play         = "PLAY"
  show Pause        = "PAUSE"
  show Teardown     = "TEARDOWN"
  show GetParameter = "GET_PARAMETER"
  show SetParameter = "SET_PARAMETER"
  show Options      = "OPTIONS"
  show Announce     = "ANNOUNCE"
  show Record       = "RECORD"
  show Redirect     = "REDIRECT"

---------------------------------------------------------------------------
-- Transport Protocol
---------------------------------------------------------------------------

||| Transport protocols for RTSP media delivery.
public export
data TransportProtocol : Type where
  RTP_AVP_UDP           : TransportProtocol
  RTP_AVP_TCP           : TransportProtocol
  RTP_AVP_UDP_Multicast : TransportProtocol

public export
Show TransportProtocol where
  show RTP_AVP_UDP           = "RTP/AVP/UDP"
  show RTP_AVP_TCP           = "RTP/AVP/TCP"
  show RTP_AVP_UDP_Multicast = "RTP/AVP/UDP;multicast"

---------------------------------------------------------------------------
-- Session State
---------------------------------------------------------------------------

||| RTSP session state machine states.
public export
data SessionState : Type where
  Init      : SessionState
  Ready     : SessionState
  Playing   : SessionState
  Recording : SessionState

public export
Show SessionState where
  show Init      = "Init"
  show Ready     = "Ready"
  show Playing   = "Playing"
  show Recording = "Recording"

---------------------------------------------------------------------------
-- Status Code
---------------------------------------------------------------------------

||| RTSP response status codes (subset of RFC 7826 Section 17).
public export
data StatusCode : Type where
  OK                  : StatusCode
  MovedPermanently    : StatusCode
  MovedTemporarily    : StatusCode
  BadRequest          : StatusCode
  Unauthorized        : StatusCode
  NotFound            : StatusCode
  MethodNotAllowed    : StatusCode
  NotAcceptable       : StatusCode
  SessionNotFound     : StatusCode
  InternalServerError : StatusCode
  NotImplemented      : StatusCode
  ServiceUnavailable  : StatusCode

public export
Show StatusCode where
  show OK                  = "200 OK"
  show MovedPermanently    = "301 Moved Permanently"
  show MovedTemporarily    = "302 Moved Temporarily"
  show BadRequest          = "400 Bad Request"
  show Unauthorized        = "401 Unauthorized"
  show NotFound            = "404 Not Found"
  show MethodNotAllowed    = "405 Method Not Allowed"
  show NotAcceptable       = "406 Not Acceptable"
  show SessionNotFound     = "454 Session Not Found"
  show InternalServerError = "500 Internal Server Error"
  show NotImplemented      = "501 Not Implemented"
  show ServiceUnavailable  = "503 Service Unavailable"
