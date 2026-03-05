-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core SIP/VoIP protocol types as closed sum types.
-- | Models SIP methods (RFC 3261 Section 7.1), response codes (Section 21),
-- | and dialog states (Section 12).
module VoIP.Types

%default total

-------------------------------------------------------------------------------
-- SIP Methods
-------------------------------------------------------------------------------

||| SIP request methods as defined in RFC 3261 and extensions.
public export
data Method : Type where
  Invite    : Method
  Ack       : Method
  Bye       : Method
  Cancel    : Method
  Register  : Method
  Options   : Method
  Info      : Method
  Update    : Method
  Subscribe : Method
  Notify    : Method
  Refer     : Method
  Message   : Method
  Prack     : Method

||| Show instance for Method.
export
Show Method where
  show Invite    = "INVITE"
  show Ack       = "ACK"
  show Bye       = "BYE"
  show Cancel    = "CANCEL"
  show Register  = "REGISTER"
  show Options   = "OPTIONS"
  show Info      = "INFO"
  show Update    = "UPDATE"
  show Subscribe = "SUBSCRIBE"
  show Notify    = "NOTIFY"
  show Refer     = "REFER"
  show Message   = "MESSAGE"
  show Prack     = "PRACK"

-------------------------------------------------------------------------------
-- SIP Response Codes
-------------------------------------------------------------------------------

||| SIP response codes from RFC 3261 Section 21.
||| Grouped by class: 1xx provisional, 2xx success, 3xx redirection,
||| 4xx client error, 5xx server error.
public export
data ResponseCode : Type where
  Trying              : ResponseCode
  Ringing             : ResponseCode
  SessionProgress     : ResponseCode
  OK                  : ResponseCode
  MultipleChoices     : ResponseCode
  MovedPermanently    : ResponseCode
  MovedTemporarily    : ResponseCode
  BadRequest          : ResponseCode
  Unauthorized        : ResponseCode
  Forbidden           : ResponseCode
  NotFound            : ResponseCode
  MethodNotAllowed    : ResponseCode
  RequestTimeout      : ResponseCode
  BusyHere            : ResponseCode
  Decline             : ResponseCode
  ServerInternalError : ResponseCode
  ServiceUnavailable  : ResponseCode

||| Show instance for ResponseCode, including the numeric status code.
export
Show ResponseCode where
  show Trying              = "100 Trying"
  show Ringing             = "180 Ringing"
  show SessionProgress     = "183 Session Progress"
  show OK                  = "200 OK"
  show MultipleChoices     = "300 Multiple Choices"
  show MovedPermanently    = "301 Moved Permanently"
  show MovedTemporarily    = "302 Moved Temporarily"
  show BadRequest          = "400 Bad Request"
  show Unauthorized        = "401 Unauthorized"
  show Forbidden           = "403 Forbidden"
  show NotFound            = "404 Not Found"
  show MethodNotAllowed    = "405 Method Not Allowed"
  show RequestTimeout      = "408 Request Timeout"
  show BusyHere            = "486 Busy Here"
  show Decline             = "603 Decline"
  show ServerInternalError = "500 Server Internal Error"
  show ServiceUnavailable  = "503 Service Unavailable"

-------------------------------------------------------------------------------
-- Dialog States
-------------------------------------------------------------------------------

||| SIP dialog states as defined in RFC 3261 Section 12.
public export
data DialogState : Type where
  Early      : DialogState
  Confirmed  : DialogState
  Terminated : DialogState

||| Show instance for DialogState.
export
Show DialogState where
  show Early      = "Early"
  show Confirmed  = "Confirmed"
  show Terminated = "Terminated"
