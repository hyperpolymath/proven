-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebSocket Close Status Codes (RFC 6455 Section 7.4)
--
-- Defines the standard close status codes as a sum type with numeric
-- encoding, classification (normal vs error), and Eq/Show instances.
-- Unknown or reserved codes are handled as typed values, not crashes.

module WS.CloseCode

%default total

-- ============================================================================
-- Close Status Codes (RFC 6455 Section 7.4.1)
-- ============================================================================

||| Standard WebSocket close status codes.
||| Each constructor corresponds to a registered status code.
public export
data CloseCode : Type where
  ||| 1000 — Normal closure; the connection successfully completed its purpose.
  Normal            : CloseCode
  ||| 1001 — Endpoint is going away (e.g., server shutting down, browser navigating).
  GoingAway         : CloseCode
  ||| 1002 — Endpoint received a frame that violates the protocol.
  ProtocolError     : CloseCode
  ||| 1003 — Endpoint received data it cannot accept (e.g., text-only got binary).
  UnsupportedData   : CloseCode
  ||| 1005 — No status code was present in the close frame.
  ||| MUST NOT be sent in a Close frame; only used internally.
  NoStatus          : CloseCode
  ||| 1006 — Connection was closed abnormally (no close frame received).
  ||| MUST NOT be sent in a Close frame; only used internally.
  Abnormal          : CloseCode
  ||| 1007 — Payload data was not consistent with the message type
  ||| (e.g., non-UTF-8 in a text message).
  InvalidPayload    : CloseCode
  ||| 1008 — Endpoint received a message that violates its policy.
  PolicyViolation   : CloseCode
  ||| 1009 — Message is too big for the endpoint to process.
  MessageTooBig     : CloseCode
  ||| 1010 — Client expected server to negotiate extensions via
  ||| Sec-WebSocket-Extensions but the server did not.
  MandatoryExtension : CloseCode
  ||| 1011 — Server encountered an unexpected condition that prevented
  ||| it from fulfilling the request.
  InternalError     : CloseCode

public export
Eq CloseCode where
  Normal             == Normal             = True
  GoingAway          == GoingAway          = True
  ProtocolError      == ProtocolError      = True
  UnsupportedData    == UnsupportedData    = True
  NoStatus           == NoStatus           = True
  Abnormal           == Abnormal           = True
  InvalidPayload     == InvalidPayload     = True
  PolicyViolation    == PolicyViolation    = True
  MessageTooBig      == MessageTooBig      = True
  MandatoryExtension == MandatoryExtension = True
  InternalError      == InternalError      = True
  _                  == _                  = False

public export
Show CloseCode where
  show Normal             = "Normal(1000)"
  show GoingAway          = "GoingAway(1001)"
  show ProtocolError      = "ProtocolError(1002)"
  show UnsupportedData    = "UnsupportedData(1003)"
  show NoStatus           = "NoStatus(1005)"
  show Abnormal           = "Abnormal(1006)"
  show InvalidPayload     = "InvalidPayload(1007)"
  show PolicyViolation    = "PolicyViolation(1008)"
  show MessageTooBig      = "MessageTooBig(1009)"
  show MandatoryExtension = "MandatoryExtension(1010)"
  show InternalError      = "InternalError(1011)"

-- ============================================================================
-- Numeric Encoding
-- ============================================================================

||| Convert a close code to its 16-bit wire value.
public export
closeCodeToWord : CloseCode -> Bits16
closeCodeToWord Normal             = 1000
closeCodeToWord GoingAway          = 1001
closeCodeToWord ProtocolError      = 1002
closeCodeToWord UnsupportedData    = 1003
closeCodeToWord NoStatus           = 1005
closeCodeToWord Abnormal           = 1006
closeCodeToWord InvalidPayload     = 1007
closeCodeToWord PolicyViolation    = 1008
closeCodeToWord MessageTooBig      = 1009
closeCodeToWord MandatoryExtension = 1010
closeCodeToWord InternalError      = 1011

||| Parse a 16-bit wire value to a close code.
||| Returns Nothing for unrecognised or reserved codes.
public export
closeCodeFromWord : Bits16 -> Maybe CloseCode
closeCodeFromWord 1000 = Just Normal
closeCodeFromWord 1001 = Just GoingAway
closeCodeFromWord 1002 = Just ProtocolError
closeCodeFromWord 1003 = Just UnsupportedData
closeCodeFromWord 1005 = Just NoStatus
closeCodeFromWord 1006 = Just Abnormal
closeCodeFromWord 1007 = Just InvalidPayload
closeCodeFromWord 1008 = Just PolicyViolation
closeCodeFromWord 1009 = Just MessageTooBig
closeCodeFromWord 1010 = Just MandatoryExtension
closeCodeFromWord 1011 = Just InternalError
closeCodeFromWord _    = Nothing

-- ============================================================================
-- Classification
-- ============================================================================

||| Check if a close code represents normal termination.
||| Only codes 1000 (Normal) and 1001 (GoingAway) are "clean" closures.
public export
isNormalClose : CloseCode -> Bool
isNormalClose Normal    = True
isNormalClose GoingAway = True
isNormalClose _         = False

||| Check if a close code indicates an error condition.
public export
isErrorClose : CloseCode -> Bool
isErrorClose code = not (isNormalClose code) && code /= NoStatus

||| Check if a close code may be sent in a Close frame.
||| Codes 1005 (NoStatus) and 1006 (Abnormal) are internal-only and
||| MUST NOT appear on the wire (RFC 6455 Section 7.4.1).
public export
isSendable : CloseCode -> Bool
isSendable NoStatus = False
isSendable Abnormal = False
isSendable _        = True

||| Get a human-readable description for a close code.
public export
closeReason : CloseCode -> String
closeReason Normal             = "Normal closure"
closeReason GoingAway          = "Endpoint going away"
closeReason ProtocolError      = "Protocol error"
closeReason UnsupportedData    = "Unsupported data type"
closeReason NoStatus           = "No status code present"
closeReason Abnormal           = "Abnormal closure (no close frame)"
closeReason InvalidPayload     = "Invalid payload data"
closeReason PolicyViolation    = "Policy violation"
closeReason MessageTooBig      = "Message too big"
closeReason MandatoryExtension = "Mandatory extension missing"
closeReason InternalError      = "Internal server error"

||| Check if a raw 16-bit value is in the valid application-use range.
||| Application codes are 4000-4999 (RFC 6455 Section 7.4.2).
public export
isApplicationCode : Bits16 -> Bool
isApplicationCode w = w >= 4000 && w <= 4999

||| Check if a raw 16-bit value is in the valid private-use range.
||| Private codes are 3000-3999 (reserved for libraries/frameworks).
public export
isPrivateCode : Bits16 -> Bool
isPrivateCode w = w >= 3000 && w <= 3999
