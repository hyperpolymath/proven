-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 7252 Constrained Application Protocol.
-- | Defines request methods, response codes, message types, and content
-- | formats as closed sum types with Show instances.

module CoAP.Types

%default total

||| CoAP request methods per RFC 7252 Section 5.8.
public export
data Method : Type where
  Get    : Method
  Post   : Method
  Put    : Method
  Delete : Method

public export
Show Method where
  show Get    = "Get"
  show Post   = "Post"
  show Put    = "Put"
  show Delete = "Delete"

||| CoAP response codes per RFC 7252 Section 5.9 and Section 12.1.2.
public export
data ResponseCode : Type where
  Created                  : ResponseCode
  Deleted                  : ResponseCode
  Valid                    : ResponseCode
  Changed                  : ResponseCode
  Content                  : ResponseCode
  BadRequest               : ResponseCode
  Unauthorized             : ResponseCode
  BadOption                : ResponseCode
  Forbidden                : ResponseCode
  NotFound                 : ResponseCode
  MethodNotAllowed         : ResponseCode
  NotAcceptable            : ResponseCode
  PreconditionFailed       : ResponseCode
  RequestEntityTooLarge    : ResponseCode
  UnsupportedContentFormat : ResponseCode
  InternalServerError      : ResponseCode
  NotImplemented           : ResponseCode
  BadGateway               : ResponseCode
  ServiceUnavailable       : ResponseCode
  GatewayTimeout           : ResponseCode
  ProxyingNotSupported     : ResponseCode

public export
Show ResponseCode where
  show Created                  = "Created"
  show Deleted                  = "Deleted"
  show Valid                    = "Valid"
  show Changed                  = "Changed"
  show Content                  = "Content"
  show BadRequest               = "BadRequest"
  show Unauthorized             = "Unauthorized"
  show BadOption                = "BadOption"
  show Forbidden                = "Forbidden"
  show NotFound                 = "NotFound"
  show MethodNotAllowed         = "MethodNotAllowed"
  show NotAcceptable            = "NotAcceptable"
  show PreconditionFailed       = "PreconditionFailed"
  show RequestEntityTooLarge    = "RequestEntityTooLarge"
  show UnsupportedContentFormat = "UnsupportedContentFormat"
  show InternalServerError      = "InternalServerError"
  show NotImplemented           = "NotImplemented"
  show BadGateway               = "BadGateway"
  show ServiceUnavailable       = "ServiceUnavailable"
  show GatewayTimeout           = "GatewayTimeout"
  show ProxyingNotSupported     = "ProxyingNotSupported"

||| CoAP message types per RFC 7252 Section 3.
public export
data MessageType : Type where
  Confirmable     : MessageType
  NonConfirmable  : MessageType
  Acknowledgement : MessageType
  Reset           : MessageType

public export
Show MessageType where
  show Confirmable     = "Confirmable"
  show NonConfirmable  = "NonConfirmable"
  show Acknowledgement = "Acknowledgement"
  show Reset           = "Reset"

||| CoAP content format identifiers per RFC 7252 Section 12.3.
public export
data ContentFormat : Type where
  TextPlain    : ContentFormat
  LinkFormat   : ContentFormat
  XML          : ContentFormat
  OctetStream  : ContentFormat
  EXI          : ContentFormat
  JSON         : ContentFormat
  CBOR         : ContentFormat

public export
Show ContentFormat where
  show TextPlain   = "TextPlain"
  show LinkFormat  = "LinkFormat"
  show XML         = "XML"
  show OctetStream = "OctetStream"
  show EXI         = "EXI"
  show JSON        = "JSON"
  show CBOR        = "CBOR"
