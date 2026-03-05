-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core DNS over HTTPS types as closed sum types.
-- | Models content types, request methods, wire formats,
-- | and error reasons per RFC 8484.
module DoH.Types

%default total

-------------------------------------------------------------------------------
-- Content Types
-------------------------------------------------------------------------------

||| Content types for DoH messages (RFC 8484 Section 6).
public export
data ContentType : Type where
  DNSMessage : ContentType
  DNSJson    : ContentType

||| Show instance for ContentType, rendering the MIME type.
export
Show ContentType where
  show DNSMessage = "application/dns-message"
  show DNSJson    = "application/dns-json"

-------------------------------------------------------------------------------
-- Request Methods
-------------------------------------------------------------------------------

||| HTTP request methods supported by DoH (RFC 8484 Section 4.1).
public export
data RequestMethod : Type where
  Get  : RequestMethod
  Post : RequestMethod

||| Show instance for RequestMethod.
export
Show RequestMethod where
  show Get  = "GET"
  show Post = "POST"

-------------------------------------------------------------------------------
-- Wire Formats
-------------------------------------------------------------------------------

||| Wire format for DNS messages in DoH.
public export
data WireFormat : Type where
  Binary : WireFormat
  Json   : WireFormat

||| Show instance for WireFormat.
export
Show WireFormat where
  show Binary = "Binary"
  show Json   = "Json"

-------------------------------------------------------------------------------
-- Error Reasons
-------------------------------------------------------------------------------

||| Error reasons specific to DoH processing.
public export
data ErrorReason : Type where
  BadContentType  : ErrorReason
  BadMethod       : ErrorReason
  PayloadTooLarge : ErrorReason
  UpstreamTimeout : ErrorReason
  UpstreamError   : ErrorReason

||| Show instance for ErrorReason.
export
Show ErrorReason where
  show BadContentType  = "BadContentType"
  show BadMethod       = "BadMethod"
  show PayloadTooLarge = "PayloadTooLarge"
  show UpstreamTimeout = "UpstreamTimeout"
  show UpstreamError   = "UpstreamError"
