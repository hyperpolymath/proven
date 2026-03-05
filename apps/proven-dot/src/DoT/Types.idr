-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core DNS over TLS types as closed sum types.
-- | Models session states, padding strategies, and error reasons
-- | per RFC 7858.
module DoT.Types

%default total

-------------------------------------------------------------------------------
-- Session States
-------------------------------------------------------------------------------

||| TLS session states for DNS over TLS connections (RFC 7858).
public export
data SessionState : Type where
  Connecting   : SessionState
  Handshaking  : SessionState
  Established  : SessionState
  Closing      : SessionState
  Closed       : SessionState

||| Show instance for SessionState.
export
Show SessionState where
  show Connecting  = "Connecting"
  show Handshaking = "Handshaking"
  show Established = "Established"
  show Closing     = "Closing"
  show Closed      = "Closed"

-------------------------------------------------------------------------------
-- Padding Strategies
-------------------------------------------------------------------------------

||| Padding strategies for DNS over TLS to resist traffic analysis.
public export
data PaddingStrategy : Type where
  NoPadding      : PaddingStrategy
  BlockPadding   : PaddingStrategy
  RandomPadding  : PaddingStrategy

||| Show instance for PaddingStrategy.
export
Show PaddingStrategy where
  show NoPadding     = "NoPadding"
  show BlockPadding  = "BlockPadding"
  show RandomPadding = "RandomPadding"

-------------------------------------------------------------------------------
-- Error Reasons
-------------------------------------------------------------------------------

||| Error reasons specific to DNS over TLS connections.
public export
data ErrorReason : Type where
  HandshakeFailed    : ErrorReason
  CertificateInvalid : ErrorReason
  Timeout            : ErrorReason
  UpstreamError      : ErrorReason

||| Show instance for ErrorReason.
export
Show ErrorReason where
  show HandshakeFailed    = "HandshakeFailed"
  show CertificateInvalid = "CertificateInvalid"
  show Timeout            = "Timeout"
  show UpstreamError      = "UpstreamError"
