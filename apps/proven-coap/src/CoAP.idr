-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-coap skeleton.
-- | Re-exports CoAP.Types and defines protocol constants for
-- | RFC 7252 Constrained Application Protocol.

module CoAP

import public CoAP.Types

%default total

||| Default CoAP UDP port per RFC 7252 Section 6.1.
public export
coapPort : Nat
coapPort = 5683

||| Default CoAP DTLS port per RFC 7252 Section 6.2.
public export
coapsPort : Nat
coapsPort = 5684

||| Maximum token length in bytes per RFC 7252 Section 3.
public export
maxTokenLength : Nat
maxTokenLength = 8

||| Default maximum payload size in bytes for constrained devices.
public export
maxPayloadSize : Nat
maxPayloadSize = 1024
