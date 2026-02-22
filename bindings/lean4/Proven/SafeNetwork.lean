/- SPDX-License-Identifier: PMPL-1.0-or-later
   Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -/

/-!
# Proven.SafeNetwork - IP address parsing and classification

Provides safe IPv4 address parsing and classification (private, loopback).
Every operation delegates to the formally verified Idris 2 core via the Zig
FFI bridge (`libproven`). No networking logic is reimplemented in Lean.
-/

import Proven.FFI

namespace Proven.SafeNetwork

open Proven.FFI

/-- Error type for network operations. -/
structure NetworkError where
  status : ProvenStatus
  deriving Repr, BEq, Inhabited

instance : ToString NetworkError := ⟨fun e => s!"NetworkError: {e.status}"⟩

/-- Alias for network operation results. -/
abbrev NetworkResult (a : Type) := Except NetworkError a

-- ============================================================================
-- IPv4 address type
-- ============================================================================

/-- A validated IPv4 address. Wraps the FFI `IPv4Address` type. -/
structure IPv4 where
  /-- The underlying FFI address. -/
  addr : IPv4Address
  deriving Repr, BEq, Inhabited

/-- Display an IPv4 address in dotted-decimal notation. -/
instance : ToString IPv4 where
  toString v :=
    s!"{v.addr.a}.{v.addr.b}.{v.addr.c}.{v.addr.d}"

-- ============================================================================
-- IPv4 parsing
-- ============================================================================

/-- Parse an IPv4 address string (e.g., "192.168.1.1").
    Delegates to `proven_network_parse_ipv4`. -/
def parseIpv4 (input : String) : IO (NetworkResult IPv4) := do
  let r <- provenNetworkParseIpv4 input.toUTF8
  let s := ProvenStatus.ofInt32 r.status
  match s with
  | .ok => return .ok { addr := r.address }
  | _   => return .error { status := s }

/-- Parse an IPv4 address, returning `Option`. -/
def parseIpv4? (input : String) : IO (Option IPv4) := do
  let r <- parseIpv4 input
  return r.toOption

-- ============================================================================
-- IPv4 classification
-- ============================================================================

/-- Check if an IPv4 address is private (RFC 1918).
    Private ranges: 10.x.x.x, 172.16-31.x.x, 192.168.x.x.
    Delegates to `proven_network_ipv4_is_private`. -/
def isPrivate (ip : IPv4) : IO Bool :=
  provenNetworkIpv4IsPrivate ip.addr

/-- Check if an IPv4 address is loopback (127.0.0.0/8).
    Delegates to `proven_network_ipv4_is_loopback`. -/
def isLoopback (ip : IPv4) : IO Bool :=
  provenNetworkIpv4IsLoopback ip.addr

/-- Parse an IPv4 string and check if it is private. -/
def isPrivateStr (input : String) : IO (Option Bool) := do
  let ip <- parseIpv4? input
  match ip with
  | some v => do
    let r <- isPrivate v
    return some r
  | none => return none

/-- Parse an IPv4 string and check if it is loopback. -/
def isLoopbackStr (input : String) : IO (Option Bool) := do
  let ip <- parseIpv4? input
  match ip with
  | some v => do
    let r <- isLoopback v
    return some r
  | none => return none

end Proven.SafeNetwork
