-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-kerberos skeleton.
-- | Re-exports Kerberos.Types and defines protocol constants for
-- | RFC 4120 Kerberos V5.

module Kerberos

import public Kerberos.Types

%default total

||| Default KDC TCP/UDP port per RFC 4120 Section 7.2.1.
public export
kdcPort : Nat
kdcPort = 88

||| Default kpasswd TCP/UDP port per RFC 3244.
public export
kpasswdPort : Nat
kpasswdPort = 464

||| Default ticket lifetime in seconds (10 hours).
public export
defaultTicketLifetime : Nat
defaultTicketLifetime = 36000

||| Maximum renewable lifetime in seconds (7 days).
public export
maxRenewableLifetime : Nat
maxRenewableLifetime = 604800
