-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Federation: Top-level module for the federation / decentralised
-- identity server. Re-exports all protocol types from
-- Federation.Types and defines server configuration constants.

module Federation

import public Federation.Types

%default total

------------------------------------------------------------------------
-- Server configuration constants
------------------------------------------------------------------------

||| The TCP port the federation server listens on (standard HTTPS).
public export
federationPort : Nat
federationPort = 443

||| Maximum payload size in bytes for incoming activities.
public export
maxPayloadSize : Nat
maxPayloadSize = 1048576

||| Timeout in seconds for activity delivery attempts.
public export
deliveryTimeout : Nat
deliveryTimeout = 30

||| Maximum recursion depth when resolving linked objects
||| (e.g. following reply chains, collection pages).
public export
maxRecursion : Nat
maxRecursion = 5
