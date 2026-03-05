-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-amqp skeleton.
-- | Re-exports AMQP.Types and defines protocol constants for
-- | AMQP 0-9-1 message queuing.

module AMQP

import public AMQP.Types

%default total

||| Default AMQP TCP port (plaintext).
public export
amqpPort : Nat
amqpPort = 5672

||| Default AMQPS TCP port (TLS).
public export
amqpsPort : Nat
amqpsPort = 5671

||| Maximum frame size in bytes per AMQP 0-9-1 negotiation default.
public export
maxFrameSize : Nat
maxFrameSize = 131072

||| Default heartbeat interval in seconds.
public export
heartbeatInterval : Nat
heartbeatInterval = 60
