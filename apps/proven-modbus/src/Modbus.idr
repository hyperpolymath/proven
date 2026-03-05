-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-modbus skeleton.
-- | Re-exports Modbus.Types and defines protocol constants for
-- | Modbus TCP industrial protocol.

module Modbus

import public Modbus.Types

%default total

||| Default Modbus TCP port.
public export
modbusPort : Nat
modbusPort = 502

||| Maximum number of registers per read/write request.
public export
maxRegisters : Nat
maxRegisters = 125

||| Maximum number of coils per read/write request.
public export
maxCoils : Nat
maxCoils = 2000

||| Minimum valid unit identifier.
public export
unitIDMin : Nat
unitIDMin = 1

||| Maximum valid unit identifier.
public export
unitIDMax : Nat
unitIDMax = 247
