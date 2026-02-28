-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-wasm: A WebAssembly module builder with verified memory safety.
--
-- Architecture:
--   - ValType: 7 WASM value types with binary encoding, classification
--   - Instruction: Core WASM instructions (numeric, memory, control, variable)
--   - Module: WASM module record with sections, validation, binary header
--   - Memory: Linear memory model with page-based allocation, bounds checking
--   - Types: Function types, global types, table types, export descriptors
--
-- This module defines the core WASM constants and re-exports submodules.

module WASM

import public WASM.ValType
import public WASM.Instruction
import public WASM.Module
import public WASM.Memory
import public WASM.Types

||| The WASM binary magic number: \0asm (0x00 0x61 0x73 0x6D).
public export
wasmMagicBytes : List Bits8
wasmMagicBytes = [0x00, 0x61, 0x73, 0x6D]

||| The WASM binary version number (version 1, little-endian).
public export
wasmVersionBytes : List Bits8
wasmVersionBytes = [0x01, 0x00, 0x00, 0x00]

||| WASM memory page size in bytes (64 KiB).
public export
wasmPageSize : Nat
wasmPageSize = 65536

||| Maximum number of memory pages (65536 = 4 GiB address space).
public export
wasmMaxPages : Nat
wasmMaxPages = 65536

||| Maximum number of functions per module (implementation limit).
public export
wasmMaxFunctions : Nat
wasmMaxFunctions = 1000000
