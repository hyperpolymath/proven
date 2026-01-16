// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

@@warning("-27")

/**
 * Proven - Code that cannot crash
 *
 * ReScript bindings for verified safety functions.
 * Calls Idris 2 verified code via Zig ABI (WASM/WASI).
 */

// Core modules
module SafeMath = Proven_SafeMath
module SafeString = Proven_SafeString
module SafeUrl = Proven_SafeUrl
module SafeEmail = Proven_SafeEmail
module SafePath = Proven_SafePath
module SafeCrypto = Proven_SafeCrypto
module SafeNetwork = Proven_SafeNetwork

// v0.8.0 Network Extended modules
module SafeHeader = Proven_SafeHeader
module SafeCookie = Proven_SafeCookie
module SafeContentType = Proven_SafeContentType

// v0.9.0 Type-safe domain modules
module SafeUuid = Proven_SafeUuid
module SafeCurrency = Proven_SafeCurrency
module SafePhone = Proven_SafePhone
module SafeHex = Proven_SafeHex

// FFI layer
module FFI = Proven_FFI

let version = "0.9.0"

/** Initialize the WASM module
 * @param wasmPath Path to the proven.wasm file
 */
let init = Proven_FFI.init

/** Check if initialized */
let isInitialized = Proven_FFI.isInitialized

/** Cleanup */
let deinit = Proven_FFI.deinit
