// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

@@warning("-27")

/**
 * Proven - Code that cannot crash
 *
 * ReScript bindings for verified safety functions.
 */

module SafeMath = Proven_SafeMath
module SafeString = Proven_SafeString
module SafeUrl = Proven_SafeUrl
module SafeEmail = Proven_SafeEmail
module SafePath = Proven_SafePath
module SafeCrypto = Proven_SafeCrypto
module SafeNetwork = Proven_SafeNetwork

let version = "0.3.0"

/** Initialize the WASM module */
@module("../../../javascript/src/wasm.js")
external init: unit => promise<unit> = "init"

/** Check if initialized */
@module("../../../javascript/src/wasm.js")
external isInitialized: unit => bool = "isInitialized"
