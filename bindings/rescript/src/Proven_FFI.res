// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven FFI - Bridge to the JavaScript FFI layer.
 *
 * The JavaScript FFI layer (bindings/javascript/src/ffi.js) handles
 * Deno.dlopen / library loading and lifetime management. This module
 * re-exports those loader functions for use by ReScript wrappers.
 *
 * This module does NOT load libproven directly.
 */

/** Re-export FFI utilities from the JavaScript FFI layer. */

@module("../../javascript/src/ffi.js")
external getLib: unit => {..} = "getLib"

@module("../../javascript/src/ffi.js")
external isAvailable: unit => bool = "isAvailable"

@module("../../javascript/src/ffi.js")
external getLoadError: unit => Nullable.t<string> = "getLoadError"

@module("../../javascript/src/ffi.js")
external closeFfi: unit => unit = "close"

@module("../../javascript/src/ffi.js")
external encodeString: string => Uint8Array.t = "encodeString"

/** Re-export result utilities from the JavaScript result module. */

@module("../../javascript/src/result.js")
external jsOk: 'a => {..} = "ok"

@module("../../javascript/src/result.js")
external jsErr: string => {..} = "err"

@module("../../javascript/src/result.js")
external jsIsOk: {..} => bool = "isOk"

@module("../../javascript/src/result.js")
external jsIsErr: {..} => bool = "isErr"

/**
 * Initialize the Proven library.
 *
 * The JavaScript FFI layer initializes lazily on first call to getLib().
 * Call this explicitly if you want to detect load errors early.
 *
 * @returns true if the library is available.
 */
let init = (): bool => {
  isAvailable()
}

/**
 * Check if the native library is loaded and initialized.
 *
 * @returns true if libproven is loaded and ready.
 */
let isInitialized = (): bool => {
  isAvailable()
}

/**
 * Explicitly close the library handle.
 * Call this at shutdown to cleanly release resources.
 */
let deinit = (): unit => {
  closeFfi()
}
