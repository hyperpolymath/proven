// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Proven FFI - WebAssembly bindings to Zig ABI
 *
 * This module loads the Proven WASM module and provides
 * typed bindings to all exported functions.
 */

/** Result status codes from the Zig FFI */
type provenStatus =
  | Ok
  | ErrNullPointer
  | ErrInvalidArgument
  | ErrOverflow
  | ErrUnderflow
  | ErrDivisionByZero
  | ErrParseFailure
  | ErrValidationFailed
  | ErrOutOfBounds
  | ErrEncodingError
  | ErrAllocationFailed
  | ErrNotImplemented

/** Convert status code to variant */
let statusFromInt = (code: int): provenStatus => {
  switch code {
  | 0 => Ok
  | -1 => ErrNullPointer
  | -2 => ErrInvalidArgument
  | -3 => ErrOverflow
  | -4 => ErrUnderflow
  | -5 => ErrDivisionByZero
  | -6 => ErrParseFailure
  | -7 => ErrValidationFailed
  | -8 => ErrOutOfBounds
  | -9 => ErrEncodingError
  | -10 => ErrAllocationFailed
  | _ => ErrNotImplemented
  }
}

/** WASM memory interface */
type wasmMemory

/** WASM instance interface */
type wasmInstance = {
  exports: wasmExports,
}
and wasmExports = {
  memory: wasmMemory,
  // Math
  proven_math_div: (. int, int) => {status: int, value: int},
  proven_math_mod: (. int, int) => {status: int, value: int},
  proven_math_add_checked: (. int, int) => {status: int, value: int},
  proven_math_sub_checked: (. int, int) => {status: int, value: int},
  proven_math_mul_checked: (. int, int) => {status: int, value: int},
  proven_math_abs_safe: (. int) => {status: int, value: int},
  proven_math_clamp: (. int, int, int) => int,
  proven_math_pow_checked: (. int, int) => {status: int, value: int},
  // Header
  proven_header_has_crlf: (. int, int) => {status: int, value: bool},
  proven_header_is_valid_name: (. int, int) => {status: int, value: bool},
  proven_header_is_dangerous: (. int, int) => {status: int, value: bool},
  proven_header_build_hsts: (. int, bool, bool) => {status: int, value: int, length: int},
  // Cookie
  proven_cookie_has_injection: (. int, int) => {status: int, value: bool},
  proven_cookie_validate_name: (. int, int) => {status: int, value: bool},
  proven_cookie_validate_value: (. int, int) => {status: int, value: bool},
  proven_cookie_get_prefix: (. int, int) => {status: int, value: int},
  // ContentType
  proven_content_type_can_sniff_dangerous: (. int, int) => {status: int, value: bool},
  proven_content_type_is_json: (. int, int, int, int) => {status: int, value: bool},
  proven_content_type_is_xml: (. int, int, int, int) => {status: int, value: bool},
  // Memory management
  proven_free_string: (. int) => unit,
  // Version
  proven_version_major: (. unit) => int,
  proven_version_minor: (. unit) => int,
  proven_version_patch: (. unit) => int,
  // Init
  proven_init: (. unit) => int,
  proven_deinit: (. unit) => unit,
  proven_is_initialized: (. unit) => bool,
}

/** WASM module state */
let wasmInstance: ref<option<wasmInstance>> = ref(None)

/** Load WASM module from URL or path */
@val external fetch: string => promise<'a> = "fetch"

/** WebAssembly instantiate */
@scope("WebAssembly") @val
external instantiateStreaming: (promise<'a>, 'b) => promise<{instance: wasmInstance}> = "instantiateStreaming"

/** Initialize the WASM module */
let init = async (wasmPath: string): result<unit, string> => {
  switch wasmInstance.contents {
  | Some(_) => Ok()
  | None =>
    try {
      let response = fetch(wasmPath)
      let result = await instantiateStreaming(response, {})
      wasmInstance := Some(result.instance)

      // Initialize the Proven runtime
      let initResult = result.instance.exports.proven_init(.)
      if initResult == 0 {
        Ok()
      } else {
        Error("Failed to initialize Proven runtime")
      }
    } catch {
    | _ => Error("Failed to load WASM module")
    }
  }
}

/** Check if WASM is initialized */
let isInitialized = (): bool => {
  switch wasmInstance.contents {
  | Some(instance) => instance.exports.proven_is_initialized(.)
  | None => false
  }
}

/** Get the WASM instance (throws if not initialized) */
let getInstance = (): result<wasmInstance, string> => {
  switch wasmInstance.contents {
  | Some(instance) => Ok(instance)
  | None => Error("WASM not initialized. Call Proven_FFI.init() first.")
  }
}

/** Get version string */
let version = (): result<string, string> => {
  switch getInstance() {
  | Error(e) => Error(e)
  | Ok(instance) =>
    let major = instance.exports.proven_version_major(.)
    let minor = instance.exports.proven_version_minor(.)
    let patch = instance.exports.proven_version_patch(.)
    Ok(`${Belt.Int.toString(major)}.${Belt.Int.toString(minor)}.${Belt.Int.toString(patch)}`)
  }
}

/** Cleanup the WASM module */
let deinit = (): unit => {
  switch wasmInstance.contents {
  | Some(instance) =>
    instance.exports.proven_deinit(.)
    wasmInstance := None
  | None => ()
  }
}
