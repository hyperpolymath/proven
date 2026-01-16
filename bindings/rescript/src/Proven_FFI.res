// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

@@warning("-27")

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

/** Result types for FFI calls */
type statusValueInt = {status: int, value: int}
type statusValueBool = {status: int, value: bool}
type statusValueIntLength = {status: int, value: int, length: int}

/** WASM exports interface */
type wasmExports = {
  memory: wasmMemory,
  // Math
  @as("proven_math_div") mathDiv: (. int, int) => statusValueInt,
  @as("proven_math_mod") mathMod: (. int, int) => statusValueInt,
  @as("proven_math_add_checked") mathAddChecked: (. int, int) => statusValueInt,
  @as("proven_math_sub_checked") mathSubChecked: (. int, int) => statusValueInt,
  @as("proven_math_mul_checked") mathMulChecked: (. int, int) => statusValueInt,
  @as("proven_math_abs_safe") mathAbsSafe: (. int) => statusValueInt,
  @as("proven_math_clamp") mathClamp: (. int, int, int) => int,
  @as("proven_math_pow_checked") mathPowChecked: (. int, int) => statusValueInt,
  // Header
  @as("proven_header_has_crlf") headerHasCrlf: (. int, int) => statusValueBool,
  @as("proven_header_is_valid_name") headerIsValidName: (. int, int) => statusValueBool,
  @as("proven_header_is_dangerous") headerIsDangerous: (. int, int) => statusValueBool,
  @as("proven_header_build_hsts") headerBuildHsts: (. int, bool, bool) => statusValueIntLength,
  // Cookie
  @as("proven_cookie_has_injection") cookieHasInjection: (. int, int) => statusValueBool,
  @as("proven_cookie_validate_name") cookieValidateName: (. int, int) => statusValueBool,
  @as("proven_cookie_validate_value") cookieValidateValue: (. int, int) => statusValueBool,
  @as("proven_cookie_get_prefix") cookieGetPrefix: (. int, int) => statusValueInt,
  // ContentType
  @as("proven_content_type_can_sniff_dangerous")
  contentTypeCanSniffDangerous: (. int, int) => statusValueBool,
  @as("proven_content_type_is_json") contentTypeIsJson: (. int, int, int, int) => statusValueBool,
  @as("proven_content_type_is_xml") contentTypeIsXml: (. int, int, int, int) => statusValueBool,
  // Memory management
  @as("proven_free_string") freeString: (. int) => unit,
  // Version
  @as("proven_version_major") versionMajor: (. unit) => int,
  @as("proven_version_minor") versionMinor: (. unit) => int,
  @as("proven_version_patch") versionPatch: (. unit) => int,
  // Init
  @as("proven_init") init_: (. unit) => int,
  @as("proven_deinit") deinit_: (. unit) => unit,
  @as("proven_is_initialized") isInitialized_: (. unit) => bool,
}

/** WASM instance interface */
type wasmInstance = {exports: wasmExports}

/** Instantiate result type */
type instantiateResult = {instance: wasmInstance}

/** WASM module state */
let wasmInstance: ref<option<wasmInstance>> = ref(None)

/** Load WASM module from URL or path */
@val external fetch: string => promise<'a> = "fetch"

/** WebAssembly instantiate */
@scope("WebAssembly") @val
external instantiateStreaming: (promise<'a>, 'b) => promise<instantiateResult> =
  "instantiateStreaming"

/** Initialize the WASM module */
let init = async (wasmPath: string): result<unit, string> => {
  switch wasmInstance.contents {
  | Some(_) => Ok()
  | None =>
    try {
      let response = fetch(wasmPath)
      let importObject = %raw(`{}`)
      let result = await instantiateStreaming(response, importObject)
      wasmInstance := Some(result.instance)

      // Initialize the Proven runtime
      let initResult = result.instance.exports.init_(.)
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

/** Check if WASM module is initialized */
let isInitialized = (): bool => {
  switch wasmInstance.contents {
  | Some(instance) => instance.exports.isInitialized_(.)
  | None => false
  }
}

/** Cleanup the WASM module */
let deinit = (): unit => {
  switch wasmInstance.contents {
  | Some(instance) =>
    instance.exports.deinit_(.)
    wasmInstance := None
  | None => ()
  }
}

/** Get the current exports (for internal use) */
let getExports = (): option<wasmExports> => {
  switch wasmInstance.contents {
  | Some(instance) => Some(instance.exports)
  | None => None
  }
}

/** Get version string */
let version = (): option<string> => {
  switch getExports() {
  | Some(exports) =>
    let major = exports.versionMajor(.)
    let minor = exports.versionMinor(.)
    let patch = exports.versionPatch(.)
    Some(Belt.Int.toString(major) ++ "." ++ Belt.Int.toString(minor) ++ "." ++ Belt.Int.toString(patch))
  | None => None
  }
}
