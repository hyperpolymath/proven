// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * FFI loader for libproven shared library.
 *
 * Loads libproven via Deno.dlopen (preferred) or falls back to Node.js
 * native addon / WASM module. This file declares all 103+ FFI function
 * signatures and provides the raw library handle.
 *
 * @module
 */

/**
 * Library search paths, ordered by precedence.
 * Relative paths are resolved from the binding directory.
 * @type {string[]}
 */
const LIBRARY_PATHS_UNIX = [
  new URL('../../ffi/zig/zig-out/lib/libproven.so', import.meta.url).pathname,
  new URL('../../ffi/zig/zig-out/lib/libproven.dylib', import.meta.url).pathname,
  '/usr/local/lib/libproven.so',
  '/usr/local/lib/libproven.dylib',
  '/usr/lib/libproven.so',
  '/usr/lib/libproven.dylib',
];

/**
 * Status code mapping from proven FFI.
 * @readonly
 * @enum {number}
 */
export const ProvenStatus = Object.freeze({
  OK: 0,
  ERR_NULL_POINTER: -1,
  ERR_INVALID_ARGUMENT: -2,
  ERR_OVERFLOW: -3,
  ERR_UNDERFLOW: -4,
  ERR_DIVISION_BY_ZERO: -5,
  ERR_PARSE_FAILURE: -6,
  ERR_VALIDATION_FAILED: -7,
  ERR_OUT_OF_BOUNDS: -8,
  ERR_ENCODING_ERROR: -9,
  ERR_ALLOCATION_FAILED: -10,
  ERR_NOT_IMPLEMENTED: -99,
});

/**
 * Map a proven status code to a human-readable error string.
 *
 * @param {number} status - The ProvenStatus integer.
 * @returns {string} A descriptive error message.
 */
export function statusToError(status) {
  switch (status) {
    case ProvenStatus.OK: return 'Ok';
    case ProvenStatus.ERR_NULL_POINTER: return 'Null pointer';
    case ProvenStatus.ERR_INVALID_ARGUMENT: return 'Invalid argument';
    case ProvenStatus.ERR_OVERFLOW: return 'Integer overflow';
    case ProvenStatus.ERR_UNDERFLOW: return 'Integer underflow';
    case ProvenStatus.ERR_DIVISION_BY_ZERO: return 'Division by zero';
    case ProvenStatus.ERR_PARSE_FAILURE: return 'Parse failure';
    case ProvenStatus.ERR_VALIDATION_FAILED: return 'Validation failed';
    case ProvenStatus.ERR_OUT_OF_BOUNDS: return 'Out of bounds';
    case ProvenStatus.ERR_ENCODING_ERROR: return 'Encoding error';
    case ProvenStatus.ERR_ALLOCATION_FAILED: return 'Allocation failed';
    case ProvenStatus.ERR_NOT_IMPLEMENTED: return 'Not implemented';
    default: return `Unknown error (${status})`;
  }
}

// ---------------------------------------------------------------------------
// Deno FFI symbol declarations
// ---------------------------------------------------------------------------

/**
 * Complete Deno FFI symbol table for libproven.
 * Each key is the C export name; value describes parameters and result type.
 */
const DENO_SYMBOLS = {
  // --- Runtime lifecycle ---
  proven_init: { parameters: [], result: 'i32' },
  proven_deinit: { parameters: [], result: 'void' },
  proven_is_initialized: { parameters: [], result: 'bool' },
  proven_ffi_abi_version: { parameters: [], result: 'u32' },
  proven_free_string: { parameters: ['pointer'], result: 'void' },

  // --- Version info ---
  proven_version_major: { parameters: [], result: 'u32' },
  proven_version_minor: { parameters: [], result: 'u32' },
  proven_version_patch: { parameters: [], result: 'u32' },
  proven_module_count: { parameters: [], result: 'u32' },

  // --- Callbacks ---
  proven_callback_register: { parameters: ['i32', 'pointer', 'pointer'], result: 'u32' },
  proven_callback_unregister: { parameters: ['u32'], result: 'i32' },
  proven_callback_fire: { parameters: ['i32', 'pointer', 'usize', 'i32'], result: 'i32' },
  proven_callback_count: { parameters: ['i32'], result: 'u32' },
  proven_callback_clear_all: { parameters: [], result: 'u32' },

  // --- SafeMath (8 functions) ---
  proven_math_div: { parameters: ['i64', 'i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_mod: { parameters: ['i64', 'i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_add_checked: { parameters: ['i64', 'i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_sub_checked: { parameters: ['i64', 'i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_mul_checked: { parameters: ['i64', 'i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_abs_safe: { parameters: ['i64'], result: { struct: ['i32', 'i64'] } },
  proven_math_clamp: { parameters: ['i64', 'i64', 'i64'], result: 'i64' },
  proven_math_pow_checked: { parameters: ['i64', 'u32'], result: { struct: ['i32', 'i64'] } },

  // --- SafeString (4 functions) ---
  proven_string_is_valid_utf8: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_string_escape_sql: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  proven_string_escape_html: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  proven_string_escape_js: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },

  // --- SafePath (2 functions) ---
  proven_path_has_traversal: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_path_sanitize_filename: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },

  // --- SafeCrypto (2 functions) ---
  proven_crypto_constant_time_eq: { parameters: ['pointer', 'usize', 'pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_crypto_random_bytes: { parameters: ['pointer', 'usize'], result: 'i32' },

  // --- SafeEmail (1 function) ---
  proven_email_is_valid: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },

  // --- SafeHeader (6 functions) ---
  proven_header_has_crlf: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_header_is_valid_name: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_header_is_dangerous: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_header_render: { parameters: ['pointer', 'usize', 'pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  proven_header_build_csp: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  proven_header_build_hsts: { parameters: ['i64', 'bool', 'bool'], result: { struct: ['i32', 'pointer', 'usize'] } },

  // --- SafeCookie (6 functions) ---
  proven_cookie_has_injection: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_cookie_validate_name: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_cookie_validate_value: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_cookie_get_prefix: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'i64'] } },
  // proven_cookie_build_set_cookie has complex struct param - use buffer-based calling
  proven_cookie_build_delete: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },

  // --- SafeContentType (5 functions) ---
  proven_content_type_can_sniff_dangerous: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_content_type_is_json: { parameters: ['pointer', 'usize', 'pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_content_type_is_xml: { parameters: ['pointer', 'usize', 'pointer', 'usize'], result: { struct: ['i32', 'bool'] } },

  // --- SafeJson (2 functions) ---
  proven_json_is_valid: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'bool'] } },
  proven_json_get_type: { parameters: ['pointer', 'usize'], result: 'i32' },

  // --- SafeUrl ---
  // proven_url_parse returns a complex struct; we use buffer
  // proven_url_free takes struct pointer

  // --- SafeNetwork (3 functions) ---
  // proven_network_parse_ipv4 returns struct with embedded array
  // proven_network_ipv4_is_private/is_loopback take struct param

  // --- SafeHex (2 functions) ---
  proven_hex_encode: { parameters: ['pointer', 'usize', 'bool'], result: { struct: ['i32', 'pointer', 'usize'] } },
  // proven_hex_decode returns HexDecodeResult

  // --- SafeUUID (5 functions) ---
  // These deal with UUID struct (16 bytes) - handled via buffer

  // --- SafeCurrency (2 functions) ---
  // proven_currency_parse returns CurrencyResult
  // proven_currency_format takes [3]u8 + u8

  // --- SafePhone (2 functions) ---
  // proven_phone_parse returns PhoneResult

  // --- SafeDateTime (4 functions) ---
  proven_datetime_is_leap_year: { parameters: ['i32'], result: 'bool' },
  proven_datetime_days_in_month: { parameters: ['i32', 'u8'], result: 'u8' },

  // --- SafeFloat (5 functions) ---
  proven_float_div: { parameters: ['f64', 'f64'], result: { struct: ['i32', 'f64'] } },
  proven_float_is_finite: { parameters: ['f64'], result: 'bool' },
  proven_float_is_nan: { parameters: ['f64'], result: 'bool' },
  proven_float_sqrt: { parameters: ['f64'], result: { struct: ['i32', 'f64'] } },
  proven_float_ln: { parameters: ['f64'], result: { struct: ['i32', 'f64'] } },

  // --- SafeAngle (4 functions) ---
  proven_angle_deg_to_rad: { parameters: ['f64'], result: 'f64' },
  proven_angle_rad_to_deg: { parameters: ['f64'], result: 'f64' },
  proven_angle_normalize_degrees: { parameters: ['f64'], result: 'f64' },
  proven_angle_normalize_radians: { parameters: ['f64'], result: 'f64' },

  // --- SafeProbability (4 functions) ---
  proven_probability_create: { parameters: ['f64'], result: 'f64' },
  proven_probability_and: { parameters: ['f64', 'f64'], result: 'f64' },
  proven_probability_or_exclusive: { parameters: ['f64', 'f64'], result: 'f64' },
  proven_probability_not: { parameters: ['f64'], result: 'f64' },

  // --- SafeML (5 functions) ---
  proven_ml_softmax: { parameters: ['pointer', 'pointer', 'usize'], result: 'i32' },
  proven_ml_sigmoid: { parameters: ['f64'], result: 'f64' },
  proven_ml_relu: { parameters: ['f64'], result: 'f64' },
  proven_ml_leaky_relu: { parameters: ['f64', 'f64'], result: 'f64' },
  proven_ml_clamp: { parameters: ['f64', 'f64', 'f64'], result: 'f64' },

  // --- SafeCalculator (1 function) ---
  proven_calculator_eval: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'f64'] } },

  // --- SafePassword (2 functions) ---
  // proven_password_validate returns PasswordResult struct
  proven_password_is_common: { parameters: ['pointer', 'usize'], result: 'bool' },

  // --- SafeChecksum (2 functions) ---
  proven_checksum_crc32: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'i64'] } },
  proven_checksum_verify_crc32: { parameters: ['pointer', 'usize', 'u32'], result: { struct: ['i32', 'bool'] } },

  // --- SafeGeo (3 functions) ---
  proven_geo_validate: { parameters: ['f64', 'f64'], result: { struct: ['i32', 'f64', 'f64'] } },
  // proven_geo_distance takes GeoCoordinate structs
  // proven_geo_in_bounds takes struct + 4 floats

  // --- SafeVersion (2 functions) ---
  // proven_version_parse returns VersionResult (complex struct)
  // proven_version_compare takes two SemanticVersion structs

  // --- SafeUnit (2 functions) ---
  proven_unit_convert_length: { parameters: ['f64', 'i32', 'i32'], result: { struct: ['i32', 'f64'] } },
  proven_unit_convert_temp: { parameters: ['f64', 'i32', 'i32'], result: { struct: ['i32', 'f64'] } },

  // --- SafeBuffer (4 functions) ---
  proven_buffer_create: { parameters: ['usize'], result: { struct: ['i32', 'pointer'] } },
  proven_buffer_append: { parameters: ['pointer', 'pointer', 'usize'], result: 'i32' },
  proven_buffer_get: { parameters: ['pointer', 'pointer', 'pointer'], result: 'i32' },
  proven_buffer_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeRateLimiter (3 functions) ---
  proven_rate_limiter_create: { parameters: ['f64', 'f64'], result: 'pointer' },
  proven_rate_limiter_try_acquire: { parameters: ['pointer', 'f64'], result: 'bool' },
  proven_rate_limiter_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeCircuitBreaker (6 functions) ---
  proven_circuit_breaker_create: { parameters: ['u32', 'u32', 'i64'], result: 'pointer' },
  proven_circuit_breaker_allow: { parameters: ['pointer'], result: 'bool' },
  proven_circuit_breaker_success: { parameters: ['pointer'], result: 'void' },
  proven_circuit_breaker_failure: { parameters: ['pointer'], result: 'void' },
  proven_circuit_breaker_state: { parameters: ['pointer'], result: 'i32' },
  proven_circuit_breaker_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeQueue (5 functions) ---
  proven_queue_create: { parameters: ['usize'], result: 'pointer' },
  proven_queue_push: { parameters: ['pointer', 'i64'], result: 'bool' },
  proven_queue_pop: { parameters: ['pointer'], result: { struct: ['i32', 'i64'] } },
  proven_queue_size: { parameters: ['pointer'], result: 'usize' },
  proven_queue_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeBloom (4 functions) ---
  proven_bloom_create: { parameters: ['usize', 'f64'], result: 'pointer' },
  proven_bloom_add: { parameters: ['pointer', 'pointer', 'usize'], result: 'void' },
  proven_bloom_contains: { parameters: ['pointer', 'pointer', 'usize'], result: 'bool' },
  proven_bloom_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeRetry (2 functions) ---
  // These take RetryConfig struct - complex parameter
  // Handled via buffer-based calling

  // --- SafeMonotonic (3 functions) ---
  proven_monotonic_create: { parameters: ['u64', 'u64'], result: 'pointer' },
  proven_monotonic_next: { parameters: ['pointer'], result: { struct: ['i32', 'i64'] } },
  proven_monotonic_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeStateMachine (5 functions) ---
  proven_state_machine_create: { parameters: ['u32', 'u32'], result: 'pointer' },
  proven_state_machine_allow: { parameters: ['pointer', 'u32', 'u32'], result: 'bool' },
  proven_state_machine_transition: { parameters: ['pointer', 'u32'], result: 'bool' },
  proven_state_machine_state: { parameters: ['pointer'], result: 'u32' },
  proven_state_machine_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeTensor (5 functions) ---
  proven_tensor_create: { parameters: ['usize', 'usize'], result: 'pointer' },
  proven_tensor_set: { parameters: ['pointer', 'usize', 'usize', 'f64'], result: 'i32' },
  proven_tensor_get: { parameters: ['pointer', 'usize', 'usize'], result: { struct: ['i32', 'f64'] } },
  proven_tensor_matmul: { parameters: ['pointer', 'pointer'], result: 'pointer' },
  proven_tensor_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeLRU (4 functions) ---
  proven_lru_create: { parameters: ['usize'], result: 'pointer' },
  proven_lru_get: { parameters: ['pointer', 'u64'], result: { struct: ['i32', 'i64'] } },
  proven_lru_put: { parameters: ['pointer', 'u64', 'i64'], result: 'i32' },
  proven_lru_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeGraph (4 functions) ---
  proven_graph_create: { parameters: ['usize'], result: 'pointer' },
  proven_graph_add_edge: { parameters: ['pointer', 'usize', 'usize'], result: 'i32' },
  proven_graph_has_edge: { parameters: ['pointer', 'usize', 'usize'], result: 'bool' },
  proven_graph_free: { parameters: ['pointer'], result: 'void' },

  // --- SafeColor (3 functions) ---
  // proven_color_parse_hex returns ColorParseResult (status + 3 u8s)
  // proven_color_rgb_to_hsl takes RGBColor struct
  // proven_color_to_hex takes RGBColor struct

  // --- SafeHTTP (3 functions) ---
  proven_http_url_encode: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  proven_http_url_decode: { parameters: ['pointer', 'usize'], result: { struct: ['i32', 'pointer', 'usize'] } },
  // proven_http_parse_www_authenticate returns complex struct
};

// ---------------------------------------------------------------------------
// Library handle
// ---------------------------------------------------------------------------

/** @type {object|null} Loaded library handle */
let lib = null;

/** @type {boolean} Whether initialization has been attempted */
let initAttempted = false;

/** @type {string|null} Error message if loading failed */
let loadError = null;

/**
 * Encode a JavaScript string to a null-terminated UTF-8 Uint8Array.
 *
 * @param {string} str - The string to encode.
 * @returns {Uint8Array} Null-terminated UTF-8 bytes.
 */
export function encodeString(str) {
  const encoder = new TextEncoder();
  const encoded = encoder.encode(str);
  const withNull = new Uint8Array(encoded.length + 1);
  withNull.set(encoded);
  withNull[encoded.length] = 0;
  return withNull;
}

/**
 * Read a null-terminated C string from a Deno pointer.
 * Frees the string via proven_free_string after reading.
 *
 * @param {Deno.PointerObject|null} ptr - Pointer to the C string.
 * @param {number} length - Known length of the string.
 * @returns {string|null} The decoded string, or null if pointer is null.
 */
export function readAndFreeString(ptr, length) {
  if (!ptr) return null;

  const view = new Deno.UnsafePointerView(ptr);
  const bytes = new Uint8Array(length);
  view.copyInto(bytes);
  const decoded = new TextDecoder().decode(bytes);

  // Free the C-allocated string
  lib.symbols.proven_free_string(ptr);

  return decoded;
}

/**
 * Read a null-terminated C string from a Deno pointer without freeing it.
 *
 * @param {Deno.PointerObject|null} ptr - Pointer to the C string.
 * @param {number} length - Known length of the string.
 * @returns {string|null} The decoded string, or null if pointer is null.
 */
export function readString(ptr, length) {
  if (!ptr) return null;
  const view = new Deno.UnsafePointerView(ptr);
  const bytes = new Uint8Array(length);
  view.copyInto(bytes);
  return new TextDecoder().decode(bytes);
}

/**
 * Try to load libproven. Called lazily on first access.
 * Supports Deno.dlopen (Deno runtime).
 *
 * @returns {boolean} True if loading succeeded.
 */
function tryLoad() {
  if (initAttempted) return lib !== null;
  initAttempted = true;

  // Deno runtime
  if (typeof Deno !== 'undefined' && typeof Deno.dlopen === 'function') {
    // Try custom path from environment
    const customPath = Deno.env.get('PROVEN_LIB_PATH');
    const paths = customPath ? [customPath, ...LIBRARY_PATHS_UNIX] : LIBRARY_PATHS_UNIX;

    for (const path of paths) {
      try {
        lib = Deno.dlopen(path, DENO_SYMBOLS);
        // Initialize the runtime
        const initStatus = lib.symbols.proven_init();
        if (initStatus !== 0) {
          loadError = `proven_init() returned status ${initStatus}`;
          lib.close();
          lib = null;
          continue;
        }
        return true;
      } catch (_e) {
        continue;
      }
    }

    loadError = 'Failed to load libproven from any known path. Set PROVEN_LIB_PATH env var.';
    return false;
  }

  loadError = 'No supported FFI runtime detected (need Deno with --allow-ffi)';
  return false;
}

/**
 * Get the loaded library symbols.
 * Triggers lazy loading if not yet attempted.
 *
 * @returns {object} The library symbols object.
 * @throws {Error} If the library could not be loaded.
 */
export function getLib() {
  if (!initAttempted) tryLoad();
  if (!lib) {
    throw new Error(`libproven not loaded: ${loadError}`);
  }
  return lib.symbols;
}

/**
 * Check if the FFI library is available without throwing.
 *
 * @returns {boolean} True if libproven is loaded and initialized.
 */
export function isAvailable() {
  if (!initAttempted) tryLoad();
  return lib !== null;
}

/**
 * Get the load error message, if any.
 *
 * @returns {string|null} Error message or null if loaded successfully.
 */
export function getLoadError() {
  if (!initAttempted) tryLoad();
  return loadError;
}

/**
 * Explicitly close the library handle.
 * Call this at shutdown to cleanly release resources.
 */
export function close() {
  if (lib) {
    try {
      lib.symbols.proven_deinit();
    } catch (_e) {
      // Ignore deinit errors during shutdown
    }
    lib.close();
    lib = null;
    initAttempted = false;
  }
}
