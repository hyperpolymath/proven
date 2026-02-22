// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// SafeCrypto -- Resource-typed wrappers around libproven cryptographic FFI.
///
/// All operations delegate to the formally verified Idris 2 implementation
/// via the Zig FFI layer. No crypto logic is reimplemented here.
/// Provides constant-time comparisons, CSPRNG, and hex encoding.

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Convert an FFI BoolResult into a Result[Bool, ProvenError].
fn bool_result_to_result(result: BoolResult) -> Result[Bool, ProvenError] {
    if result.status == STATUS_OK {
        Ok(result.value != 0)
    } else {
        Err(status_to_error(result.status))
    }
}

/// Convert an FFI StringResult into a Result[String, ProvenError].
fn string_result_to_result(result: StringResult) -> Result[String, ProvenError] {
    if result.status == STATUS_OK {
        let s = string_from_raw(result.ptr, result.len)
        proven_free_string(result.ptr)
        Ok(s)
    } else {
        Err(status_to_error(result.status))
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Compare two byte arrays in constant time (prevents timing attacks).
/// Delegates to proven_crypto_constant_time_eq via FFI.
fn constant_time_eq(a: Bytes, b: Bytes) -> Result[Bool, ProvenError] {
    let (ptr_a, len_a) = bytes_to_raw(a)
    let (ptr_b, len_b) = bytes_to_raw(b)
    bool_result_to_result(
        proven_crypto_constant_time_eq(ptr_a, len_a, ptr_b, len_b)
    )
}

/// Generate cryptographically secure random bytes.
/// Delegates to proven_crypto_random_bytes via FFI.
fn random_bytes(length: Int) -> Result[Bytes, ProvenError] {
    let buf = alloc_bytes(length)
    let (ptr, _) = bytes_to_raw_mut(buf)
    let status = proven_crypto_random_bytes(ptr, length)
    if status == STATUS_OK {
        Ok(buf)
    } else {
        free_bytes(buf)
        Err(status_to_error(status))
    }
}

/// Hex-encode a byte array to a string.
/// Delegates to proven_hex_encode via FFI.
fn hex_encode(data: Bytes, uppercase: Bool) -> Result[String, ProvenError] {
    let (ptr, len) = bytes_to_raw(data)
    string_result_to_result(proven_hex_encode(ptr, len, uppercase))
}

/// Hex-decode a string to a byte array.
/// Delegates to proven_hex_decode via FFI.
fn hex_decode(hex_string: String) -> Result[Bytes, ProvenError] {
    let (ptr, len) = string_to_raw(hex_string)
    let result = proven_hex_decode(ptr, len)
    if result.status == STATUS_OK {
        let bytes = bytes_from_raw(result.ptr, result.len)
        proven_free_string(result.ptr)
        Ok(bytes)
    } else {
        Err(status_to_error(result.status))
    }
}

/// Generate a random hex string of the specified byte length.
/// Returns a hex string that is 2x the byte length.
fn random_hex(byte_length: Int, uppercase: Bool) -> Result[String, ProvenError] {
    match random_bytes(byte_length) {
        Err(e) => Err(e),
        Ok(bytes) => hex_encode(bytes, uppercase)
    }
}
