// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Proven -- Code that cannot crash.
///
/// Eclexia bindings for the formally verified Proven safety library.
/// All computation is delegated to Idris 2 via the Zig FFI layer; these
/// bindings are thin resource-typed wrappers that leverage Eclexia's
/// economics-as-code paradigm for safe resource management.
///
/// Modules:
///   - SafeMath:     Arithmetic without overflow/underflow/division-by-zero.
///   - SafeString:   UTF-8 and escaping without exceptions.
///   - SafeCurrency: Currency operations with resource semantics.
///   - SafeCrypto:   Cryptographic primitives done right.
///   - SafeJson:     JSON parsing with safe validation.

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Error type returned by all Proven operations.
type ProvenError = { code: Int, message: String }

/// Convert a status code to a human-readable ProvenError.
fn status_to_error(code: Int) -> ProvenError {
    let msg = match code {
        -1  => "null pointer",
        -2  => "invalid argument",
        -3  => "overflow",
        -4  => "underflow",
        -5  => "division by zero",
        -6  => "parse failure",
        -7  => "validation failed",
        -8  => "out of bounds",
        -9  => "encoding error",
        -10 => "allocation failed",
        -99 => "not implemented",
        _   => "unknown error (code " + to_string(code) + ")"
    }
    ProvenError { code: code, message: msg }
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

/// Initialise the Proven runtime (including the Idris 2 runtime).
/// Must be called before any other Proven function.
fn init() -> Result[(), ProvenError] {
    let status = proven_init()
    if status == 0 {
        Ok(())
    } else {
        Err(status_to_error(status))
    }
}

/// Shut down the Proven runtime and release all resources.
fn deinit() {
    proven_deinit()
}

/// Check whether the runtime is initialised.
fn is_initialized() -> Bool {
    proven_is_initialized()
}

/// Library version.
const VERSION: String = "0.4.0"
