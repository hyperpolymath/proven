// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Proven - Thin FFI wrapper around libproven shared library.
///
/// All computation is delegated to the Idris 2 formally verified core
/// via the Zig FFI bridge (libproven). This Swift package provides
/// safe, idiomatic Swift wrappers using Result types.
///
/// Architecture:
///   Swift (this package) -> CProven (C interop) -> libproven.so -> Zig FFI -> Idris 2

import CProven

// MARK: - Error Type

/// Unified error type mapping libproven status codes to Swift.
public enum ProvenError: Error, Equatable, Sendable {
    case nullPointer
    case invalidArgument
    case overflow
    case underflow
    case divisionByZero
    case parseFailed
    case validationFailed
    case outOfBounds
    case encodingError
    case allocationFailed
    case notImplemented
    case unknown(Int32)

    /// Convert a ProvenStatus integer to a ProvenError.
    /// Returns nil for PROVEN_OK (status == 0).
    public static func fromStatus(_ status: ProvenStatus) -> ProvenError? {
        switch status {
        case PROVEN_OK:
            return nil
        case PROVEN_ERR_NULL_POINTER:
            return .nullPointer
        case PROVEN_ERR_INVALID_ARGUMENT:
            return .invalidArgument
        case PROVEN_ERR_OVERFLOW:
            return .overflow
        case PROVEN_ERR_UNDERFLOW:
            return .underflow
        case PROVEN_ERR_DIVISION_BY_ZERO:
            return .divisionByZero
        case PROVEN_ERR_PARSE_FAILURE:
            return .parseFailed
        case PROVEN_ERR_VALIDATION_FAILED:
            return .validationFailed
        case PROVEN_ERR_OUT_OF_BOUNDS:
            return .outOfBounds
        case PROVEN_ERR_ENCODING_ERROR:
            return .encodingError
        case PROVEN_ERR_ALLOCATION_FAILED:
            return .allocationFailed
        case PROVEN_ERR_NOT_IMPLEMENTED:
            return .notImplemented
        default:
            return .unknown(status.rawValue)
        }
    }

    /// Convert from raw Int32 status code.
    public static func fromRawStatus(_ raw: Int32) -> ProvenError? {
        if raw == 0 { return nil }
        return fromStatus(ProvenStatus(rawValue: raw) ?? PROVEN_ERR_NOT_IMPLEMENTED)
    }
}

// MARK: - Runtime

/// Proven runtime management.
public enum ProvenRuntime {
    /// Initialize the Proven runtime. Call before using any other function.
    @discardableResult
    public static func initialize() -> Result<Void, ProvenError> {
        let status = proven_init()
        if status == 0 {
            return .success(())
        }
        return .failure(ProvenError.fromRawStatus(status) ?? .unknown(status))
    }

    /// Deinitialize the Proven runtime. Call when done.
    public static func deinitialize() {
        proven_deinit()
    }

    /// Check if the runtime is initialized.
    public static var isInitialized: Bool {
        proven_is_initialized()
    }

    /// Get the FFI ABI version number.
    public static var abiVersion: UInt32 {
        proven_ffi_abi_version()
    }

    /// Get the library version as (major, minor, patch).
    public static var version: (major: UInt32, minor: UInt32, patch: UInt32) {
        (proven_version_major(), proven_version_minor(), proven_version_patch())
    }

    /// Get the module count.
    public static var moduleCount: UInt32 {
        proven_module_count()
    }
}

// MARK: - Internal Helpers

/// Convert a Swift String to a call with a (ptr, len) pair, returning Result.
/// This helper calls a C function that returns ProvenIntResult.
internal func withStringBytes<R>(
    _ string: String,
    _ body: (UnsafePointer<UInt8>, Int) -> R
) -> R {
    let utf8 = Array(string.utf8)
    return utf8.withUnsafeBufferPointer { buffer in
        body(buffer.baseAddress ?? UnsafePointer<UInt8>(bitPattern: 1)!, buffer.count)
    }
}

/// Convert a ProvenStringResult to a Swift String, freeing the C memory.
/// Returns nil if status is not OK or value is NULL.
internal func consumeStringResult(_ result: ProvenStringResult) -> Result<String, ProvenError> {
    if let error = ProvenError.fromStatus(result.status) {
        return .failure(error)
    }
    guard let ptr = result.value else {
        return .failure(.nullPointer)
    }
    let string = String(cString: ptr)
    proven_free_string(ptr)
    return .success(string)
}
