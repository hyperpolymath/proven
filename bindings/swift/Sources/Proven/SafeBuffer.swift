// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Bounded buffer delegated to libproven FFI.
///
/// Buffer operations with bounds checking via the formally verified
/// Idris 2 core.

import CProven

/// Bounded buffer backed by libproven.
/// This class manages the lifecycle of a C-allocated bounded buffer.
public final class SafeBuffer {
    private let buffer: UnsafeMutablePointer<ProvenBoundedBuffer>

    /// Create a bounded buffer with the given capacity.
    /// Returns nil if allocation fails or capacity is invalid.
    public init?(capacity: Int) {
        let result = proven_buffer_create(capacity)
        if let error = ProvenError.fromStatus(result.status) {
            _ = error
            return nil
        }
        guard let ptr = result.buffer else {
            return nil
        }
        self.buffer = ptr
    }

    deinit {
        proven_buffer_free(buffer)
    }

    /// Append data to the buffer.
    public func append(_ data: [UInt8]) -> Result<Void, ProvenError> {
        let status = data.withUnsafeBufferPointer { buf in
            proven_buffer_append(buffer, buf.baseAddress, buf.count)
        }
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(())
    }

    /// Append a string's UTF-8 bytes to the buffer.
    public func append(_ string: String) -> Result<Void, ProvenError> {
        append(Array(string.utf8))
    }

    /// Get the current buffer contents as bytes.
    public func get() -> Result<[UInt8], ProvenError> {
        var outPtr: UnsafePointer<UInt8>? = nil
        var outLen: Int = 0
        let status = proven_buffer_get(buffer, &outPtr, &outLen)
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        guard let ptr = outPtr else {
            return .success([])
        }
        return .success(Array(UnsafeBufferPointer(start: ptr, count: outLen)))
    }
}
