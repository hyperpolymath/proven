// SPDX-License-Identifier: PMPL-1.0
//! Shared type definitions for Ephapax ↔ Proven FFI

const std = @import("std");

/// Opaque handle to Idris2 LRU cache
/// This is the SAME handle used for both affine and linear modes
/// The difference is how Ephapax's type checker treats it, not the underlying type
pub const LRUCacheHandle = opaque {};

/// Opaque handle to Idris2 SafeBuffer
pub const BufferHandle = opaque {};

/// Opaque handle to Idris2 ResourceHandle
pub const ResourceHandle = opaque {};

/// Result type for operations that can fail
pub const Result = enum(c_int) {
    Ok = 0,
    Error = 1,
};

/// Error information
pub const ErrorInfo = extern struct {
    code: c_int,
    message: [*:0]const u8,
};

/// Byte slice for FFI (compatible with Ephapax Bytes type)
pub const ByteSlice = extern struct {
    ptr: [*]const u8,
    len: usize,
};

/// Option type for FFI
pub fn OptionPtr(comptime T: type) type {
    return extern struct {
        has_value: bool,
        value: ?*T,
    };
}
