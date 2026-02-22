// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeBuffer - FFI bindings to libproven bounded buffer operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for buffer operations.
pub const BufferError = error{
    CreationFailed,
    OutOfBounds,
    ProvenError,
};

/// Bounded buffer backed by libproven.
pub const BoundedBuffer = struct {
    ptr: *c.ProvenBoundedBuffer,

    /// Append data to buffer with bounds checking via libproven.
    pub fn append(self: BoundedBuffer, data: []const u8) BufferError!void {
        const status = c.proven_buffer_append(self.ptr, data.ptr, data.len);
        return switch (status) {
            c.PROVEN_OK => {},
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.OutOfBounds,
            else => error.ProvenError,
        };
    }

    /// Get buffer contents via libproven.
    pub fn get(self: BoundedBuffer) BufferError![]const u8 {
        var out_ptr: [*]const u8 = undefined;
        var out_len: usize = undefined;
        const status = c.proven_buffer_get(self.ptr, &out_ptr, &out_len);
        if (status != c.PROVEN_OK) return error.ProvenError;
        return out_ptr[0..out_len];
    }

    /// Free buffer via libproven.
    pub fn deinit(self: BoundedBuffer) void {
        c.proven_buffer_free(self.ptr);
    }
};

/// Create a bounded buffer via libproven.
/// capacity: Maximum capacity in bytes (max 100MB).
pub fn create(capacity: usize) BufferError!BoundedBuffer {
    const result = c.proven_buffer_create(capacity);
    if (result.status != c.PROVEN_OK or result.buffer == null) return error.CreationFailed;
    return BoundedBuffer{ .ptr = result.buffer.? };
}

test "create and append" {
    const buf = try create(1024);
    defer buf.deinit();
    try buf.append("hello");
    const data = try buf.get();
    try std.testing.expectEqualStrings("hello", data);
}
