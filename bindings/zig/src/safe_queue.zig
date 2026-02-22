// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeQueue - FFI bindings to libproven bounded queue operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for queue operations.
pub const QueueError = error{
    CreationFailed,
    QueueFull,
    QueueEmpty,
    ProvenError,
};

/// Bounded FIFO queue backed by libproven.
pub const BoundedQueue = struct {
    ptr: *c.ProvenBoundedQueue,

    /// Push value to queue via libproven.
    pub fn push(self: BoundedQueue, value: i64) QueueError!void {
        if (!c.proven_queue_push(self.ptr, value)) return error.QueueFull;
    }

    /// Pop value from queue (FIFO order) via libproven.
    pub fn pop(self: BoundedQueue) QueueError!i64 {
        const result = c.proven_queue_pop(self.ptr);
        return switch (result.status) {
            c.PROVEN_OK => result.value,
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.QueueEmpty,
            else => error.ProvenError,
        };
    }

    /// Get queue element count via libproven.
    pub fn size(self: BoundedQueue) usize {
        return c.proven_queue_size(self.ptr);
    }

    /// Free queue via libproven.
    pub fn deinit(self: BoundedQueue) void {
        c.proven_queue_free(self.ptr);
    }
};

/// Create a bounded queue via libproven.
/// capacity: Maximum elements (max 1,000,000).
pub fn create(capacity: usize) QueueError!BoundedQueue {
    const ptr = c.proven_queue_create(capacity);
    if (ptr == null) return error.CreationFailed;
    return BoundedQueue{ .ptr = ptr.? };
}

test "create and push/pop" {
    const q = try create(10);
    defer q.deinit();
    try q.push(42);
    try q.push(99);
    try std.testing.expectEqual(@as(usize, 2), q.size());
    try std.testing.expectEqual(@as(i64, 42), try q.pop());
    try std.testing.expectEqual(@as(i64, 99), try q.pop());
    try std.testing.expectError(error.QueueEmpty, q.pop());
}
