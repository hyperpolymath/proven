// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven SafeTensor - FFI bindings to libproven tensor operations.
// All computation is performed in verified Idris 2 code via libproven.

const std = @import("std");
const c = @cImport(@cInclude("proven.h"));

/// Error types for tensor operations.
pub const TensorError = error{
    CreationFailed,
    OutOfBounds,
    DimensionMismatch,
    ProvenError,
};

/// 2D tensor (matrix) backed by libproven.
pub const Tensor2D = struct {
    ptr: *c.ProvenTensor2D,

    /// Set tensor value at (row, col) via libproven.
    pub fn set(self: Tensor2D, row: usize, col: usize, value: f64) TensorError!void {
        const status = c.proven_tensor_set(self.ptr, row, col, value);
        return switch (status) {
            c.PROVEN_OK => {},
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.OutOfBounds,
            else => error.ProvenError,
        };
    }

    /// Get tensor value at (row, col) via libproven.
    pub fn get(self: Tensor2D, row: usize, col: usize) TensorError!f64 {
        const result = c.proven_tensor_get(self.ptr, row, col);
        return switch (result.status) {
            c.PROVEN_OK => result.value,
            c.PROVEN_ERR_OUT_OF_BOUNDS => error.OutOfBounds,
            else => error.ProvenError,
        };
    }

    /// Matrix multiplication (self * other) via libproven.
    /// Returns a new tensor, or error if dimensions mismatch.
    pub fn matmul(self: Tensor2D, other: Tensor2D) TensorError!Tensor2D {
        const ptr = c.proven_tensor_matmul(self.ptr, other.ptr);
        if (ptr == null) return error.DimensionMismatch;
        return Tensor2D{ .ptr = ptr.? };
    }

    /// Free tensor via libproven.
    pub fn deinit(self: Tensor2D) void {
        c.proven_tensor_free(self.ptr);
    }
};

/// Create a 2D tensor initialized to zero via libproven.
pub fn create(rows: usize, cols: usize) TensorError!Tensor2D {
    const ptr = c.proven_tensor_create(rows, cols);
    if (ptr == null) return error.CreationFailed;
    return Tensor2D{ .ptr = ptr.? };
}

test "create and set/get" {
    const t = try create(3, 3);
    defer t.deinit();
    try t.set(0, 0, 1.0);
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), try t.get(0, 0), 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.0), try t.get(1, 1), 0.001);
}
