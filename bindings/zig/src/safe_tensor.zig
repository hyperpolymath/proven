// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe tensor operations that cannot crash.

const std = @import("std");

/// Error types for tensor operations.
pub const TensorError = error{
    DimensionMismatch,
    InvalidShape,
    IndexOutOfBounds,
    OutOfMemory,
    InvalidOperation,
};

/// A 1D tensor (vector)
pub fn Vector(comptime T: type) type {
    return struct {
        data: []T,
        allocator: std.mem.Allocator,

        const Self = @This();

        /// Create a new vector with given size
        pub fn init(allocator: std.mem.Allocator, size: usize) !Self {
            const data = try allocator.alloc(T, size);
            @memset(data, 0);
            return Self{ .data = data, .allocator = allocator };
        }

        /// Create from slice
        pub fn fromSlice(allocator: std.mem.Allocator, slice: []const T) !Self {
            const data = try allocator.dupe(T, slice);
            return Self{ .data = data, .allocator = allocator };
        }

        /// Free the vector
        pub fn deinit(self: Self) void {
            self.allocator.free(self.data);
        }

        /// Get length
        pub fn len(self: Self) usize {
            return self.data.len;
        }

        /// Get element at index
        pub fn get(self: Self, index: usize) TensorError!T {
            if (index >= self.data.len) return error.IndexOutOfBounds;
            return self.data[index];
        }

        /// Set element at index
        pub fn set(self: *Self, index: usize, value: T) TensorError!void {
            if (index >= self.data.len) return error.IndexOutOfBounds;
            self.data[index] = value;
        }

        /// Dot product
        pub fn dot(self: Self, other: Self) TensorError!T {
            if (self.data.len != other.data.len) return error.DimensionMismatch;
            var sum: T = 0;
            for (self.data, other.data) |a, b| {
                sum += a * b;
            }
            return sum;
        }

        /// Add two vectors
        pub fn add(self: Self, other: Self) TensorError!Self {
            if (self.data.len != other.data.len) return error.DimensionMismatch;
            var result = try Self.init(self.allocator, self.data.len);
            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a + b;
            }
            return result;
        }

        /// Subtract two vectors
        pub fn sub(self: Self, other: Self) TensorError!Self {
            if (self.data.len != other.data.len) return error.DimensionMismatch;
            var result = try Self.init(self.allocator, self.data.len);
            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a - b;
            }
            return result;
        }

        /// Scale by scalar
        pub fn scale(self: Self, scalar: T) !Self {
            var result = try Self.init(self.allocator, self.data.len);
            for (self.data, 0..) |v, i| {
                result.data[i] = v * scalar;
            }
            return result;
        }

        /// L2 norm
        pub fn norm(self: Self) f64 {
            var sum: f64 = 0;
            for (self.data) |v| {
                const fv: f64 = @floatFromInt(v);
                sum += fv * fv;
            }
            return @sqrt(sum);
        }
    };
}

/// A 2D tensor (matrix)
pub fn Matrix(comptime T: type) type {
    return struct {
        data: []T,
        rows: usize,
        cols: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        /// Create a new matrix
        pub fn init(allocator: std.mem.Allocator, rows: usize, cols: usize) !Self {
            const data = try allocator.alloc(T, rows * cols);
            @memset(data, 0);
            return Self{
                .data = data,
                .rows = rows,
                .cols = cols,
                .allocator = allocator,
            };
        }

        /// Create identity matrix
        pub fn identity(allocator: std.mem.Allocator, size: usize) !Self {
            var m = try Self.init(allocator, size, size);
            for (0..size) |i| {
                m.data[i * size + i] = 1;
            }
            return m;
        }

        /// Free the matrix
        pub fn deinit(self: Self) void {
            self.allocator.free(self.data);
        }

        /// Get element at (row, col)
        pub fn get(self: Self, row: usize, col: usize) TensorError!T {
            if (row >= self.rows or col >= self.cols) return error.IndexOutOfBounds;
            return self.data[row * self.cols + col];
        }

        /// Set element at (row, col)
        pub fn set(self: *Self, row: usize, col: usize, value: T) TensorError!void {
            if (row >= self.rows or col >= self.cols) return error.IndexOutOfBounds;
            self.data[row * self.cols + col] = value;
        }

        /// Get shape as (rows, cols)
        pub fn shape(self: Self) struct { usize, usize } {
            return .{ self.rows, self.cols };
        }

        /// Add two matrices
        pub fn add(self: Self, other: Self) TensorError!Self {
            if (self.rows != other.rows or self.cols != other.cols) {
                return error.DimensionMismatch;
            }
            var result = try Self.init(self.allocator, self.rows, self.cols);
            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a + b;
            }
            return result;
        }

        /// Multiply two matrices
        pub fn mul(self: Self, other: Self) TensorError!Self {
            if (self.cols != other.rows) return error.DimensionMismatch;

            var result = try Self.init(self.allocator, self.rows, other.cols);

            for (0..self.rows) |i| {
                for (0..other.cols) |j| {
                    var sum: T = 0;
                    for (0..self.cols) |k| {
                        sum += self.data[i * self.cols + k] * other.data[k * other.cols + j];
                    }
                    result.data[i * other.cols + j] = sum;
                }
            }

            return result;
        }

        /// Transpose
        pub fn transpose(self: Self) !Self {
            var result = try Self.init(self.allocator, self.cols, self.rows);
            for (0..self.rows) |i| {
                for (0..self.cols) |j| {
                    result.data[j * self.rows + i] = self.data[i * self.cols + j];
                }
            }
            return result;
        }

        /// Scale by scalar
        pub fn scale(self: Self, scalar: T) !Self {
            var result = try Self.init(self.allocator, self.rows, self.cols);
            for (self.data, 0..) |v, i| {
                result.data[i] = v * scalar;
            }
            return result;
        }

        /// Trace (sum of diagonal)
        pub fn trace(self: Self) TensorError!T {
            if (self.rows != self.cols) return error.InvalidOperation;
            var sum: T = 0;
            for (0..self.rows) |i| {
                sum += self.data[i * self.cols + i];
            }
            return sum;
        }

        /// Frobenius norm squared
        pub fn frobeniusNormSq(self: Self) T {
            var sum: T = 0;
            for (self.data) |v| {
                sum += v * v;
            }
            return sum;
        }
    };
}

/// Create a zero vector
pub fn zeros(comptime T: type, allocator: std.mem.Allocator, size: usize) !Vector(T) {
    return Vector(T).init(allocator, size);
}

/// Create a ones vector
pub fn ones(comptime T: type, allocator: std.mem.Allocator, size: usize) !Vector(T) {
    var v = try Vector(T).init(allocator, size);
    @memset(v.data, 1);
    return v;
}

/// Create a range vector [start, start+1, ..., start+size-1]
pub fn range(comptime T: type, allocator: std.mem.Allocator, start: T, size: usize) !Vector(T) {
    var v = try Vector(T).init(allocator, size);
    for (0..size) |i| {
        v.data[i] = start + @as(T, @intCast(i));
    }
    return v;
}

test "Vector operations" {
    const allocator = std.testing.allocator;

    var v1 = try Vector(i64).fromSlice(allocator, &[_]i64{ 1, 2, 3 });
    defer v1.deinit();

    var v2 = try Vector(i64).fromSlice(allocator, &[_]i64{ 4, 5, 6 });
    defer v2.deinit();

    // Dot product
    const dot = try v1.dot(v2);
    try std.testing.expectEqual(@as(i64, 32), dot); // 1*4 + 2*5 + 3*6

    // Add
    var sum = try v1.add(v2);
    defer sum.deinit();
    try std.testing.expectEqual(@as(i64, 5), sum.data[0]);
    try std.testing.expectEqual(@as(i64, 7), sum.data[1]);
    try std.testing.expectEqual(@as(i64, 9), sum.data[2]);
}

test "Matrix operations" {
    const allocator = std.testing.allocator;

    var m = try Matrix(i64).identity(allocator, 3);
    defer m.deinit();

    // Check identity
    try std.testing.expectEqual(@as(i64, 1), try m.get(0, 0));
    try std.testing.expectEqual(@as(i64, 0), try m.get(0, 1));
    try std.testing.expectEqual(@as(i64, 1), try m.get(1, 1));

    // Trace
    const tr = try m.trace();
    try std.testing.expectEqual(@as(i64, 3), tr);
}
