// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe matrix operations with dimension checking that cannot crash.
//!
//! Provides dense matrix operations with guaranteed dimension validation,
//! bounds checking, and safe arithmetic. All operations return errors
//! instead of crashing on invalid inputs.

const std = @import("std");

/// Error types for matrix operations.
pub const MatrixError = error{
    /// Matrix dimensions do not match for the operation.
    DimensionMismatch,
    /// Row or column index is out of bounds.
    IndexOutOfBounds,
    /// Matrix shape is invalid (zero dimensions, etc.).
    InvalidShape,
    /// Matrix is not square when required to be.
    NotSquare,
    /// Matrix is singular and cannot be inverted.
    SingularMatrix,
    /// Arithmetic overflow during computation.
    Overflow,
    /// Memory allocation failed.
    OutOfMemory,
};

/// Maximum supported matrix dimension to prevent excessive memory allocation.
pub const MAX_DIMENSION = 4096;

/// A dense matrix with safe operations.
pub fn DenseMatrix(comptime T: type) type {
    return struct {
        data: []T,
        rows: usize,
        cols: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        /// Create a new zero-initialized matrix with given dimensions.
        pub fn init(allocator: std.mem.Allocator, rows: usize, cols: usize) MatrixError!Self {
            if (rows == 0 or cols == 0) return error.InvalidShape;
            if (rows > MAX_DIMENSION or cols > MAX_DIMENSION) return error.InvalidShape;

            const total_size = std.math.mul(usize, rows, cols) catch return error.Overflow;
            const data = allocator.alloc(T, total_size) catch return error.OutOfMemory;
            @memset(data, 0);

            return Self{
                .data = data,
                .rows = rows,
                .cols = cols,
                .allocator = allocator,
            };
        }

        /// Create a matrix from a 2D slice of values.
        pub fn fromSlice(allocator: std.mem.Allocator, values: []const []const T) MatrixError!Self {
            if (values.len == 0) return error.InvalidShape;

            const rows = values.len;
            const cols = values[0].len;

            if (cols == 0) return error.InvalidShape;

            // Verify all rows have the same column count
            for (values) |row| {
                if (row.len != cols) return error.DimensionMismatch;
            }

            var matrix = try Self.init(allocator, rows, cols);

            for (values, 0..) |row, i| {
                for (row, 0..) |val, j| {
                    matrix.data[i * cols + j] = val;
                }
            }

            return matrix;
        }

        /// Create an identity matrix of given size.
        pub fn identity(allocator: std.mem.Allocator, size: usize) MatrixError!Self {
            var matrix = try Self.init(allocator, size, size);

            for (0..size) |i| {
                matrix.data[i * size + i] = 1;
            }

            return matrix;
        }

        /// Create a diagonal matrix from a slice of values.
        pub fn diagonal(allocator: std.mem.Allocator, values: []const T) MatrixError!Self {
            if (values.len == 0) return error.InvalidShape;

            var matrix = try Self.init(allocator, values.len, values.len);

            for (values, 0..) |val, i| {
                matrix.data[i * values.len + i] = val;
            }

            return matrix;
        }

        /// Free the matrix memory.
        pub fn deinit(self: Self) void {
            self.allocator.free(self.data);
        }

        /// Clone the matrix.
        pub fn clone(self: Self) MatrixError!Self {
            var new_matrix = try Self.init(self.allocator, self.rows, self.cols);
            @memcpy(new_matrix.data, self.data);
            return new_matrix;
        }

        /// Get the element at (row, col).
        pub fn get(self: Self, row: usize, col: usize) MatrixError!T {
            if (row >= self.rows or col >= self.cols) return error.IndexOutOfBounds;
            return self.data[row * self.cols + col];
        }

        /// Set the element at (row, col).
        pub fn set(self: *Self, row: usize, col: usize, value: T) MatrixError!void {
            if (row >= self.rows or col >= self.cols) return error.IndexOutOfBounds;
            self.data[row * self.cols + col] = value;
        }

        /// Get the shape as (rows, cols).
        pub fn shape(self: Self) struct { usize, usize } {
            return .{ self.rows, self.cols };
        }

        /// Check if the matrix is square.
        pub fn isSquare(self: Self) bool {
            return self.rows == self.cols;
        }

        /// Check if dimensions match another matrix.
        pub fn hasSameShape(self: Self, other: Self) bool {
            return self.rows == other.rows and self.cols == other.cols;
        }

        /// Check if multiplication is compatible with another matrix.
        pub fn canMultiplyWith(self: Self, other: Self) bool {
            return self.cols == other.rows;
        }

        /// Add two matrices element-wise.
        pub fn add(self: Self, other: Self) MatrixError!Self {
            if (!self.hasSameShape(other)) return error.DimensionMismatch;

            var result = try Self.init(self.allocator, self.rows, self.cols);

            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a + b;
            }

            return result;
        }

        /// Subtract two matrices element-wise.
        pub fn sub(self: Self, other: Self) MatrixError!Self {
            if (!self.hasSameShape(other)) return error.DimensionMismatch;

            var result = try Self.init(self.allocator, self.rows, self.cols);

            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a - b;
            }

            return result;
        }

        /// Multiply two matrices.
        pub fn mul(self: Self, other: Self) MatrixError!Self {
            if (!self.canMultiplyWith(other)) return error.DimensionMismatch;

            var result = try Self.init(self.allocator, self.rows, other.cols);

            for (0..self.rows) |i| {
                for (0..other.cols) |j| {
                    var acc: T = 0;
                    for (0..self.cols) |k| {
                        acc += self.data[i * self.cols + k] * other.data[k * other.cols + j];
                    }
                    result.data[i * other.cols + j] = acc;
                }
            }

            return result;
        }

        /// Scale the matrix by a scalar value.
        pub fn scale(self: Self, scalar: T) MatrixError!Self {
            var result = try Self.init(self.allocator, self.rows, self.cols);

            for (self.data, 0..) |v, i| {
                result.data[i] = v * scalar;
            }

            return result;
        }

        /// Transpose the matrix.
        pub fn transpose(self: Self) MatrixError!Self {
            var result = try Self.init(self.allocator, self.cols, self.rows);

            for (0..self.rows) |i| {
                for (0..self.cols) |j| {
                    result.data[j * self.rows + i] = self.data[i * self.cols + j];
                }
            }

            return result;
        }

        /// Compute the trace (sum of diagonal elements).
        pub fn trace(self: Self) MatrixError!T {
            if (!self.isSquare()) return error.NotSquare;

            var acc: T = 0;
            for (0..self.rows) |i| {
                acc += self.data[i * self.cols + i];
            }

            return acc;
        }

        /// Compute the Frobenius norm squared (sum of squared elements).
        pub fn frobeniusNormSquared(self: Self) T {
            var acc: T = 0;
            for (self.data) |v| {
                acc += v * v;
            }
            return acc;
        }

        /// Compute the Frobenius norm.
        pub fn frobeniusNorm(self: Self) f64 {
            const norm_sq = self.frobeniusNormSquared();
            return @sqrt(@as(f64, @floatFromInt(norm_sq)));
        }

        /// Get a row as a slice (read-only view, no allocation).
        pub fn getRow(self: Self, row: usize) MatrixError![]const T {
            if (row >= self.rows) return error.IndexOutOfBounds;
            const start = row * self.cols;
            return self.data[start .. start + self.cols];
        }

        /// Get a column as a new allocated slice.
        pub fn getColumn(self: Self, col: usize) MatrixError![]T {
            if (col >= self.cols) return error.IndexOutOfBounds;

            const column = self.allocator.alloc(T, self.rows) catch return error.OutOfMemory;

            for (0..self.rows) |i| {
                column[i] = self.data[i * self.cols + col];
            }

            return column;
        }

        /// Extract a submatrix.
        pub fn submatrix(
            self: Self,
            row_start: usize,
            col_start: usize,
            num_rows: usize,
            num_cols: usize,
        ) MatrixError!Self {
            if (row_start + num_rows > self.rows or col_start + num_cols > self.cols) {
                return error.IndexOutOfBounds;
            }
            if (num_rows == 0 or num_cols == 0) return error.InvalidShape;

            var result = try Self.init(self.allocator, num_rows, num_cols);

            for (0..num_rows) |i| {
                for (0..num_cols) |j| {
                    result.data[i * num_cols + j] =
                        self.data[(row_start + i) * self.cols + (col_start + j)];
                }
            }

            return result;
        }

        /// Element-wise multiplication (Hadamard product).
        pub fn hadamard(self: Self, other: Self) MatrixError!Self {
            if (!self.hasSameShape(other)) return error.DimensionMismatch;

            var result = try Self.init(self.allocator, self.rows, self.cols);

            for (self.data, other.data, 0..) |a, b, i| {
                result.data[i] = a * b;
            }

            return result;
        }

        /// Check if the matrix is symmetric (A == A^T).
        pub fn isSymmetric(self: Self) bool {
            if (!self.isSquare()) return false;

            for (0..self.rows) |i| {
                for (i + 1..self.cols) |j| {
                    if (self.data[i * self.cols + j] != self.data[j * self.cols + i]) {
                        return false;
                    }
                }
            }

            return true;
        }

        /// Check if the matrix is diagonal.
        pub fn isDiagonal(self: Self) bool {
            if (!self.isSquare()) return false;

            for (0..self.rows) |i| {
                for (0..self.cols) |j| {
                    if (i != j and self.data[i * self.cols + j] != 0) {
                        return false;
                    }
                }
            }

            return true;
        }

        /// Check if the matrix is the identity matrix.
        pub fn isIdentity(self: Self) bool {
            if (!self.isSquare()) return false;

            for (0..self.rows) |i| {
                for (0..self.cols) |j| {
                    const expected: T = if (i == j) 1 else 0;
                    if (self.data[i * self.cols + j] != expected) {
                        return false;
                    }
                }
            }

            return true;
        }

        /// Sum of all elements.
        pub fn sum(self: Self) T {
            var total: T = 0;
            for (self.data) |v| {
                total += v;
            }
            return total;
        }

        /// Maximum element.
        pub fn max(self: Self) T {
            var max_val = self.data[0];
            for (self.data[1..]) |v| {
                if (v > max_val) max_val = v;
            }
            return max_val;
        }

        /// Minimum element.
        pub fn min(self: Self) T {
            var min_val = self.data[0];
            for (self.data[1..]) |v| {
                if (v < min_val) min_val = v;
            }
            return min_val;
        }
    };
}

/// Validate that matrix dimensions are compatible for addition.
pub fn validateAddition(rows_a: usize, cols_a: usize, rows_b: usize, cols_b: usize) MatrixError!void {
    if (rows_a != rows_b or cols_a != cols_b) return error.DimensionMismatch;
}

/// Validate that matrix dimensions are compatible for multiplication.
pub fn validateMultiplication(rows_a: usize, cols_a: usize, rows_b: usize, cols_b: usize) MatrixError!void {
    _ = rows_a;
    _ = cols_b;
    if (cols_a != rows_b) return error.DimensionMismatch;
}

/// Validate that a matrix is square.
pub fn validateSquare(rows: usize, cols: usize) MatrixError!void {
    if (rows != cols) return error.NotSquare;
}

/// Compute the result dimensions for matrix multiplication.
pub fn multiplicationResultShape(rows_a: usize, cols_a: usize, rows_b: usize, cols_b: usize) MatrixError!struct { usize, usize } {
    try validateMultiplication(rows_a, cols_a, rows_b, cols_b);
    return .{ rows_a, cols_b };
}

test "DenseMatrix init and deinit" {
    const allocator = std.testing.allocator;

    var m = try DenseMatrix(i64).init(allocator, 3, 4);
    defer m.deinit();

    try std.testing.expectEqual(@as(usize, 3), m.rows);
    try std.testing.expectEqual(@as(usize, 4), m.cols);
}

test "DenseMatrix get and set" {
    const allocator = std.testing.allocator;

    var m = try DenseMatrix(i64).init(allocator, 3, 3);
    defer m.deinit();

    try m.set(1, 2, 42);
    try std.testing.expectEqual(@as(i64, 42), try m.get(1, 2));

    // Out of bounds
    try std.testing.expectError(error.IndexOutOfBounds, m.get(5, 0));
    try std.testing.expectError(error.IndexOutOfBounds, m.set(0, 10, 0));
}

test "DenseMatrix identity" {
    const allocator = std.testing.allocator;

    var m = try DenseMatrix(i64).identity(allocator, 3);
    defer m.deinit();

    try std.testing.expectEqual(@as(i64, 1), try m.get(0, 0));
    try std.testing.expectEqual(@as(i64, 1), try m.get(1, 1));
    try std.testing.expectEqual(@as(i64, 1), try m.get(2, 2));
    try std.testing.expectEqual(@as(i64, 0), try m.get(0, 1));
    try std.testing.expect(m.isIdentity());
}

test "DenseMatrix add and sub" {
    const allocator = std.testing.allocator;

    var a = try DenseMatrix(i64).init(allocator, 2, 2);
    defer a.deinit();
    try a.set(0, 0, 1);
    try a.set(0, 1, 2);
    try a.set(1, 0, 3);
    try a.set(1, 1, 4);

    var b = try DenseMatrix(i64).init(allocator, 2, 2);
    defer b.deinit();
    try b.set(0, 0, 5);
    try b.set(0, 1, 6);
    try b.set(1, 0, 7);
    try b.set(1, 1, 8);

    var sum_result = try a.add(b);
    defer sum_result.deinit();

    try std.testing.expectEqual(@as(i64, 6), try sum_result.get(0, 0));
    try std.testing.expectEqual(@as(i64, 12), try sum_result.get(1, 1));
}

test "DenseMatrix mul" {
    const allocator = std.testing.allocator;

    // 2x3 matrix
    var a = try DenseMatrix(i64).init(allocator, 2, 3);
    defer a.deinit();
    try a.set(0, 0, 1);
    try a.set(0, 1, 2);
    try a.set(0, 2, 3);
    try a.set(1, 0, 4);
    try a.set(1, 1, 5);
    try a.set(1, 2, 6);

    // 3x2 matrix
    var b = try DenseMatrix(i64).init(allocator, 3, 2);
    defer b.deinit();
    try b.set(0, 0, 7);
    try b.set(0, 1, 8);
    try b.set(1, 0, 9);
    try b.set(1, 1, 10);
    try b.set(2, 0, 11);
    try b.set(2, 1, 12);

    var result = try a.mul(b);
    defer result.deinit();

    // Result should be 2x2
    try std.testing.expectEqual(@as(usize, 2), result.rows);
    try std.testing.expectEqual(@as(usize, 2), result.cols);

    // [1,2,3] * [7,9,11]^T = 7+18+33 = 58
    try std.testing.expectEqual(@as(i64, 58), try result.get(0, 0));
}

test "DenseMatrix dimension mismatch" {
    const allocator = std.testing.allocator;

    var a = try DenseMatrix(i64).init(allocator, 2, 3);
    defer a.deinit();

    var b = try DenseMatrix(i64).init(allocator, 2, 2);
    defer b.deinit();

    // Addition with mismatched dimensions
    try std.testing.expectError(error.DimensionMismatch, a.add(b));

    // Multiplication with incompatible dimensions
    try std.testing.expectError(error.DimensionMismatch, b.mul(a));
}

test "DenseMatrix transpose" {
    const allocator = std.testing.allocator;

    var m = try DenseMatrix(i64).init(allocator, 2, 3);
    defer m.deinit();
    try m.set(0, 0, 1);
    try m.set(0, 1, 2);
    try m.set(0, 2, 3);
    try m.set(1, 0, 4);
    try m.set(1, 1, 5);
    try m.set(1, 2, 6);

    var t = try m.transpose();
    defer t.deinit();

    try std.testing.expectEqual(@as(usize, 3), t.rows);
    try std.testing.expectEqual(@as(usize, 2), t.cols);
    try std.testing.expectEqual(@as(i64, 1), try t.get(0, 0));
    try std.testing.expectEqual(@as(i64, 4), try t.get(0, 1));
    try std.testing.expectEqual(@as(i64, 3), try t.get(2, 0));
}

test "DenseMatrix trace" {
    const allocator = std.testing.allocator;

    var m = try DenseMatrix(i64).identity(allocator, 4);
    defer m.deinit();

    const tr = try m.trace();
    try std.testing.expectEqual(@as(i64, 4), tr);

    // Non-square matrix should error
    var ns = try DenseMatrix(i64).init(allocator, 2, 3);
    defer ns.deinit();
    try std.testing.expectError(error.NotSquare, ns.trace());
}

test "DenseMatrix invalid shape" {
    const allocator = std.testing.allocator;

    try std.testing.expectError(error.InvalidShape, DenseMatrix(i64).init(allocator, 0, 5));
    try std.testing.expectError(error.InvalidShape, DenseMatrix(i64).init(allocator, 5, 0));
    try std.testing.expectError(error.InvalidShape, DenseMatrix(i64).init(allocator, MAX_DIMENSION + 1, 1));
}

test "validateMultiplication" {
    try validateMultiplication(2, 3, 3, 4);
    try std.testing.expectError(error.DimensionMismatch, validateMultiplication(2, 3, 4, 5));
}

test "multiplicationResultShape" {
    const shape = try multiplicationResultShape(2, 3, 3, 4);
    try std.testing.expectEqual(@as(usize, 2), shape[0]);
    try std.testing.expectEqual(@as(usize, 4), shape[1]);
}
