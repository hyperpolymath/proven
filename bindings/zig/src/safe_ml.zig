// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe machine learning utilities that cannot crash.

const std = @import("std");

/// Error types for ML operations.
pub const MLError = error{
    DimensionMismatch,
    InvalidInput,
    EmptyInput,
    OutOfMemory,
    NumericalInstability,
};

/// Activation functions
pub const Activation = enum {
    relu,
    sigmoid,
    tanh,
    softmax,
    leaky_relu,

    /// Apply activation function
    pub fn apply(self: Activation, x: f64) f64 {
        return switch (self) {
            .relu => @max(0, x),
            .sigmoid => 1.0 / (1.0 + @exp(-x)),
            .tanh => std.math.tanh(x),
            .leaky_relu => if (x > 0) x else 0.01 * x,
            .softmax => x, // Requires full vector
        };
    }

    /// Derivative for backpropagation
    pub fn derivative(self: Activation, x: f64) f64 {
        return switch (self) {
            .relu => if (x > 0) 1 else 0,
            .sigmoid => {
                const s = self.apply(x);
                return s * (1 - s);
            },
            .tanh => {
                const t = self.apply(x);
                return 1 - t * t;
            },
            .leaky_relu => if (x > 0) 1 else 0.01,
            .softmax => 1, // Special case
        };
    }
};

/// Normalize a slice to [0, 1] range
pub fn normalize(data: []f64) MLError!void {
    if (data.len == 0) return error.EmptyInput;

    var min: f64 = data[0];
    var max: f64 = data[0];

    for (data) |v| {
        if (v < min) min = v;
        if (v > max) max = v;
    }

    const range = max - min;
    if (range == 0) return;

    for (data) |*v| {
        v.* = (v.* - min) / range;
    }
}

/// Standardize to zero mean and unit variance (z-score)
pub fn standardize(data: []f64) MLError!void {
    if (data.len == 0) return error.EmptyInput;

    // Calculate mean
    var sum: f64 = 0;
    for (data) |v| sum += v;
    const mean = sum / @as(f64, @floatFromInt(data.len));

    // Calculate standard deviation
    var variance: f64 = 0;
    for (data) |v| {
        const diff = v - mean;
        variance += diff * diff;
    }
    variance /= @as(f64, @floatFromInt(data.len));
    const std_dev = @sqrt(variance);

    if (std_dev == 0) return;

    // Standardize
    for (data) |*v| {
        v.* = (v.* - mean) / std_dev;
    }
}

/// Softmax function for a slice
pub fn softmax(allocator: std.mem.Allocator, input: []const f64) ![]f64 {
    if (input.len == 0) return error.EmptyInput;

    var result = try allocator.alloc(f64, input.len);
    errdefer allocator.free(result);

    // Find max for numerical stability
    var max: f64 = input[0];
    for (input) |v| {
        if (v > max) max = v;
    }

    // Calculate exp and sum
    var sum: f64 = 0;
    for (input, 0..) |v, i| {
        result[i] = @exp(v - max);
        sum += result[i];
    }

    // Normalize
    if (sum == 0) return error.NumericalInstability;
    for (result) |*v| {
        v.* /= sum;
    }

    return result;
}

/// Cross-entropy loss
pub fn crossEntropyLoss(predictions: []const f64, targets: []const f64) MLError!f64 {
    if (predictions.len != targets.len) return error.DimensionMismatch;
    if (predictions.len == 0) return error.EmptyInput;

    var loss: f64 = 0;
    const epsilon = 1e-15; // Prevent log(0)

    for (predictions, targets) |p, t| {
        const clipped = @max(epsilon, @min(1 - epsilon, p));
        loss -= t * @log(clipped);
    }

    return loss / @as(f64, @floatFromInt(predictions.len));
}

/// Mean squared error loss
pub fn mseLoss(predictions: []const f64, targets: []const f64) MLError!f64 {
    if (predictions.len != targets.len) return error.DimensionMismatch;
    if (predictions.len == 0) return error.EmptyInput;

    var sum: f64 = 0;
    for (predictions, targets) |p, t| {
        const diff = p - t;
        sum += diff * diff;
    }

    return sum / @as(f64, @floatFromInt(predictions.len));
}

/// Binary accuracy
pub fn binaryAccuracy(predictions: []const f64, targets: []const f64, threshold: f64) MLError!f64 {
    if (predictions.len != targets.len) return error.DimensionMismatch;
    if (predictions.len == 0) return error.EmptyInput;

    var correct: usize = 0;
    for (predictions, targets) |p, t| {
        const predicted_class: f64 = if (p >= threshold) 1 else 0;
        if (predicted_class == t) correct += 1;
    }

    return @as(f64, @floatFromInt(correct)) / @as(f64, @floatFromInt(predictions.len));
}

/// Clip values to a range
pub fn clip(data: []f64, min_val: f64, max_val: f64) void {
    for (data) |*v| {
        v.* = @max(min_val, @min(max_val, v.*));
    }
}

/// Dot product of two vectors
pub fn dot(a: []const f64, b: []const f64) MLError!f64 {
    if (a.len != b.len) return error.DimensionMismatch;

    var sum: f64 = 0;
    for (a, b) |va, vb| {
        sum += va * vb;
    }
    return sum;
}

/// L2 norm (Euclidean)
pub fn l2Norm(data: []const f64) f64 {
    var sum: f64 = 0;
    for (data) |v| sum += v * v;
    return @sqrt(sum);
}

/// L1 norm (Manhattan)
pub fn l1Norm(data: []const f64) f64 {
    var sum: f64 = 0;
    for (data) |v| sum += @abs(v);
    return sum;
}

/// Cosine similarity
pub fn cosineSimilarity(a: []const f64, b: []const f64) MLError!f64 {
    if (a.len != b.len) return error.DimensionMismatch;

    const dot_product = try dot(a, b);
    const norm_a = l2Norm(a);
    const norm_b = l2Norm(b);

    if (norm_a == 0 or norm_b == 0) return error.NumericalInstability;

    return dot_product / (norm_a * norm_b);
}

/// One-hot encode an index
pub fn oneHot(allocator: std.mem.Allocator, index: usize, num_classes: usize) ![]f64 {
    if (index >= num_classes) return error.InvalidInput;

    var result = try allocator.alloc(f64, num_classes);
    @memset(result, 0);
    result[index] = 1;

    return result;
}

/// Argmax - find index of maximum value
pub fn argmax(data: []const f64) MLError!usize {
    if (data.len == 0) return error.EmptyInput;

    var max_idx: usize = 0;
    var max_val = data[0];

    for (data[1..], 1..) |v, i| {
        if (v > max_val) {
            max_val = v;
            max_idx = i;
        }
    }

    return max_idx;
}

/// Argmin - find index of minimum value
pub fn argmin(data: []const f64) MLError!usize {
    if (data.len == 0) return error.EmptyInput;

    var min_idx: usize = 0;
    var min_val = data[0];

    for (data[1..], 1..) |v, i| {
        if (v < min_val) {
            min_val = v;
            min_idx = i;
        }
    }

    return min_idx;
}

test "activation functions" {
    try std.testing.expectEqual(@as(f64, 0), Activation.relu.apply(-1));
    try std.testing.expectEqual(@as(f64, 1), Activation.relu.apply(1));
    try std.testing.expect(Activation.sigmoid.apply(0) > 0.49);
    try std.testing.expect(Activation.sigmoid.apply(0) < 0.51);
}

test "normalize" {
    var data = [_]f64{ 0, 50, 100 };
    try normalize(&data);
    try std.testing.expectApproxEqAbs(@as(f64, 0), data[0], 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 0.5), data[1], 0.001);
    try std.testing.expectApproxEqAbs(@as(f64, 1), data[2], 0.001);
}

test "softmax" {
    const allocator = std.testing.allocator;
    const input = [_]f64{ 1, 2, 3 };
    const result = try softmax(allocator, &input);
    defer allocator.free(result);

    // Sum should be 1
    var sum: f64 = 0;
    for (result) |v| sum += v;
    try std.testing.expectApproxEqAbs(@as(f64, 1), sum, 0.001);
}

test "argmax" {
    const data = [_]f64{ 1, 5, 3, 2 };
    try std.testing.expectEqual(@as(usize, 1), try argmax(&data));
}
