// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Shape of a tensor.
public struct TensorShape: Equatable, Hashable {
    public let dimensions: [Int]

    public init(_ dimensions: [Int]) {
        self.dimensions = dimensions.map { max(0, $0) }
    }

    public init(_ dimensions: Int...) {
        self.dimensions = dimensions.map { max(0, $0) }
    }

    /// Number of dimensions (rank).
    public var rank: Int { dimensions.count }

    /// Total number of elements.
    public var elementCount: Int {
        dimensions.reduce(1, *)
    }

    /// Check if shapes are broadcast-compatible.
    public func isBroadcastCompatible(with other: TensorShape) -> Bool {
        let maxRank = max(rank, other.rank)
        let paddedSelf = Array(repeating: 1, count: maxRank - rank) + dimensions
        let paddedOther = Array(repeating: 1, count: maxRank - other.rank) + other.dimensions

        for i in 0..<maxRank {
            if paddedSelf[i] != paddedOther[i] && paddedSelf[i] != 1 && paddedOther[i] != 1 {
                return false
            }
        }
        return true
    }

    /// Get broadcast result shape.
    public func broadcastShape(with other: TensorShape) -> TensorShape? {
        guard isBroadcastCompatible(with: other) else { return nil }

        let maxRank = max(rank, other.rank)
        let paddedSelf = Array(repeating: 1, count: maxRank - rank) + dimensions
        let paddedOther = Array(repeating: 1, count: maxRank - other.rank) + other.dimensions

        var result = [Int]()
        for i in 0..<maxRank {
            result.append(max(paddedSelf[i], paddedOther[i]))
        }
        return TensorShape(result)
    }
}

/// A simple tensor (multi-dimensional array).
public struct Tensor<T: Numeric>: Equatable where T: Equatable {
    public let shape: TensorShape
    public private(set) var data: [T]

    /// Create tensor with shape and fill value.
    public init(shape: TensorShape, fill: T) {
        self.shape = shape
        self.data = Array(repeating: fill, count: shape.elementCount)
    }

    /// Create tensor with shape and data.
    public init?(shape: TensorShape, data: [T]) {
        guard data.count == shape.elementCount else { return nil }
        self.shape = shape
        self.data = data
    }

    /// Create a scalar tensor.
    public init(scalar: T) {
        self.shape = TensorShape([])
        self.data = [scalar]
    }

    /// Create a 1D tensor (vector).
    public init(vector: [T]) {
        self.shape = TensorShape([vector.count])
        self.data = vector
    }

    /// Create a 2D tensor (matrix).
    public init?(matrix: [[T]]) {
        guard !matrix.isEmpty && !matrix[0].isEmpty else { return nil }
        let rows = matrix.count
        let cols = matrix[0].count

        // Verify all rows have same length
        for row in matrix {
            if row.count != cols { return nil }
        }

        self.shape = TensorShape([rows, cols])
        self.data = matrix.flatMap { $0 }
    }

    /// Get element at flat index.
    public subscript(flatIndex: Int) -> T? {
        get {
            guard flatIndex >= 0 && flatIndex < data.count else { return nil }
            return data[flatIndex]
        }
        set {
            guard let value = newValue, flatIndex >= 0 && flatIndex < data.count else { return }
            data[flatIndex] = value
        }
    }

    /// Convert multi-dimensional indices to flat index.
    public func flatIndex(_ indices: [Int]) -> Int? {
        guard indices.count == shape.rank else { return nil }

        var flat = 0
        var stride = 1

        for i in (0..<shape.rank).reversed() {
            guard indices[i] >= 0 && indices[i] < shape.dimensions[i] else { return nil }
            flat += indices[i] * stride
            stride *= shape.dimensions[i]
        }

        return flat
    }

    /// Get element at multi-dimensional indices.
    public func get(_ indices: Int...) -> T? {
        guard let flat = flatIndex(indices) else { return nil }
        return data[flat]
    }

    /// Set element at multi-dimensional indices.
    public mutating func set(_ indices: [Int], value: T) -> Bool {
        guard let flat = flatIndex(indices) else { return false }
        data[flat] = value
        return true
    }

    /// Reshape tensor to new shape (must have same element count).
    public func reshape(_ newShape: TensorShape) -> Tensor<T>? {
        guard newShape.elementCount == shape.elementCount else { return nil }
        return Tensor(shape: newShape, data: data)
    }

    /// Transpose 2D tensor.
    public func transpose() -> Tensor<T>? {
        guard shape.rank == 2 else { return nil }

        let rows = shape.dimensions[0]
        let cols = shape.dimensions[1]
        var newData = [T]()
        newData.reserveCapacity(data.count)

        for j in 0..<cols {
            for i in 0..<rows {
                newData.append(data[i * cols + j])
            }
        }

        return Tensor(shape: TensorShape([cols, rows]), data: newData)
    }

    /// Map function over all elements.
    public func map<U: Numeric>(_ transform: (T) -> U) -> Tensor<U> where U: Equatable {
        Tensor<U>(shape: shape, data: data.map(transform))!
    }
}

/// Tensor operations.
public enum SafeTensor {
    /// Create zeros tensor.
    public static func zeros<T: Numeric>(shape: TensorShape) -> Tensor<T> where T: Equatable {
        Tensor(shape: shape, fill: T.zero)
    }

    /// Create ones tensor.
    public static func ones(shape: TensorShape) -> Tensor<Double> {
        Tensor(shape: shape, fill: 1.0)
    }

    /// Create identity matrix.
    public static func eye(n: Int) -> Tensor<Double>? {
        guard n > 0 else { return nil }
        var data = Array(repeating: 0.0, count: n * n)
        for i in 0..<n {
            data[i * n + i] = 1.0
        }
        return Tensor(shape: TensorShape([n, n]), data: data)
    }

    /// Element-wise addition.
    public static func add(_ a: Tensor<Double>, _ b: Tensor<Double>) -> Tensor<Double>? {
        guard a.shape == b.shape else { return nil }
        var result = a
        for i in 0..<a.data.count {
            result.data[i] += b.data[i]
        }
        return result
    }

    /// Element-wise subtraction.
    public static func subtract(_ a: Tensor<Double>, _ b: Tensor<Double>) -> Tensor<Double>? {
        guard a.shape == b.shape else { return nil }
        var result = a
        for i in 0..<a.data.count {
            result.data[i] -= b.data[i]
        }
        return result
    }

    /// Element-wise multiplication (Hadamard product).
    public static func multiply(_ a: Tensor<Double>, _ b: Tensor<Double>) -> Tensor<Double>? {
        guard a.shape == b.shape else { return nil }
        var result = a
        for i in 0..<a.data.count {
            result.data[i] *= b.data[i]
        }
        return result
    }

    /// Scalar multiplication.
    public static func scale(_ tensor: Tensor<Double>, by scalar: Double) -> Tensor<Double> {
        tensor.map { $0 * scalar }
    }

    /// Matrix multiplication (2D tensors only).
    public static func matmul(_ a: Tensor<Double>, _ b: Tensor<Double>) -> Tensor<Double>? {
        guard a.shape.rank == 2 && b.shape.rank == 2 else { return nil }
        guard a.shape.dimensions[1] == b.shape.dimensions[0] else { return nil }

        let m = a.shape.dimensions[0]
        let n = a.shape.dimensions[1]
        let p = b.shape.dimensions[1]

        var result = Array(repeating: 0.0, count: m * p)

        for i in 0..<m {
            for j in 0..<p {
                var sum = 0.0
                for k in 0..<n {
                    sum += a.data[i * n + k] * b.data[k * p + j]
                }
                result[i * p + j] = sum
            }
        }

        return Tensor(shape: TensorShape([m, p]), data: result)
    }

    /// Dot product of two vectors.
    public static func dot(_ a: Tensor<Double>, _ b: Tensor<Double>) -> Double? {
        guard a.shape.rank == 1 && b.shape.rank == 1 else { return nil }
        guard a.shape == b.shape else { return nil }

        var sum = 0.0
        for i in 0..<a.data.count {
            sum += a.data[i] * b.data[i]
        }
        return sum
    }

    /// Sum of all elements.
    public static func sum(_ tensor: Tensor<Double>) -> Double {
        tensor.data.reduce(0, +)
    }

    /// Mean of all elements.
    public static func mean(_ tensor: Tensor<Double>) -> Double {
        sum(tensor) / Double(tensor.data.count)
    }

    /// Maximum element.
    public static func max(_ tensor: Tensor<Double>) -> Double? {
        tensor.data.max()
    }

    /// Minimum element.
    public static func min(_ tensor: Tensor<Double>) -> Double? {
        tensor.data.min()
    }

    /// L2 norm (Frobenius norm for matrices).
    public static func norm(_ tensor: Tensor<Double>) -> Double {
        var sum = 0.0
        for value in tensor.data {
            sum += value * value
        }
        return Foundation.sqrt(sum)
    }
}
