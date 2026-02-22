// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// 2D Tensor operations delegated to libproven FFI.
///
/// Matrix creation, element access, and multiplication via the
/// formally verified Idris 2 core.

import CProven

/// 2D Tensor backed by libproven.
/// This class manages the lifecycle of a C-allocated tensor.
public final class SafeTensor {
    private let tensor: UnsafeMutablePointer<ProvenTensor2D>

    /// The number of rows.
    public let rows: Int
    /// The number of columns.
    public let cols: Int

    /// Create a 2D tensor (matrix) with the given dimensions.
    /// Returns nil if allocation fails or dimensions are invalid.
    public init?(rows: Int, cols: Int) {
        guard let ptr = proven_tensor_create(rows, cols) else {
            return nil
        }
        self.tensor = ptr
        self.rows = rows
        self.cols = cols
    }

    /// Internal initializer from a raw pointer (used for matmul result).
    private init(raw: UnsafeMutablePointer<ProvenTensor2D>, rows: Int, cols: Int) {
        self.tensor = raw
        self.rows = rows
        self.cols = cols
    }

    deinit {
        proven_tensor_free(tensor)
    }

    /// Set a value at the given row and column.
    public func set(row: Int, col: Int, value: Double) -> Result<Void, ProvenError> {
        let status = proven_tensor_set(tensor, row, col, value)
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(())
    }

    /// Get a value at the given row and column.
    public func get(row: Int, col: Int) -> Result<Double, ProvenError> {
        let result = proven_tensor_get(tensor, row, col)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Matrix multiplication: self * other.
    /// Returns nil if dimensions are incompatible or allocation fails.
    public func matmul(_ other: SafeTensor) -> SafeTensor? {
        guard self.cols == other.rows else { return nil }
        guard let resultPtr = proven_tensor_matmul(self.tensor, other.tensor) else {
            return nil
        }
        return SafeTensor(raw: resultPtr, rows: self.rows, cols: other.cols)
    }
}
