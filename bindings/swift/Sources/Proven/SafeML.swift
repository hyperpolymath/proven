// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Machine learning utilities delegated to libproven FFI.
///
/// Activation functions (softmax, sigmoid, ReLU) via the
/// formally verified Idris 2 core.

import CProven

/// Machine learning utilities backed by libproven.
public enum SafeML {
    /// Softmax activation function over an array of values.
    /// Returns the softmax-normalized output, or an error.
    public static func softmax(_ input: [Double]) -> Result<[Double], ProvenError> {
        guard !input.isEmpty else { return .success([]) }

        var output = [Double](repeating: 0.0, count: input.count)
        let status = input.withUnsafeBufferPointer { inBuf in
            output.withUnsafeMutableBufferPointer { outBuf in
                proven_ml_softmax(inBuf.baseAddress, outBuf.baseAddress, input.count)
            }
        }
        if let error = ProvenError.fromStatus(status) {
            return .failure(error)
        }
        return .success(output)
    }

    /// Sigmoid activation: 1 / (1 + exp(-x)).
    public static func sigmoid(_ x: Double) -> Double {
        proven_ml_sigmoid(x)
    }

    /// ReLU activation: max(0, x).
    public static func relu(_ x: Double) -> Double {
        proven_ml_relu(x)
    }
}
