// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Machine learning utilities and safe activation functions.
public enum SafeML {
    // MARK: - Activation Functions

    /// Sigmoid activation: 1 / (1 + exp(-x))
    public static func sigmoid(_ x: Double) -> Double {
        if x >= 0 {
            return 1 / (1 + exp(-x))
        } else {
            let expX = exp(x)
            return expX / (1 + expX)
        }
    }

    /// Tanh activation.
    public static func tanh(_ x: Double) -> Double {
        Foundation.tanh(x)
    }

    /// ReLU activation: max(0, x)
    public static func relu(_ x: Double) -> Double {
        max(0, x)
    }

    /// Leaky ReLU activation.
    public static func leakyRelu(_ x: Double, alpha: Double = 0.01) -> Double {
        x >= 0 ? x : alpha * x
    }

    /// ELU activation.
    public static func elu(_ x: Double, alpha: Double = 1.0) -> Double {
        x >= 0 ? x : alpha * (exp(x) - 1)
    }

    /// SELU activation.
    public static func selu(_ x: Double) -> Double {
        let alpha = 1.6732632423543772
        let scale = 1.0507009873554805
        return scale * (x >= 0 ? x : alpha * (exp(x) - 1))
    }

    /// GELU activation (Gaussian Error Linear Unit).
    public static func gelu(_ x: Double) -> Double {
        0.5 * x * (1 + Foundation.tanh(Foundation.sqrt(2 / .pi) * (x + 0.044715 * x * x * x)))
    }

    /// Swish activation: x * sigmoid(x)
    public static func swish(_ x: Double) -> Double {
        x * sigmoid(x)
    }

    /// Softplus activation: ln(1 + exp(x))
    public static func softplus(_ x: Double) -> Double {
        if x > 20 {
            return x
        } else if x < -20 {
            return exp(x)
        }
        return log(1 + exp(x))
    }

    /// Softsign activation: x / (1 + |x|)
    public static func softsign(_ x: Double) -> Double {
        x / (1 + abs(x))
    }

    // MARK: - Softmax

    /// Softmax for array of values.
    public static func softmax(_ values: [Double]) -> [Double] {
        guard !values.isEmpty else { return [] }

        // For numerical stability, subtract max
        let maxVal = values.max()!
        let exps = values.map { exp($0 - maxVal) }
        let sum = exps.reduce(0, +)

        return exps.map { $0 / sum }
    }

    /// Log-softmax for array of values.
    public static func logSoftmax(_ values: [Double]) -> [Double] {
        guard !values.isEmpty else { return [] }

        let maxVal = values.max()!
        let shifted = values.map { $0 - maxVal }
        let logSumExp = log(shifted.map { exp($0) }.reduce(0, +))

        return shifted.map { $0 - logSumExp }
    }

    // MARK: - Normalization

    /// Min-max normalization to [0, 1].
    public static func minMaxNormalize(_ values: [Double]) -> [Double] {
        guard !values.isEmpty else { return [] }

        let minVal = values.min()!
        let maxVal = values.max()!
        let range = maxVal - minVal

        guard range > 0 else { return values.map { _ in 0.5 } }
        return values.map { ($0 - minVal) / range }
    }

    /// Z-score normalization (standardization).
    public static func zScoreNormalize(_ values: [Double]) -> [Double] {
        guard !values.isEmpty else { return [] }

        let mean = values.reduce(0, +) / Double(values.count)
        let variance = values.map { ($0 - mean) * ($0 - mean) }.reduce(0, +) / Double(values.count)
        let std = Foundation.sqrt(variance)

        guard std > 0 else { return values.map { _ in 0 } }
        return values.map { ($0 - mean) / std }
    }

    /// L2 normalization.
    public static func l2Normalize(_ values: [Double]) -> [Double] {
        guard !values.isEmpty else { return [] }

        let norm = Foundation.sqrt(values.map { $0 * $0 }.reduce(0, +))
        guard norm > 0 else { return values }
        return values.map { $0 / norm }
    }

    // MARK: - Loss Functions

    /// Mean Squared Error.
    public static func mse(predicted: [Double], actual: [Double]) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var sum = 0.0
        for i in 0..<predicted.count {
            let diff = predicted[i] - actual[i]
            sum += diff * diff
        }
        return sum / Double(predicted.count)
    }

    /// Mean Absolute Error.
    public static func mae(predicted: [Double], actual: [Double]) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var sum = 0.0
        for i in 0..<predicted.count {
            sum += abs(predicted[i] - actual[i])
        }
        return sum / Double(predicted.count)
    }

    /// Binary Cross-Entropy Loss.
    public static func binaryCrossEntropy(predicted: [Double], actual: [Double], epsilon: Double = 1e-15) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var sum = 0.0
        for i in 0..<predicted.count {
            let p = max(epsilon, min(1 - epsilon, predicted[i]))
            let y = actual[i]
            sum -= y * log(p) + (1 - y) * log(1 - p)
        }
        return sum / Double(predicted.count)
    }

    /// Categorical Cross-Entropy Loss.
    public static func categoricalCrossEntropy(predicted: [[Double]], actual: [[Double]], epsilon: Double = 1e-15) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var sum = 0.0
        for i in 0..<predicted.count {
            guard predicted[i].count == actual[i].count else { return nil }
            for j in 0..<predicted[i].count {
                let p = max(epsilon, min(1 - epsilon, predicted[i][j]))
                sum -= actual[i][j] * log(p)
            }
        }
        return sum / Double(predicted.count)
    }

    // MARK: - Metrics

    /// Calculate accuracy.
    public static func accuracy(predicted: [Int], actual: [Int]) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var correct = 0
        for i in 0..<predicted.count {
            if predicted[i] == actual[i] {
                correct += 1
            }
        }
        return Double(correct) / Double(predicted.count)
    }

    /// Calculate precision.
    public static func precision(predicted: [Bool], actual: [Bool]) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var truePositives = 0
        var falsePositives = 0

        for i in 0..<predicted.count {
            if predicted[i] {
                if actual[i] {
                    truePositives += 1
                } else {
                    falsePositives += 1
                }
            }
        }

        let total = truePositives + falsePositives
        return total > 0 ? Double(truePositives) / Double(total) : 0
    }

    /// Calculate recall.
    public static func recall(predicted: [Bool], actual: [Bool]) -> Double? {
        guard predicted.count == actual.count && !predicted.isEmpty else { return nil }

        var truePositives = 0
        var falseNegatives = 0

        for i in 0..<predicted.count {
            if actual[i] {
                if predicted[i] {
                    truePositives += 1
                } else {
                    falseNegatives += 1
                }
            }
        }

        let total = truePositives + falseNegatives
        return total > 0 ? Double(truePositives) / Double(total) : 0
    }

    /// Calculate F1 score.
    public static func f1Score(predicted: [Bool], actual: [Bool]) -> Double? {
        guard let p = precision(predicted: predicted, actual: actual),
              let r = recall(predicted: predicted, actual: actual) else { return nil }

        return p + r > 0 ? 2 * p * r / (p + r) : 0
    }

    // MARK: - Data Utilities

    /// One-hot encode integer labels.
    public static func oneHotEncode(labels: [Int], numClasses: Int) -> [[Double]] {
        labels.map { label in
            var encoded = Array(repeating: 0.0, count: numClasses)
            if label >= 0 && label < numClasses {
                encoded[label] = 1.0
            }
            return encoded
        }
    }

    /// Argmax - index of maximum value.
    public static func argmax(_ values: [Double]) -> Int? {
        guard !values.isEmpty else { return nil }

        var maxIndex = 0
        var maxValue = values[0]

        for i in 1..<values.count {
            if values[i] > maxValue {
                maxValue = values[i]
                maxIndex = i
            }
        }

        return maxIndex
    }

    /// Clip values to range.
    public static func clip(_ values: [Double], min: Double, max: Double) -> [Double] {
        values.map { Swift.max(min, Swift.min(max, $0)) }
    }
}
