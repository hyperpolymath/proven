// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.*

/**
 * Machine learning utility functions.
 */
object SafeML {
    // ========== Activation Functions ==========

    /**
     * Sigmoid activation: 1 / (1 + e^-x)
     */
    fun sigmoid(x: Double): Double = 1.0 / (1.0 + exp(-x))

    /**
     * Sigmoid derivative: sigmoid(x) * (1 - sigmoid(x))
     */
    fun sigmoidDerivative(x: Double): Double {
        val s = sigmoid(x)
        return s * (1 - s)
    }

    /**
     * Hyperbolic tangent activation.
     */
    fun tanh(x: Double): Double = kotlin.math.tanh(x)

    /**
     * Tanh derivative: 1 - tanh²(x)
     */
    fun tanhDerivative(x: Double): Double {
        val t = tanh(x)
        return 1 - t * t
    }

    /**
     * ReLU activation: max(0, x)
     */
    fun relu(x: Double): Double = maxOf(0.0, x)

    /**
     * ReLU derivative.
     */
    fun reluDerivative(x: Double): Double = if (x > 0) 1.0 else 0.0

    /**
     * Leaky ReLU activation.
     */
    fun leakyRelu(x: Double, alpha: Double = 0.01): Double = if (x > 0) x else alpha * x

    /**
     * Leaky ReLU derivative.
     */
    fun leakyReluDerivative(x: Double, alpha: Double = 0.01): Double = if (x > 0) 1.0 else alpha

    /**
     * ELU (Exponential Linear Unit).
     */
    fun elu(x: Double, alpha: Double = 1.0): Double = if (x > 0) x else alpha * (exp(x) - 1)

    /**
     * SELU (Scaled ELU).
     */
    fun selu(x: Double): Double {
        val alpha = 1.6732632423543772848170429916717
        val scale = 1.0507009873554804934193349852946
        return scale * (if (x > 0) x else alpha * (exp(x) - 1))
    }

    /**
     * Swish activation: x * sigmoid(x)
     */
    fun swish(x: Double): Double = x * sigmoid(x)

    /**
     * GELU (Gaussian Error Linear Unit).
     */
    fun gelu(x: Double): Double {
        return 0.5 * x * (1 + kotlin.math.tanh(sqrt(2 / PI) * (x + 0.044715 * x.pow(3))))
    }

    /**
     * Softplus: log(1 + e^x)
     */
    fun softplus(x: Double): Double = ln(1 + exp(x))

    /**
     * Softsign: x / (1 + |x|)
     */
    fun softsign(x: Double): Double = x / (1 + abs(x))

    // ========== Vector Activation Functions ==========

    /**
     * Softmax activation for a vector.
     */
    fun softmax(x: List<Double>): List<Double> {
        val maxVal = x.maxOrNull() ?: return x
        val exps = x.map { exp(it - maxVal) }
        val sumExps = exps.sum()
        return exps.map { it / sumExps }
    }

    /**
     * Log-softmax for numerical stability.
     */
    fun logSoftmax(x: List<Double>): List<Double> {
        val maxVal = x.maxOrNull() ?: return x
        val shifted = x.map { it - maxVal }
        val logSumExp = ln(shifted.sumOf { exp(it) })
        return shifted.map { it - logSumExp }
    }

    // ========== Loss Functions ==========

    /**
     * Mean Squared Error.
     */
    fun mse(predicted: List<Double>, actual: List<Double>): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null
        val sum = predicted.zip(actual).sumOf { (p, a) -> (p - a).pow(2) }
        return sum / predicted.size
    }

    /**
     * Root Mean Squared Error.
     */
    fun rmse(predicted: List<Double>, actual: List<Double>): Double? {
        val mseVal = mse(predicted, actual) ?: return null
        return sqrt(mseVal)
    }

    /**
     * Mean Absolute Error.
     */
    fun mae(predicted: List<Double>, actual: List<Double>): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null
        val sum = predicted.zip(actual).sumOf { (p, a) -> abs(p - a) }
        return sum / predicted.size
    }

    /**
     * Binary Cross-Entropy loss.
     */
    fun binaryCrossEntropy(predicted: List<Double>, actual: List<Double>, epsilon: Double = 1e-15): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null

        val sum = predicted.zip(actual).sumOf { (p, a) ->
            val clippedP = p.coerceIn(epsilon, 1 - epsilon)
            -a * ln(clippedP) - (1 - a) * ln(1 - clippedP)
        }

        return sum / predicted.size
    }

    /**
     * Categorical Cross-Entropy loss.
     */
    fun categoricalCrossEntropy(predicted: List<Double>, actual: List<Double>, epsilon: Double = 1e-15): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null

        val sum = predicted.zip(actual).sumOf { (p, a) ->
            val clippedP = p.coerceIn(epsilon, 1 - epsilon)
            -a * ln(clippedP)
        }

        return sum
    }

    /**
     * Huber loss (smooth L1).
     */
    fun huberLoss(predicted: List<Double>, actual: List<Double>, delta: Double = 1.0): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null

        val sum = predicted.zip(actual).sumOf { (p, a) ->
            val error = abs(p - a)
            if (error <= delta) {
                0.5 * error.pow(2)
            } else {
                delta * error - 0.5 * delta.pow(2)
            }
        }

        return sum / predicted.size
    }

    // ========== Metrics ==========

    /**
     * Accuracy for classification.
     */
    fun accuracy(predicted: List<Int>, actual: List<Int>): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null
        val correct = predicted.zip(actual).count { (p, a) -> p == a }
        return correct.toDouble() / predicted.size
    }

    /**
     * Precision for binary classification.
     */
    fun precision(predicted: List<Int>, actual: List<Int>, positiveClass: Int = 1): Double? {
        if (predicted.size != actual.size) return null

        val truePositives = predicted.zip(actual).count { (p, a) -> p == positiveClass && a == positiveClass }
        val falsePositives = predicted.zip(actual).count { (p, a) -> p == positiveClass && a != positiveClass }

        val total = truePositives + falsePositives
        return if (total == 0) 0.0 else truePositives.toDouble() / total
    }

    /**
     * Recall for binary classification.
     */
    fun recall(predicted: List<Int>, actual: List<Int>, positiveClass: Int = 1): Double? {
        if (predicted.size != actual.size) return null

        val truePositives = predicted.zip(actual).count { (p, a) -> p == positiveClass && a == positiveClass }
        val falseNegatives = predicted.zip(actual).count { (p, a) -> p != positiveClass && a == positiveClass }

        val total = truePositives + falseNegatives
        return if (total == 0) 0.0 else truePositives.toDouble() / total
    }

    /**
     * F1 Score.
     */
    fun f1Score(predicted: List<Int>, actual: List<Int>, positiveClass: Int = 1): Double? {
        val p = precision(predicted, actual, positiveClass) ?: return null
        val r = recall(predicted, actual, positiveClass) ?: return null

        return if (p + r == 0.0) 0.0 else 2 * p * r / (p + r)
    }

    /**
     * R² (coefficient of determination).
     */
    fun r2Score(predicted: List<Double>, actual: List<Double>): Double? {
        if (predicted.size != actual.size || predicted.isEmpty()) return null

        val mean = actual.average()
        val ssRes = predicted.zip(actual).sumOf { (p, a) -> (a - p).pow(2) }
        val ssTot = actual.sumOf { (it - mean).pow(2) }

        return if (ssTot == 0.0) 1.0 else 1 - ssRes / ssTot
    }

    // ========== Normalization ==========

    /**
     * Min-max normalization to [0, 1].
     */
    fun minMaxNormalize(data: List<Double>): List<Double> {
        val minVal = data.minOrNull() ?: return data
        val maxVal = data.maxOrNull() ?: return data
        val range = maxVal - minVal

        return if (range == 0.0) {
            data.map { 0.5 }
        } else {
            data.map { (it - minVal) / range }
        }
    }

    /**
     * Z-score normalization (standardization).
     */
    fun zScoreNormalize(data: List<Double>): List<Double> {
        if (data.isEmpty()) return data

        val mean = data.average()
        val variance = data.sumOf { (it - mean).pow(2) } / data.size
        val std = sqrt(variance)

        return if (std == 0.0) {
            data.map { 0.0 }
        } else {
            data.map { (it - mean) / std }
        }
    }

    /**
     * L2 normalization (unit vector).
     */
    fun l2Normalize(data: List<Double>): List<Double> {
        val norm = sqrt(data.sumOf { it.pow(2) })
        return if (norm == 0.0) data else data.map { it / norm }
    }

    /**
     * Batch normalization.
     */
    fun batchNormalize(
        data: List<Double>,
        gamma: Double = 1.0,
        beta: Double = 0.0,
        epsilon: Double = 1e-5
    ): List<Double> {
        if (data.isEmpty()) return data

        val mean = data.average()
        val variance = data.sumOf { (it - mean).pow(2) } / data.size

        return data.map { x ->
            gamma * (x - mean) / sqrt(variance + epsilon) + beta
        }
    }

    // ========== Utility Functions ==========

    /**
     * One-hot encode a single integer.
     */
    fun oneHotEncode(value: Int, numClasses: Int): List<Double>? {
        if (value < 0 || value >= numClasses) return null
        return List(numClasses) { if (it == value) 1.0 else 0.0 }
    }

    /**
     * One-hot encode a list of integers.
     */
    fun oneHotEncode(values: List<Int>, numClasses: Int): List<List<Double>>? {
        return values.map { oneHotEncode(it, numClasses) ?: return null }
    }

    /**
     * Argmax: index of maximum value.
     */
    fun argmax(values: List<Double>): Int? {
        if (values.isEmpty()) return null
        return values.indices.maxByOrNull { values[it] }
    }

    /**
     * Argmin: index of minimum value.
     */
    fun argmin(values: List<Double>): Int? {
        if (values.isEmpty()) return null
        return values.indices.minByOrNull { values[it] }
    }

    /**
     * Clip values to a range.
     */
    fun clip(data: List<Double>, minVal: Double, maxVal: Double): List<Double> {
        return data.map { it.coerceIn(minVal, maxVal) }
    }

    /**
     * Cosine similarity between two vectors.
     */
    fun cosineSimilarity(a: List<Double>, b: List<Double>): Double? {
        if (a.size != b.size || a.isEmpty()) return null

        val dotProduct = a.zip(b).sumOf { (x, y) -> x * y }
        val normA = sqrt(a.sumOf { it.pow(2) })
        val normB = sqrt(b.sumOf { it.pow(2) })

        return if (normA == 0.0 || normB == 0.0) 0.0 else dotProduct / (normA * normB)
    }

    /**
     * Euclidean distance between two vectors.
     */
    fun euclideanDistance(a: List<Double>, b: List<Double>): Double? {
        if (a.size != b.size) return null
        return sqrt(a.zip(b).sumOf { (x, y) -> (x - y).pow(2) })
    }

    /**
     * Manhattan distance between two vectors.
     */
    fun manhattanDistance(a: List<Double>, b: List<Double>): Double? {
        if (a.size != b.size) return null
        return a.zip(b).sumOf { (x, y) -> abs(x - y) }
    }
}
