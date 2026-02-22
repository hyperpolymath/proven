// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Safe machine-learning activation functions via libproven FFI.
 *
 * Provides sigmoid, ReLU, leaky ReLU, and clamping.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No activation-function logic is reimplemented in Java.
 */
public final class SafeML {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeML() {}

    /**
     * Sigmoid activation function: 1 / (1 + exp(-x)).
     *
     * @param x the input value
     * @return the sigmoid output
     */
    public static double sigmoid(double x) {
        return LIB.proven_ml_sigmoid(x);
    }

    /**
     * ReLU activation function: max(0, x).
     *
     * @param x the input value
     * @return the ReLU output
     */
    public static double relu(double x) {
        return LIB.proven_ml_relu(x);
    }

    /**
     * Leaky ReLU activation function: x if x > 0, else alpha * x.
     *
     * @param x     the input value
     * @param alpha the leak coefficient for negative inputs
     * @return the leaky ReLU output
     */
    public static double leakyRelu(double x, double alpha) {
        return LIB.proven_ml_leaky_relu(x, alpha);
    }

    /**
     * Clamp a value to [min, max].
     *
     * @param x      the value to clamp
     * @param minVal the minimum bound
     * @param maxVal the maximum bound
     * @return the clamped value
     */
    public static double clamp(double x, double minVal, double maxVal) {
        return LIB.proven_ml_clamp(x, minVal, maxVal);
    }
}
