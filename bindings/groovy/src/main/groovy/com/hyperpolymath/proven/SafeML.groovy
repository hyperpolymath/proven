// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeML - JNA wrapper for proven_ml_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe ML activation functions via libproven JNA FFI.
 *
 * Every method delegates to the Idris 2 verified implementation.
 * No ML logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeML {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Sigmoid activation: 1 / (1 + exp(-x)). */
    static double sigmoid(double x) {
        LIB.proven_ml_sigmoid(x)
    }

    /** ReLU activation: max(0, x). */
    static double relu(double x) {
        LIB.proven_ml_relu(x)
    }

    /** Leaky ReLU activation: x if x > 0, alpha * x otherwise. */
    static double leakyRelu(double x, double alpha) {
        LIB.proven_ml_leaky_relu(x, alpha)
    }

    /** Clamp value to range [minVal, maxVal]. */
    static double clamp(double x, double minVal, double maxVal) {
        LIB.proven_ml_clamp(x, minVal, maxVal)
    }
}
