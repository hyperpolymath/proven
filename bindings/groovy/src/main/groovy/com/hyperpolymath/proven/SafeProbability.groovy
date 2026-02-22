// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeProbability - JNA wrapper for proven_probability_* functions.
// All computation delegated to libproven via JNA; no logic reimplemented here.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic

/**
 * Safe probability operations via libproven JNA FFI.
 *
 * All values are clamped to [0.0, 1.0] by the verified implementation.
 * Every method delegates to the Idris 2 verified implementation.
 * No probability logic is reimplemented in Groovy.
 */
@CompileStatic
class SafeProbability {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE

    /** Create a valid probability (clamped to [0, 1]). */
    static double create(double value) {
        LIB.proven_probability_create(value)
    }

    /** Probability of A AND B (independent events). */
    static double and(double a, double b) {
        LIB.proven_probability_and(a, b)
    }

    /** Probability of exclusive or. */
    static double orExclusive(double a, double b) {
        LIB.proven_probability_or_exclusive(a, b)
    }

    /** Complement probability: 1 - P. */
    static double not(double p) {
        LIB.proven_probability_not(p)
    }
}
