// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Safe probability operations via libproven FFI.
 *
 * Provides probability creation, conjunction, disjunction, and negation.
 * Values are clamped to [0.0, 1.0] by the native library.
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No probability logic is reimplemented in Java.
 */
public final class SafeProbability {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeProbability() {}

    /**
     * Create a probability value, clamped to [0.0, 1.0].
     *
     * @param value the raw probability value
     * @return the clamped probability
     */
    public static double create(double value) {
        return LIB.proven_probability_create(value);
    }

    /**
     * Probability of independent events both occurring (P(A) * P(B)).
     *
     * @param a probability of event A
     * @param b probability of event B
     * @return the joint probability
     */
    public static double and(double a, double b) {
        return LIB.proven_probability_and(a, b);
    }

    /**
     * Probability of exactly one event occurring (exclusive or).
     *
     * @param a probability of event A
     * @param b probability of event B
     * @return the exclusive-or probability
     */
    public static double orExclusive(double a, double b) {
        return LIB.proven_probability_or_exclusive(a, b);
    }

    /**
     * Complement probability (1 - P).
     *
     * @param p the probability
     * @return the complement
     */
    public static double not(double p) {
        return LIB.proven_probability_not(p);
    }
}
