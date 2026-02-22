// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

import com.sun.jna.Memory;

import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Safe expression evaluator via libproven FFI.
 *
 * Evaluates mathematical expressions safely (no injection, bounded).
 * All logic delegates to the Idris 2 verified implementation through
 * the Zig C ABI. No expression parsing logic is reimplemented in Java.
 */
public final class SafeCalculator {

    private static final ProvenLibrary LIB = ProvenLibrary.INSTANCE;

    private SafeCalculator() {}

    /**
     * Evaluate a mathematical expression string.
     *
     * @param expression the expression to evaluate (e.g. "2 + 3 * 4")
     * @return the result, or empty on parse/eval error
     */
    public static Optional<Double> eval(String expression) {
        Memory mem = NativeUtil.toNativeString(expression);
        if (mem == null) {
            return Optional.empty();
        }
        byte[] bytes = expression.getBytes(StandardCharsets.UTF_8);
        return NativeUtil.extractDouble(
            LIB.proven_calculator_eval(mem, bytes.length));
    }
}
