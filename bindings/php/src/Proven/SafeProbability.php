<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeProbability - FFI wrapper for proven_probability_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe probability operations via libproven FFI.
 *
 * All values are clamped to [0.0, 1.0] by the verified implementation.
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeProbability
{
    /**
     * Create a valid probability (clamped to [0, 1]).
     */
    public static function create(float $value): float
    {
        return FFI::getLib()->proven_probability_create($value);
    }

    /**
     * Probability of A AND B (independent events): P(A) * P(B).
     */
    public static function and(float $a, float $b): float
    {
        return FFI::getLib()->proven_probability_and($a, $b);
    }

    /**
     * Probability of A XOR B (exclusive or): P(A) + P(B) - 2*P(A)*P(B).
     */
    public static function orExclusive(float $a, float $b): float
    {
        return FFI::getLib()->proven_probability_or_exclusive($a, $b);
    }

    /**
     * Complement probability: 1 - P.
     */
    public static function not(float $p): float
    {
        return FFI::getLib()->proven_probability_not($p);
    }
}
