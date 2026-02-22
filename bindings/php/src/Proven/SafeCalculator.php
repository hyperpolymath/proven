<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCalculator - FFI wrapper for proven_calculator_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe expression evaluator via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeCalculator
{
    /**
     * Evaluate a mathematical expression string.
     *
     * @param string $expression The expression to evaluate (e.g. "2 + 3 * 4").
     * @return float|null The result, or null on parse/eval error.
     */
    public static function eval(string $expression): ?float
    {
        $result = FFI::getLib()->proven_calculator_eval($expression, strlen($expression));
        if ($result->status !== 0) {
            return null;
        }
        return (float) $result->value;
    }
}
