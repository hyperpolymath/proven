<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeML - FFI wrapper for proven_ml_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Safe machine learning activation functions via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeML
{
    /**
     * Compute softmax over an array of values.
     *
     * @param float[] $input Array of input values.
     * @return float[]|null Array of softmax probabilities, or null on error.
     */
    public static function softmax(array $input): ?array
    {
        $len = count($input);
        if ($len === 0) {
            return null;
        }
        $inBuf = \FFI::new("double[$len]");
        $outBuf = \FFI::new("double[$len]");
        for ($i = 0; $i < $len; $i++) {
            $inBuf[$i] = (float) $input[$i];
        }
        $status = FFI::getLib()->proven_ml_softmax(
            \FFI::addr($inBuf[0]), \FFI::addr($outBuf[0]), $len
        );
        if ($status !== 0) {
            return null;
        }
        $output = [];
        for ($i = 0; $i < $len; $i++) {
            $output[] = (float) $outBuf[$i];
        }
        return $output;
    }

    /**
     * Sigmoid activation function: 1 / (1 + exp(-x)).
     */
    public static function sigmoid(float $x): float
    {
        return FFI::getLib()->proven_ml_sigmoid($x);
    }

    /**
     * ReLU activation function: max(0, x).
     */
    public static function relu(float $x): float
    {
        return FFI::getLib()->proven_ml_relu($x);
    }

    /**
     * Leaky ReLU activation function: x if x > 0, alpha * x otherwise.
     */
    public static function leakyRelu(float $x, float $alpha): float
    {
        return FFI::getLib()->proven_ml_leaky_relu($x, $alpha);
    }

    /**
     * Clamp a value to a range.
     */
    public static function clamp(float $x, float $minVal, float $maxVal): float
    {
        return FFI::getLib()->proven_ml_clamp($x, $minVal, $maxVal);
    }
}
