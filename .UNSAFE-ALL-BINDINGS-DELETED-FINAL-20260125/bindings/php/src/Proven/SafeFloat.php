<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Safe floating-point operations with NaN/Infinity prevention.
 *
 * Unlike integers which can overflow, floating-point numbers silently produce
 * NaN or Infinity on edge cases. This module makes those edge cases explicit
 * through Result types.
 */
class SafeFloat
{
    /** Minimum positive value to consider non-zero (prevents denormal issues) */
    public const EPSILON = 1e-10;

    /**
     * Safe division with zero check.
     *
     * @return Result<float>
     */
    public static function div(float $a, float $b): Result
    {
        if (abs($b) < self::EPSILON) {
            return Result::err(ProvenError::divisionByZero());
        }

        $result = $a / $b;
        if (!is_finite($result)) {
            return Result::err(ProvenError::overflow("$a / $b is not finite"));
        }

        return Result::ok($result);
    }

    /**
     * Safe natural logarithm (ln).
     *
     * @return Result<float>
     */
    public static function ln(float $x): Result
    {
        if ($x <= 0.0) {
            return Result::err(ProvenError::validationError("ln($x) undefined for non-positive values"));
        }
        return Result::ok(log($x));
    }

    /**
     * Safe log base 10.
     *
     * @return Result<float>
     */
    public static function log10(float $x): Result
    {
        if ($x <= 0.0) {
            return Result::err(ProvenError::validationError("log10($x) undefined for non-positive values"));
        }
        return Result::ok(log10($x));
    }

    /**
     * Safe log with arbitrary base.
     *
     * @return Result<float>
     */
    public static function log(float $x, float $base): Result
    {
        if ($x <= 0.0) {
            return Result::err(ProvenError::validationError("log($x) undefined for non-positive values"));
        }
        if ($base <= 0.0 || $base === 1.0) {
            return Result::err(ProvenError::validationError("log base $base is invalid"));
        }
        return Result::ok(log($x, $base));
    }

    /**
     * Safe square root.
     *
     * @return Result<float>
     */
    public static function sqrt(float $x): Result
    {
        if ($x < 0.0) {
            return Result::err(ProvenError::validationError("sqrt($x) undefined for negative values"));
        }
        return Result::ok(sqrt($x));
    }

    /**
     * Safe power operation.
     *
     * @return Result<float>
     */
    public static function pow(float $base, float $exp): Result
    {
        $result = pow($base, $exp);
        if (!is_finite($result)) {
            return Result::err(ProvenError::overflow("$base^$exp is not finite"));
        }
        return Result::ok($result);
    }

    /**
     * Safe exponential (e^x).
     *
     * @return Result<float>
     */
    public static function exp(float $x): Result
    {
        $result = exp($x);
        if (!is_finite($result)) {
            return Result::err(ProvenError::overflow("exp($x) overflows"));
        }
        return Result::ok($result);
    }

    /**
     * Compute vector magnitude (L2 norm).
     *
     * @param array<float> $v
     */
    public static function magnitude(array $v): float
    {
        $sum = 0.0;
        foreach ($v as $x) {
            $sum += $x * $x;
        }
        return sqrt($sum);
    }

    /**
     * Safe vector normalization (unit vector).
     *
     * @param array<float> $v
     * @return Result<array<float>>
     */
    public static function normalize(array $v): Result
    {
        $mag = self::magnitude($v);
        if ($mag < self::EPSILON) {
            return Result::err(ProvenError::divisionByZero());
        }

        $result = [];
        foreach ($v as $x) {
            $result[] = $x / $mag;
        }
        return Result::ok($result);
    }

    /**
     * Check if a float is finite (not NaN or Infinity).
     */
    public static function isFinite(float $x): bool
    {
        return is_finite($x);
    }

    /**
     * Check if a float is NaN.
     */
    public static function isNaN(float $x): bool
    {
        return is_nan($x);
    }

    /**
     * Check if a float is infinite.
     */
    public static function isInfinite(float $x): bool
    {
        return is_infinite($x);
    }

    /**
     * Check if a float is safe for division (non-zero and finite).
     */
    public static function isSafeDivisor(float $x): bool
    {
        return is_finite($x) && abs($x) >= self::EPSILON;
    }

    /**
     * Clamp a float to a range, handling NaN by returning min.
     */
    public static function clamp(float $value, float $min, float $max): float
    {
        if (is_nan($value)) {
            return $min;
        }
        if ($value < $min) {
            return $min;
        }
        if ($value > $max) {
            return $max;
        }
        return $value;
    }

    /**
     * Safe reciprocal (1/x).
     *
     * @return Result<float>
     */
    public static function reciprocal(float $x): Result
    {
        return self::div(1.0, $x);
    }

    /**
     * Compute mean of a vector safely.
     *
     * @param array<float> $v
     * @return Result<float>
     */
    public static function mean(array $v): Result
    {
        if (empty($v)) {
            return Result::err(ProvenError::emptyInput());
        }
        $sum = array_sum($v);
        return self::div($sum, (float)count($v));
    }

    /**
     * Compute variance safely.
     *
     * @param array<float> $v
     * @return Result<float>
     */
    public static function variance(array $v): Result
    {
        $meanResult = self::mean($v);
        if ($meanResult->isErr()) {
            return $meanResult;
        }
        $mean = $meanResult->unwrap();

        $sumSq = 0.0;
        foreach ($v as $x) {
            $sumSq += ($x - $mean) ** 2;
        }

        return self::div($sumSq, (float)count($v));
    }

    /**
     * Compute standard deviation safely.
     *
     * @param array<float> $v
     * @return Result<float>
     */
    public static function stdDev(array $v): Result
    {
        $varResult = self::variance($v);
        if ($varResult->isErr()) {
            return $varResult;
        }
        return self::sqrt($varResult->unwrap());
    }

    /**
     * Compute dot product of two vectors.
     *
     * @param array<float> $a
     * @param array<float> $b
     * @return Result<float>
     */
    public static function dotProduct(array $a, array $b): Result
    {
        if (count($a) !== count($b)) {
            return Result::err(ProvenError::validationError('Vectors must have same length'));
        }

        $sum = 0.0;
        foreach ($a as $i => $x) {
            $sum += $x * $b[$i];
        }

        if (!is_finite($sum)) {
            return Result::err(ProvenError::overflow('Dot product overflow'));
        }

        return Result::ok($sum);
    }

    /**
     * Linearly interpolate between two values.
     */
    public static function lerp(float $a, float $b, float $t): float
    {
        return $a + ($b - $a) * self::clamp($t, 0.0, 1.0);
    }

    /**
     * Compute cosine similarity between two vectors.
     *
     * @param array<float> $a
     * @param array<float> $b
     * @return Result<float>
     */
    public static function cosineSimilarity(array $a, array $b): Result
    {
        $dotResult = self::dotProduct($a, $b);
        if ($dotResult->isErr()) {
            return $dotResult;
        }
        $dot = $dotResult->unwrap();

        $magA = self::magnitude($a);
        $magB = self::magnitude($b);

        if ($magA < self::EPSILON || $magB < self::EPSILON) {
            return Result::err(ProvenError::divisionByZero());
        }

        return Result::ok($dot / ($magA * $magB));
    }

    /**
     * Safe inverse (wrap NaN to None).
     *
     * @return Option<float>
     */
    public static function safeInverse(float $x): Option
    {
        if (abs($x) < self::EPSILON) {
            return Option::none();
        }
        $result = 1.0 / $x;
        if (!is_finite($result)) {
            return Option::none();
        }
        return Option::some($result);
    }

    /**
     * Replace NaN with a default value.
     */
    public static function nanOr(float $value, float $default): float
    {
        return is_nan($value) ? $default : $value;
    }

    /**
     * Replace Infinity with a default value.
     */
    public static function infiniteOr(float $value, float $default): float
    {
        return is_infinite($value) ? $default : $value;
    }
}
