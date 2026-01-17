<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Angle in degrees with safe operations.
 */
readonly class Degrees implements \Stringable
{
    public function __construct(
        public float $value,
    ) {}

    /**
     * Convert to radians.
     */
    public function toRadians(): Radians
    {
        return new Radians($this->value * M_PI / 180.0);
    }

    /**
     * Normalize to [0, 360).
     */
    public function normalize(): self
    {
        $normalized = fmod($this->value, 360.0);
        if ($normalized < 0) {
            $normalized += 360.0;
        }
        return new self($normalized);
    }

    /**
     * Normalize to [-180, 180).
     */
    public function normalizeSigned(): self
    {
        $normalized = fmod($this->value + 180.0, 360.0);
        if ($normalized < 0) {
            $normalized += 360.0;
        }
        return new self($normalized - 180.0);
    }

    /**
     * Add another angle.
     */
    public function add(self $other): self
    {
        return new self($this->value + $other->value);
    }

    /**
     * Subtract another angle.
     */
    public function subtract(self $other): self
    {
        return new self($this->value - $other->value);
    }

    /**
     * Get the shortest difference to another angle.
     */
    public function shortestDifferenceTo(self $other): self
    {
        $diff = fmod($other->value - $this->value + 180.0, 360.0);
        if ($diff < 0) {
            $diff += 360.0;
        }
        return new self($diff - 180.0);
    }

    /**
     * Get sine.
     */
    public function sin(): float
    {
        return sin(deg2rad($this->value));
    }

    /**
     * Get cosine.
     */
    public function cos(): float
    {
        return cos(deg2rad($this->value));
    }

    /**
     * Get tangent.
     *
     * @return Option<float>
     */
    public function tan(): Option
    {
        $cos = $this->cos();
        if (abs($cos) < 1e-10) {
            return Option::none();
        }
        return Option::some(tan(deg2rad($this->value)));
    }

    public function __toString(): string
    {
        return sprintf('%.2f deg', $this->value);
    }
}

/**
 * Angle in radians with safe operations.
 */
readonly class Radians implements \Stringable
{
    public function __construct(
        public float $value,
    ) {}

    /**
     * Convert to degrees.
     */
    public function toDegrees(): Degrees
    {
        return new Degrees($this->value * 180.0 / M_PI);
    }

    /**
     * Normalize to [0, 2*PI).
     */
    public function normalize(): self
    {
        $normalized = fmod($this->value, 2 * M_PI);
        if ($normalized < 0) {
            $normalized += 2 * M_PI;
        }
        return new self($normalized);
    }

    /**
     * Normalize to [-PI, PI).
     */
    public function normalizeSigned(): self
    {
        $normalized = fmod($this->value + M_PI, 2 * M_PI);
        if ($normalized < 0) {
            $normalized += 2 * M_PI;
        }
        return new self($normalized - M_PI);
    }

    /**
     * Add another angle.
     */
    public function add(self $other): self
    {
        return new self($this->value + $other->value);
    }

    /**
     * Subtract another angle.
     */
    public function subtract(self $other): self
    {
        return new self($this->value - $other->value);
    }

    /**
     * Get sine.
     */
    public function sin(): float
    {
        return sin($this->value);
    }

    /**
     * Get cosine.
     */
    public function cos(): float
    {
        return cos($this->value);
    }

    /**
     * Get tangent.
     *
     * @return Option<float>
     */
    public function tan(): Option
    {
        $cos = $this->cos();
        if (abs($cos) < 1e-10) {
            return Option::none();
        }
        return Option::some(tan($this->value));
    }

    public function __toString(): string
    {
        return sprintf('%.4f rad', $this->value);
    }
}

/**
 * Safe angle operations.
 */
class SafeAngle
{
    /**
     * Convert degrees to radians.
     */
    public static function degToRad(float $degrees): float
    {
        return $degrees * M_PI / 180.0;
    }

    /**
     * Convert radians to degrees.
     */
    public static function radToDeg(float $radians): float
    {
        return $radians * 180.0 / M_PI;
    }

    /**
     * Normalize degrees to [0, 360).
     */
    public static function normalizeDegrees(float $degrees): float
    {
        $normalized = fmod($degrees, 360.0);
        if ($normalized < 0) {
            $normalized += 360.0;
        }
        return $normalized;
    }

    /**
     * Normalize radians to [0, 2*PI).
     */
    public static function normalizeRadians(float $radians): float
    {
        $normalized = fmod($radians, 2 * M_PI);
        if ($normalized < 0) {
            $normalized += 2 * M_PI;
        }
        return $normalized;
    }

    /**
     * Linear interpolation between two angles (in degrees).
     * Takes the shortest path around the circle.
     *
     * @param float $t Interpolation factor (0-1)
     */
    public static function lerpAngleDegrees(float $from, float $to, float $t): float
    {
        $t = max(0, min(1, $t));
        $diff = fmod($to - $from + 540.0, 360.0) - 180.0;
        return self::normalizeDegrees($from + $diff * $t);
    }

    /**
     * Linear interpolation between two angles (in radians).
     * Takes the shortest path around the circle.
     *
     * @param float $t Interpolation factor (0-1)
     */
    public static function lerpAngleRadians(float $from, float $to, float $t): float
    {
        $t = max(0, min(1, $t));
        $diff = fmod($to - $from + 3 * M_PI, 2 * M_PI) - M_PI;
        return self::normalizeRadians($from + $diff * $t);
    }

    /**
     * Calculate the shortest angular distance in degrees.
     */
    public static function angularDistanceDegrees(float $from, float $to): float
    {
        $diff = fmod($to - $from + 180.0, 360.0) - 180.0;
        return abs($diff);
    }

    /**
     * Calculate the shortest angular distance in radians.
     */
    public static function angularDistanceRadians(float $from, float $to): float
    {
        $diff = fmod($to - $from + M_PI, 2 * M_PI) - M_PI;
        return abs($diff);
    }

    /**
     * Safe arcsine that returns None for out-of-range inputs.
     *
     * @return Option<float> Radians
     */
    public static function asin(float $x): Option
    {
        if ($x < -1.0 || $x > 1.0) {
            return Option::none();
        }
        return Option::some(asin($x));
    }

    /**
     * Safe arccosine that returns None for out-of-range inputs.
     *
     * @return Option<float> Radians
     */
    public static function acos(float $x): Option
    {
        if ($x < -1.0 || $x > 1.0) {
            return Option::none();
        }
        return Option::some(acos($x));
    }

    /**
     * Safe arctangent (atan2 variant).
     */
    public static function atan2(float $y, float $x): float
    {
        return atan2($y, $x);
    }

    /**
     * Check if an angle (in degrees) is within a range.
     * Handles wrap-around correctly.
     */
    public static function isWithinDegrees(float $angle, float $start, float $end): bool
    {
        $angle = self::normalizeDegrees($angle);
        $start = self::normalizeDegrees($start);
        $end = self::normalizeDegrees($end);

        if ($start <= $end) {
            return $angle >= $start && $angle <= $end;
        }
        // Wrap-around case
        return $angle >= $start || $angle <= $end;
    }

    /**
     * Create degrees from a value.
     */
    public static function degrees(float $value): Degrees
    {
        return new Degrees($value);
    }

    /**
     * Create radians from a value.
     */
    public static function radians(float $value): Radians
    {
        return new Radians($value);
    }

    /**
     * Common angles in degrees.
     */
    public static function rightAngle(): Degrees { return new Degrees(90.0); }
    public static function straightAngle(): Degrees { return new Degrees(180.0); }
    public static function fullRotation(): Degrees { return new Degrees(360.0); }
}
