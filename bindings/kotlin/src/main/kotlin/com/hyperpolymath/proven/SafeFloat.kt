// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe floating-point wrapper that prevents NaN and Infinity.
 */
@JvmInline
value class SafeFloat private constructor(val value: Double) : Comparable<SafeFloat> {

    override fun compareTo(other: SafeFloat): Int = value.compareTo(other.value)

    operator fun plus(other: SafeFloat): Result<SafeFloat> = create(value + other.value)
    operator fun minus(other: SafeFloat): Result<SafeFloat> = create(value - other.value)
    operator fun times(other: SafeFloat): Result<SafeFloat> = create(value * other.value)
    operator fun div(other: SafeFloat): Result<SafeFloat> {
        if (other.value == 0.0) return Result.failure(ArithmeticException("Division by zero"))
        return create(value / other.value)
    }

    operator fun unaryMinus(): SafeFloat = SafeFloat(-value)

    fun abs(): SafeFloat = SafeFloat(kotlin.math.abs(value))

    fun sqrt(): Result<SafeFloat> {
        if (value < 0) return Result.failure(ArithmeticException("Cannot take square root of negative number"))
        return create(kotlin.math.sqrt(value))
    }

    fun pow(exponent: SafeFloat): Result<SafeFloat> = create(kotlin.math.pow(value, exponent.value))

    fun floor(): SafeFloat = SafeFloat(kotlin.math.floor(value))
    fun ceil(): SafeFloat = SafeFloat(kotlin.math.ceil(value))
    fun round(): SafeFloat = SafeFloat(kotlin.math.round(value).toDouble())

    fun toInt(): Int = value.toInt()
    fun toLong(): Long = value.toLong()
    fun toFloat(): Float = value.toFloat()

    override fun toString(): String = value.toString()

    companion object {
        val ZERO = SafeFloat(0.0)
        val ONE = SafeFloat(1.0)
        val NEG_ONE = SafeFloat(-1.0)
        val MAX = SafeFloat(Double.MAX_VALUE)
        val MIN = SafeFloat(-Double.MAX_VALUE)
        val MIN_POSITIVE = SafeFloat(Double.MIN_VALUE)

        /**
         * Create a SafeFloat, returning failure if value is NaN or Infinity.
         */
        fun create(value: Double): Result<SafeFloat> {
            return when {
                value.isNaN() -> Result.failure(ArithmeticException("NaN is not allowed"))
                value.isInfinite() -> Result.failure(ArithmeticException("Infinity is not allowed"))
                else -> Result.success(SafeFloat(value))
            }
        }

        /**
         * Create a SafeFloat, clamping infinite values and replacing NaN with default.
         */
        fun createClamped(value: Double, default: Double = 0.0): SafeFloat {
            return when {
                value.isNaN() -> SafeFloat(default)
                value == Double.POSITIVE_INFINITY -> MAX
                value == Double.NEGATIVE_INFINITY -> MIN
                else -> SafeFloat(value)
            }
        }

        /**
         * Create from Int.
         */
        fun fromInt(value: Int): SafeFloat = SafeFloat(value.toDouble())

        /**
         * Create from Long.
         */
        fun fromLong(value: Long): SafeFloat = SafeFloat(value.toDouble())
    }
}

/**
 * Safe float utilities.
 */
object SafeFloatOps {
    /**
     * Safe division.
     */
    fun div(a: Double, b: Double): Result<SafeFloat> {
        if (b == 0.0) return Result.failure(ArithmeticException("Division by zero"))
        return SafeFloat.create(a / b)
    }

    /**
     * Safe square root.
     */
    fun sqrt(value: Double): Result<SafeFloat> {
        if (value < 0) return Result.failure(ArithmeticException("Cannot take square root of negative number"))
        return SafeFloat.create(kotlin.math.sqrt(value))
    }

    /**
     * Safe natural logarithm.
     */
    fun ln(value: Double): Result<SafeFloat> {
        if (value <= 0) return Result.failure(ArithmeticException("Logarithm requires positive input"))
        return SafeFloat.create(kotlin.math.ln(value))
    }

    /**
     * Safe log base 10.
     */
    fun log10(value: Double): Result<SafeFloat> {
        if (value <= 0) return Result.failure(ArithmeticException("Logarithm requires positive input"))
        return SafeFloat.create(kotlin.math.log10(value))
    }

    /**
     * Safe power.
     */
    fun pow(base: Double, exponent: Double): Result<SafeFloat> {
        return SafeFloat.create(kotlin.math.pow(base, exponent))
    }

    /**
     * Check if approximately equal within epsilon.
     */
    fun approxEqual(a: Double, b: Double, epsilon: Double = 1e-10): Boolean {
        return kotlin.math.abs(a - b) < epsilon
    }

    /**
     * Clamp value to range.
     */
    fun clamp(value: Double, min: Double, max: Double): Double {
        return kotlin.math.max(min, kotlin.math.min(max, value))
    }

    /**
     * Linear interpolation.
     */
    fun lerp(a: Double, b: Double, t: Double): Double {
        return a + (b - a) * clamp(t, 0.0, 1.0)
    }

    /**
     * Map value from one range to another.
     */
    fun map(value: Double, inMin: Double, inMax: Double, outMin: Double, outMax: Double): Result<Double> {
        if (inMax == inMin) return Result.failure(ArithmeticException("Input range cannot be zero"))
        val t = (value - inMin) / (inMax - inMin)
        return Result.success(outMin + (outMax - outMin) * t)
    }
}
