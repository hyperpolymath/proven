// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.*

/**
 * Calculator error types.
 */
sealed class CalculatorError : Exception() {
    data object Overflow : CalculatorError()
    data object DivisionByZero : CalculatorError()
    data class InvalidInput(override val message: String) : CalculatorError()
    data class DomainError(override val message: String) : CalculatorError()
}

/**
 * Safe calculator with overflow protection.
 */
object SafeCalculator {
    /**
     * Safe addition.
     */
    fun add(a: Double, b: Double): Result<Double> {
        val result = a + b
        return if (result.isFinite()) Result.success(result)
        else Result.failure(CalculatorError.Overflow)
    }

    /**
     * Safe subtraction.
     */
    fun sub(a: Double, b: Double): Result<Double> {
        val result = a - b
        return if (result.isFinite()) Result.success(result)
        else Result.failure(CalculatorError.Overflow)
    }

    /**
     * Safe multiplication.
     */
    fun mul(a: Double, b: Double): Result<Double> {
        val result = a * b
        return if (result.isFinite()) Result.success(result)
        else Result.failure(CalculatorError.Overflow)
    }

    /**
     * Safe division.
     */
    fun div(a: Double, b: Double): Result<Double> {
        if (b == 0.0) return Result.failure(CalculatorError.DivisionByZero)
        val result = a / b
        return if (result.isFinite()) Result.success(result)
        else Result.failure(CalculatorError.Overflow)
    }

    /**
     * Safe modulo.
     */
    fun mod(a: Double, b: Double): Result<Double> {
        if (b == 0.0) return Result.failure(CalculatorError.DivisionByZero)
        return Result.success(a.mod(b))
    }

    /**
     * Safe power.
     */
    fun pow(base: Double, exponent: Double): Result<Double> {
        val result = base.pow(exponent)
        return if (result.isFinite()) Result.success(result)
        else Result.failure(CalculatorError.Overflow)
    }

    /**
     * Safe square root.
     */
    fun sqrt(value: Double): Result<Double> {
        if (value < 0) return Result.failure(CalculatorError.DomainError("Cannot take square root of negative number"))
        return Result.success(kotlin.math.sqrt(value))
    }

    /**
     * Safe natural logarithm.
     */
    fun ln(value: Double): Result<Double> {
        if (value <= 0) return Result.failure(CalculatorError.DomainError("Logarithm requires positive input"))
        return Result.success(kotlin.math.ln(value))
    }

    /**
     * Safe log base 10.
     */
    fun log10(value: Double): Result<Double> {
        if (value <= 0) return Result.failure(CalculatorError.DomainError("Logarithm requires positive input"))
        return Result.success(kotlin.math.log10(value))
    }

    /**
     * Safe log with arbitrary base.
     */
    fun log(value: Double, base: Double): Result<Double> {
        if (value <= 0) return Result.failure(CalculatorError.DomainError("Logarithm requires positive input"))
        if (base <= 0 || base == 1.0) return Result.failure(CalculatorError.DomainError("Invalid logarithm base"))
        return Result.success(kotlin.math.ln(value) / kotlin.math.ln(base))
    }

    /**
     * Safe factorial (for small integers).
     */
    fun factorial(n: Int): Result<Double> {
        if (n < 0) return Result.failure(CalculatorError.InvalidInput("Factorial requires non-negative integer"))
        if (n > 170) return Result.failure(CalculatorError.Overflow)

        var result = 1.0
        for (i in 2..n) {
            result *= i
        }
        return Result.success(result)
    }

    /**
     * Safe permutation P(n, r).
     */
    fun permutation(n: Int, r: Int): Result<Double> {
        if (n < 0 || r < 0) return Result.failure(CalculatorError.InvalidInput("Permutation requires non-negative integers"))
        if (r > n) return Result.failure(CalculatorError.InvalidInput("r cannot be greater than n"))

        var result = 1.0
        for (i in (n - r + 1)..n) {
            result *= i
            if (!result.isFinite()) return Result.failure(CalculatorError.Overflow)
        }
        return Result.success(result)
    }

    /**
     * Safe combination C(n, r).
     */
    fun combination(n: Int, r: Int): Result<Double> {
        if (n < 0 || r < 0) return Result.failure(CalculatorError.InvalidInput("Combination requires non-negative integers"))
        if (r > n) return Result.failure(CalculatorError.InvalidInput("r cannot be greater than n"))

        var actualR = r
        if (actualR > n - actualR) actualR = n - actualR

        var result = 1.0
        for (i in 0 until actualR) {
            result = result * (n - i) / (i + 1)
            if (!result.isFinite()) return Result.failure(CalculatorError.Overflow)
        }
        return Result.success(round(result))
    }

    /**
     * GCD using Euclidean algorithm.
     */
    fun gcd(a: Int, b: Int): Int {
        var x = abs(a)
        var y = abs(b)
        while (y != 0) {
            val temp = y
            y = x % y
            x = temp
        }
        return x
    }

    /**
     * LCM.
     */
    fun lcm(a: Int, b: Int): Result<Long> {
        val g = gcd(a, b)
        if (g == 0) return Result.success(0)

        val result = abs(a.toLong()) * (abs(b) / g)
        return Result.success(result)
    }

    /**
     * Check if prime.
     */
    fun isPrime(n: Int): Boolean {
        if (n < 2) return false
        if (n == 2) return true
        if (n % 2 == 0) return false

        val sqrtN = kotlin.math.sqrt(n.toDouble()).toInt()
        for (i in 3..sqrtN step 2) {
            if (n % i == 0) return false
        }
        return true
    }

    /**
     * Clamp value to range.
     */
    fun clamp(value: Double, min: Double, max: Double): Double {
        return maxOf(min, minOf(max, value))
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
        if (inMax == inMin) return Result.failure(CalculatorError.InvalidInput("Input range cannot be zero"))
        val t = (value - inMin) / (inMax - inMin)
        return Result.success(outMin + (outMax - outMin) * t)
    }
}

/**
 * Expression parser for evaluating mathematical expressions.
 */
class ExpressionParser {
    private val constants = mutableMapOf(
        "pi" to PI,
        "e" to E,
        "phi" to (1 + kotlin.math.sqrt(5.0)) / 2,
        "tau" to PI * 2
    )

    /**
     * Register a custom constant.
     */
    fun addConstant(name: String, value: Double) {
        constants[name.lowercase()] = value
    }

    /**
     * Evaluate a simple expression.
     */
    fun evaluate(expr: String): Result<Double> {
        val cleaned = expr.replace(" ", "").lowercase()

        // Try as number
        cleaned.toDoubleOrNull()?.let { return Result.success(it) }

        // Try as constant
        constants[cleaned]?.let { return Result.success(it) }

        // Handle basic operators
        return parseExpression(cleaned)
    }

    private fun parseExpression(expr: String): Result<Double> {
        // Handle addition/subtraction
        expr.lastIndexOf('+').takeIf { it > 0 }?.let { idx ->
            val left = evaluate(expr.substring(0, idx)).getOrElse { return Result.failure(it) }
            val right = evaluate(expr.substring(idx + 1)).getOrElse { return Result.failure(it) }
            return SafeCalculator.add(left, right)
        }

        // Handle subtraction (but not negative numbers)
        for (i in expr.lastIndex downTo 1) {
            if (expr[i] == '-' && i > 0 && expr[i - 1] != 'e') {
                val left = evaluate(expr.substring(0, i)).getOrElse { return Result.failure(it) }
                val right = evaluate(expr.substring(i + 1)).getOrElse { return Result.failure(it) }
                return SafeCalculator.sub(left, right)
            }
        }

        // Handle multiplication
        expr.lastIndexOf('*').takeIf { it > 0 }?.let { idx ->
            val left = evaluate(expr.substring(0, idx)).getOrElse { return Result.failure(it) }
            val right = evaluate(expr.substring(idx + 1)).getOrElse { return Result.failure(it) }
            return SafeCalculator.mul(left, right)
        }

        // Handle division
        expr.lastIndexOf('/').takeIf { it > 0 }?.let { idx ->
            val left = evaluate(expr.substring(0, idx)).getOrElse { return Result.failure(it) }
            val right = evaluate(expr.substring(idx + 1)).getOrElse { return Result.failure(it) }
            return SafeCalculator.div(left, right)
        }

        return Result.failure(CalculatorError.InvalidInput("Could not parse expression: $expr"))
    }
}
