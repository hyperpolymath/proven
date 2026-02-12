// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Safe arithmetic operations that cannot crash or overflow unexpectedly.
 */
object SafeMath {
    /**
     * Safely divide two integers, returning null on division by zero.
     */
    fun div(numerator: Int, denominator: Int): Int? =
        if (denominator == 0) null else numerator / denominator

    /**
     * Safely divide two longs, returning null on division by zero.
     */
    fun div(numerator: Long, denominator: Long): Long? =
        if (denominator == 0L) null else numerator / denominator

    /**
     * Safely compute modulo, returning null on division by zero.
     */
    fun mod(numerator: Int, denominator: Int): Int? =
        if (denominator == 0) null else numerator % denominator

    /**
     * Safely compute modulo for longs, returning null on division by zero.
     */
    fun mod(numerator: Long, denominator: Long): Long? =
        if (denominator == 0L) null else numerator % denominator

    /**
     * Safely add two integers, returning null on overflow.
     */
    fun add(a: Int, b: Int): Int? {
        val result = a.toLong() + b.toLong()
        return if (result > Int.MAX_VALUE || result < Int.MIN_VALUE) null else result.toInt()
    }

    /**
     * Safely add two longs, returning null on overflow.
     */
    fun add(a: Long, b: Long): Long? = try {
        Math.addExact(a, b)
    } catch (e: ArithmeticException) {
        null
    }

    /**
     * Safely subtract two integers, returning null on overflow.
     */
    fun sub(a: Int, b: Int): Int? {
        val result = a.toLong() - b.toLong()
        return if (result > Int.MAX_VALUE || result < Int.MIN_VALUE) null else result.toInt()
    }

    /**
     * Safely subtract two longs, returning null on overflow.
     */
    fun sub(a: Long, b: Long): Long? = try {
        Math.subtractExact(a, b)
    } catch (e: ArithmeticException) {
        null
    }

    /**
     * Safely multiply two integers, returning null on overflow.
     */
    fun mul(a: Int, b: Int): Int? = try {
        Math.multiplyExact(a, b)
    } catch (e: ArithmeticException) {
        null
    }

    /**
     * Safely multiply two longs, returning null on overflow.
     */
    fun mul(a: Long, b: Long): Long? = try {
        Math.multiplyExact(a, b)
    } catch (e: ArithmeticException) {
        null
    }

    /**
     * Result type for checked operations.
     */
    sealed class MathResult<out T> {
        data class Success<T>(val value: T) : MathResult<T>()
        data object Overflow : MathResult<Nothing>()
        data object DivisionByZero : MathResult<Nothing>()
    }

    /**
     * Add with overflow checking, returning a result type.
     */
    fun checkedAdd(a: Long, b: Long): MathResult<Long> = try {
        MathResult.Success(Math.addExact(a, b))
    } catch (e: ArithmeticException) {
        MathResult.Overflow
    }

    /**
     * Subtract with overflow checking, returning a result type.
     */
    fun checkedSub(a: Long, b: Long): MathResult<Long> = try {
        MathResult.Success(Math.subtractExact(a, b))
    } catch (e: ArithmeticException) {
        MathResult.Overflow
    }

    /**
     * Multiply with overflow checking, returning a result type.
     */
    fun checkedMul(a: Long, b: Long): MathResult<Long> = try {
        MathResult.Success(Math.multiplyExact(a, b))
    } catch (e: ArithmeticException) {
        MathResult.Overflow
    }
}
