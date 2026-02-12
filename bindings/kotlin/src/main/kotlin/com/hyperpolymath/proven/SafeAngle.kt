// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.*

/**
 * Angle in degrees.
 */
@JvmInline
value class Degrees(val value: Double) : Comparable<Degrees> {
    override fun compareTo(other: Degrees): Int = value.compareTo(other.value)

    val normalized: Degrees
        get() {
            var v = value % 360
            if (v < 0) v += 360
            return Degrees(v)
        }

    val normalizedSigned: Degrees
        get() {
            var v = value % 360
            if (v < 0) v += 360
            if (v >= 180) v -= 360
            return Degrees(v)
        }

    val toRadians: Radians get() = Radians(value * PI / 180)

    operator fun plus(other: Degrees): Degrees = Degrees(value + other.value)
    operator fun minus(other: Degrees): Degrees = Degrees(value - other.value)
    operator fun times(scalar: Double): Degrees = Degrees(value * scalar)
    operator fun unaryMinus(): Degrees = Degrees(-value)

    companion object {
        val ZERO = Degrees(0.0)
        val RIGHT = Degrees(90.0)
        val STRAIGHT = Degrees(180.0)
        val FULL = Degrees(360.0)
    }
}

/**
 * Angle in radians.
 */
@JvmInline
value class Radians(val value: Double) : Comparable<Radians> {
    override fun compareTo(other: Radians): Int = value.compareTo(other.value)

    val normalized: Radians
        get() {
            var v = value % (2 * PI)
            if (v < 0) v += 2 * PI
            return Radians(v)
        }

    val normalizedSigned: Radians
        get() {
            var v = value % (2 * PI)
            if (v < 0) v += 2 * PI
            if (v >= PI) v -= 2 * PI
            return Radians(v)
        }

    val toDegrees: Degrees get() = Degrees(value * 180 / PI)

    operator fun plus(other: Radians): Radians = Radians(value + other.value)
    operator fun minus(other: Radians): Radians = Radians(value - other.value)
    operator fun times(scalar: Double): Radians = Radians(value * scalar)
    operator fun unaryMinus(): Radians = Radians(-value)

    companion object {
        val ZERO = Radians(0.0)
        val PI_OVER_6 = Radians(PI / 6)
        val PI_OVER_4 = Radians(PI / 4)
        val PI_OVER_3 = Radians(PI / 3)
        val PI_OVER_2 = Radians(PI / 2)
        val PI_RAD = Radians(PI)
        val TWO_PI = Radians(2 * PI)
    }
}

/**
 * Angle utilities.
 */
object SafeAngle {
    /**
     * Calculate smallest angle between two angles.
     */
    fun angleBetween(a: Degrees, b: Degrees): Degrees {
        val diff = abs((a.value - b.value) % 360)
        return Degrees(if (diff > 180) 360 - diff else diff)
    }

    /**
     * Linear interpolation between angles (taking shortest path).
     */
    fun lerp(from: Degrees, to: Degrees, t: Double): Degrees {
        val tt = t.coerceIn(0.0, 1.0)
        var diff = (to.value - from.value) % 360
        if (diff > 180) diff -= 360
        if (diff < -180) diff += 360
        return Degrees(from.value + diff * tt).normalized
    }

    /**
     * Check if angle is in range [start, end] (going clockwise).
     */
    fun isInRange(angle: Degrees, start: Degrees, end: Degrees): Boolean {
        val a = angle.normalized.value
        val s = start.normalized.value
        val e = end.normalized.value

        return if (s <= e) a in s..e else a >= s || a <= e
    }

    /**
     * Convert compass bearing to mathematical angle.
     */
    fun bearingToAngle(bearing: Degrees): Degrees = Degrees(90 - bearing.value).normalized

    /**
     * Convert mathematical angle to compass bearing.
     */
    fun angleToBearing(angle: Degrees): Degrees = Degrees(90 - angle.value).normalized

    // Trigonometric functions with degrees
    fun sin(angle: Degrees): Double = kotlin.math.sin(angle.toRadians.value)
    fun cos(angle: Degrees): Double = kotlin.math.cos(angle.toRadians.value)
    fun tan(angle: Degrees): Double = kotlin.math.tan(angle.toRadians.value)

    // Inverse trigonometric functions returning degrees
    fun asin(value: Double): Degrees? {
        if (value !in -1.0..1.0) return null
        return Radians(kotlin.math.asin(value)).toDegrees
    }

    fun acos(value: Double): Degrees? {
        if (value !in -1.0..1.0) return null
        return Radians(kotlin.math.acos(value)).toDegrees
    }

    fun atan(value: Double): Degrees = Radians(kotlin.math.atan(value)).toDegrees

    fun atan2(y: Double, x: Double): Degrees = Radians(kotlin.math.atan2(y, x)).toDegrees
}
