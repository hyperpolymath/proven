// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe angle operations with unit conversions and normalization.
 * Provides validated angle types in degrees, radians, and gradians.
 */
module proven.safe_angle;

import std.math : PI, sin, cos, tan, asin, acos, atan, atan2, abs, isNaN;
import std.typecons : Nullable, nullable;

/// Angle represented internally in radians.
struct Angle
{
    /// Value in radians.
    double radians;

    /// Create angle from radians.
    static Angle fromRadians(double rad) pure nothrow @safe @nogc
    {
        return Angle(rad);
    }

    /// Create angle from degrees.
    static Angle fromDegrees(double deg) pure nothrow @safe @nogc
    {
        return Angle(deg * PI / 180.0);
    }

    /// Create angle from gradians.
    static Angle fromGradians(double grad) pure nothrow @safe @nogc
    {
        return Angle(grad * PI / 200.0);
    }

    /// Create angle from turns (1 turn = 360 degrees).
    static Angle fromTurns(double turns) pure nothrow @safe @nogc
    {
        return Angle(turns * 2.0 * PI);
    }

    /// Convert to degrees.
    double toDegrees() const pure nothrow @safe @nogc
    {
        return radians * 180.0 / PI;
    }

    /// Convert to gradians.
    double toGradians() const pure nothrow @safe @nogc
    {
        return radians * 200.0 / PI;
    }

    /// Convert to turns.
    double toTurns() const pure nothrow @safe @nogc
    {
        return radians / (2.0 * PI);
    }

    /// Normalize to [0, 2*PI) range.
    Angle normalize() const pure nothrow @safe @nogc
    {
        double normalized = radians;
        immutable twoPi = 2.0 * PI;
        while (normalized < 0)
            normalized += twoPi;
        while (normalized >= twoPi)
            normalized -= twoPi;
        return Angle(normalized);
    }

    /// Normalize to [-PI, PI) range.
    Angle normalizeSymmetric() const pure nothrow @safe @nogc
    {
        double normalized = radians;
        immutable twoPi = 2.0 * PI;
        while (normalized < -PI)
            normalized += twoPi;
        while (normalized >= PI)
            normalized -= twoPi;
        return Angle(normalized);
    }

    /// Calculate sine.
    double sin() const pure nothrow @safe @nogc
    {
        return .sin(radians);
    }

    /// Calculate cosine.
    double cos() const pure nothrow @safe @nogc
    {
        return .cos(radians);
    }

    /// Calculate tangent.
    Nullable!double tan() const pure nothrow @safe @nogc
    {
        // Check for values near PI/2 or 3*PI/2
        immutable normalized = normalize().radians;
        immutable halfPi = PI / 2.0;
        if (abs(normalized - halfPi) < 1e-10 || abs(normalized - 3.0 * halfPi) < 1e-10)
            return Nullable!double.init;
        return nullable(.tan(radians));
    }

    /// Addition.
    Angle opBinary(string op : "+")(Angle other) const pure nothrow @safe @nogc
    {
        return Angle(radians + other.radians);
    }

    /// Subtraction.
    Angle opBinary(string op : "-")(Angle other) const pure nothrow @safe @nogc
    {
        return Angle(radians - other.radians);
    }

    /// Scalar multiplication.
    Angle opBinary(string op : "*")(double scalar) const pure nothrow @safe @nogc
    {
        return Angle(radians * scalar);
    }

    /// Scalar division.
    Nullable!Angle opBinary(string op : "/")(double scalar) const pure nothrow @safe @nogc
    {
        if (abs(scalar) < 1e-10)
            return Nullable!Angle.init;
        return nullable(Angle(radians / scalar));
    }

    /// Negation.
    Angle opUnary(string op : "-")() const pure nothrow @safe @nogc
    {
        return Angle(-radians);
    }

    /// Comparison.
    int opCmp(const Angle other) const pure nothrow @safe @nogc
    {
        if (radians < other.radians)
            return -1;
        if (radians > other.radians)
            return 1;
        return 0;
    }

    /// Equality.
    bool opEquals(const Angle other) const pure nothrow @safe @nogc
    {
        return abs(radians - other.radians) < 1e-10;
    }

    /// Check if angle is approximately zero.
    bool isZero(double tolerance = 1e-10) const pure nothrow @safe @nogc
    {
        return abs(radians) < tolerance;
    }

    /// Check if angle is a right angle (90 degrees).
    bool isRightAngle(double tolerance = 1e-10) const pure nothrow @safe @nogc
    {
        immutable normalized = normalize().radians;
        return abs(normalized - PI / 2.0) < tolerance;
    }

    /// Check if angle is acute (< 90 degrees).
    bool isAcute() const pure nothrow @safe @nogc
    {
        immutable normalized = normalize().radians;
        return normalized > 0 && normalized < PI / 2.0;
    }

    /// Check if angle is obtuse (> 90 degrees and < 180 degrees).
    bool isObtuse() const pure nothrow @safe @nogc
    {
        immutable normalized = normalize().radians;
        return normalized > PI / 2.0 && normalized < PI;
    }

    /// Check if angle is reflex (> 180 degrees).
    bool isReflex() const pure nothrow @safe @nogc
    {
        immutable normalized = normalize().radians;
        return normalized > PI;
    }

    /// Common angle constants.
    static immutable Angle ZERO = Angle(0.0);
    static immutable Angle RIGHT = Angle(PI / 2.0);
    static immutable Angle STRAIGHT = Angle(PI);
    static immutable Angle FULL = Angle(2.0 * PI);
}

/// Create angle from two points (atan2).
Angle angleFromPoints(double x1, double y1, double x2, double y2) pure nothrow @safe @nogc
{
    return Angle(atan2(y2 - y1, x2 - x1));
}

/// Safe arcsine (returns null for out-of-range inputs).
Nullable!Angle safeArcsin(double value) pure nothrow @safe @nogc
{
    if (value < -1.0 || value > 1.0)
        return Nullable!Angle.init;
    return nullable(Angle(asin(value)));
}

/// Safe arccosine (returns null for out-of-range inputs).
Nullable!Angle safeArccos(double value) pure nothrow @safe @nogc
{
    if (value < -1.0 || value > 1.0)
        return Nullable!Angle.init;
    return nullable(Angle(acos(value)));
}

/// Safe arctangent.
Angle arctan(double value) pure nothrow @safe @nogc
{
    return Angle(atan(value));
}

/// Safe arctangent with two arguments.
Angle arctan2(double y, double x) pure nothrow @safe @nogc
{
    return Angle(atan2(y, x));
}

/// Calculate angle between two vectors.
Nullable!Angle angleBetweenVectors(double x1, double y1, double x2, double y2) pure nothrow @safe @nogc
{
    immutable mag1 = (x1 * x1 + y1 * y1);
    immutable mag2 = (x2 * x2 + y2 * y2);
    if (mag1 < 1e-10 || mag2 < 1e-10)
        return Nullable!Angle.init;

    immutable dot = x1 * x2 + y1 * y2;
    import std.math : sqrt;

    immutable cosAngle = dot / (sqrt(mag1) * sqrt(mag2));

    // Clamp to valid range for acos
    immutable clamped = cosAngle < -1.0 ? -1.0 : (cosAngle > 1.0 ? 1.0 : cosAngle);
    return nullable(Angle(acos(clamped)));
}

/// Convert degrees, minutes, seconds to decimal degrees.
double dmsToDecimal(int degrees, int minutes, double seconds) pure nothrow @safe @nogc
{
    immutable sign = degrees < 0 ? -1.0 : 1.0;
    immutable absDeg = degrees < 0 ? -degrees : degrees;
    return sign * (absDeg + minutes / 60.0 + seconds / 3600.0);
}

/// Convert decimal degrees to degrees, minutes, seconds.
void decimalToDms(double decimal, out int degrees, out int minutes, out double seconds) pure nothrow @safe @nogc
{
    immutable sign = decimal < 0 ? -1 : 1;
    immutable absDecimal = decimal < 0 ? -decimal : decimal;
    degrees = cast(int) absDecimal * sign;
    immutable remaining = (absDecimal - cast(int) absDecimal) * 60.0;
    minutes = cast(int) remaining;
    seconds = (remaining - minutes) * 60.0;
}

// Unit tests
unittest
{
    // Test creation
    auto deg90 = Angle.fromDegrees(90);
    assert(abs(deg90.radians - PI / 2.0) < 1e-10);

    // Test conversion
    auto rad = Angle.fromRadians(PI);
    assert(abs(rad.toDegrees() - 180.0) < 1e-10);

    // Test trigonometry
    assert(abs(Angle.fromDegrees(0).sin()) < 1e-10);
    assert(abs(Angle.fromDegrees(90).sin() - 1.0) < 1e-10);
    assert(abs(Angle.fromDegrees(0).cos() - 1.0) < 1e-10);

    // Test normalization
    auto over360 = Angle.fromDegrees(450);
    assert(abs(over360.normalize().toDegrees() - 90.0) < 1e-10);

    // Test angle classification
    assert(Angle.fromDegrees(45).isAcute());
    assert(Angle.fromDegrees(120).isObtuse());
    assert(Angle.fromDegrees(270).isReflex());

    // Test safe inverse trig
    assert(!safeArcsin(0.5).isNull);
    assert(safeArcsin(2.0).isNull);
    assert(safeArccos(-2.0).isNull);

    // Test DMS conversion
    immutable decimal = dmsToDecimal(45, 30, 30);
    assert(abs(decimal - 45.508333) < 0.001);
}
