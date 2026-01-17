// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe floating-point operations with NaN/Infinity prevention.
 * Provides mathematically verified safe operations for float/double,
 * preventing common pitfalls in numerical computing.
 */
module proven.safe_float;

import std.math : sqrt, log, log10, exp, pow, isNaN, isInfinity, isFinite, abs;
import std.typecons : Nullable, nullable;

/// Minimum positive value to consider non-zero (prevents denormal issues).
enum EPSILON = 1e-10;

/// Safe division with zero check.
Nullable!double safeDiv(double numerator, double denominator) pure nothrow @safe @nogc
{
    if (abs(denominator) < EPSILON)
        return Nullable!double.init;
    immutable result = numerator / denominator;
    if (isNaN(result) || isInfinity(result))
        return Nullable!double.init;
    return nullable(result);
}

/// Safe division for float.
Nullable!float safeDivFloat(float numerator, float denominator) pure nothrow @safe @nogc
{
    if (abs(denominator) < float.epsilon)
        return Nullable!float.init;
    immutable result = numerator / denominator;
    if (isNaN(result) || isInfinity(result))
        return Nullable!float.init;
    return nullable(result);
}

/// Safe natural logarithm.
Nullable!double safeLn(double x) pure nothrow @safe @nogc
{
    if (x <= 0.0)
        return Nullable!double.init;
    return nullable(log(x));
}

/// Safe log base 10.
Nullable!double safeLog10(double x) pure nothrow @safe @nogc
{
    if (x <= 0.0)
        return Nullable!double.init;
    return nullable(log10(x));
}

/// Safe square root.
Nullable!double safeSqrt(double x) pure nothrow @safe @nogc
{
    if (x < 0.0)
        return Nullable!double.init;
    return nullable(sqrt(x));
}

/// Safe power operation.
Nullable!double safePow(double base, double exponent) pure nothrow @safe @nogc
{
    immutable result = pow(base, exponent);
    if (isNaN(result) || isInfinity(result))
        return Nullable!double.init;
    return nullable(result);
}

/// Safe exponential (e^x).
Nullable!double safeExp(double x) pure nothrow @safe @nogc
{
    immutable result = exp(x);
    if (isInfinity(result))
        return Nullable!double.init;
    return nullable(result);
}

/// Safe exponential for float.
Nullable!float safeExpFloat(float x) pure nothrow @safe @nogc
{
    immutable result = exp(x);
    if (isInfinity(result))
        return Nullable!float.init;
    return nullable(result);
}

/// Compute vector magnitude (L2 norm).
double magnitude(const double[] vector) pure nothrow @safe
{
    double sumSquares = 0.0;
    foreach (v; vector)
        sumSquares += v * v;
    return sqrt(sumSquares);
}

/// Compute float vector magnitude.
float magnitudeFloat(const float[] vector) pure nothrow @safe
{
    float sumSquares = 0.0f;
    foreach (v; vector)
        sumSquares += v * v;
    return sqrt(sumSquares);
}

/// Safe vector normalization.
Nullable!(double[]) normalize(const double[] vector) pure nothrow @safe
{
    immutable mag = magnitude(vector);
    if (mag < EPSILON)
        return typeof(return).init;

    double[] result = new double[vector.length];
    foreach (i, v; vector)
        result[i] = v / mag;
    return nullable(result);
}

/// Safe float vector normalization.
Nullable!(float[]) normalizeFloat(const float[] vector) pure nothrow @safe
{
    immutable mag = magnitudeFloat(vector);
    if (mag < float.epsilon)
        return typeof(return).init;

    float[] result = new float[vector.length];
    foreach (i, v; vector)
        result[i] = v / mag;
    return nullable(result);
}

/// Check if a float is finite (not NaN or Infinity).
bool isSafeFloat(double x) pure nothrow @safe @nogc
{
    return isFinite(x);
}

/// Check if a float is safe for division (non-zero and finite).
bool isSafeDivisor(double x) pure nothrow @safe @nogc
{
    return isFinite(x) && abs(x) >= EPSILON;
}

/// Clamp a float to a range, handling NaN by returning min.
double safeClamp(double value, double minValue, double maxValue) pure nothrow @safe @nogc
{
    if (isNaN(value))
        return minValue;
    if (value < minValue)
        return minValue;
    if (value > maxValue)
        return maxValue;
    return value;
}

/// Safe reciprocal (1/x).
Nullable!double safeReciprocal(double x) pure nothrow @safe @nogc
{
    return safeDiv(1.0, x);
}

/// Compute mean of a vector safely.
Nullable!double safeMean(const double[] values) pure nothrow @safe
{
    if (values.length == 0)
        return Nullable!double.init;

    double sum = 0.0;
    foreach (v; values)
        sum += v;
    return safeDiv(sum, cast(double) values.length);
}

/// Compute mean of float vector.
Nullable!float safeMeanFloat(const float[] values) pure nothrow @safe
{
    if (values.length == 0)
        return Nullable!float.init;

    float sum = 0.0f;
    foreach (v; values)
        sum += v;
    return safeDivFloat(sum, cast(float) values.length);
}

/// Compute variance safely.
Nullable!double safeVariance(const double[] values) pure nothrow @safe
{
    auto meanResult = safeMean(values);
    if (meanResult.isNull)
        return Nullable!double.init;

    immutable mean = meanResult.get;
    double sumSq = 0.0;
    foreach (v; values)
    {
        immutable diff = v - mean;
        sumSq += diff * diff;
    }
    return safeDiv(sumSq, cast(double) values.length);
}

/// Compute standard deviation safely.
Nullable!double safeStdDev(const double[] values) pure nothrow @safe
{
    auto varResult = safeVariance(values);
    if (varResult.isNull)
        return Nullable!double.init;
    return safeSqrt(varResult.get);
}

/// Compute dot product of two vectors.
Nullable!double safeDotProduct(const double[] v1, const double[] v2) pure nothrow @safe
{
    if (v1.length != v2.length)
        return Nullable!double.init;

    double sum = 0.0;
    foreach (i; 0 .. v1.length)
        sum += v1[i] * v2[i];

    if (isNaN(sum) || isInfinity(sum))
        return Nullable!double.init;

    return nullable(sum);
}

/// Compute cosine similarity between two vectors.
Nullable!double cosineSimilarity(const double[] v1, const double[] v2) pure nothrow @safe
{
    auto dot = safeDotProduct(v1, v2);
    if (dot.isNull)
        return Nullable!double.init;

    immutable mag1 = magnitude(v1);
    immutable mag2 = magnitude(v2);

    if (mag1 < EPSILON || mag2 < EPSILON)
        return Nullable!double.init;

    return safeDiv(dot.get, mag1 * mag2);
}

/// Linear interpolation with clamping.
double safeLerp(double start, double end, double t) pure nothrow @safe @nogc
{
    immutable clampedT = safeClamp(t, 0.0, 1.0);
    return start + (end - start) * clampedT;
}

/// Check if two floats are approximately equal.
bool approxEqual(double a, double b, double tolerance = EPSILON) pure nothrow @safe @nogc
{
    return abs(a - b) < tolerance;
}

// Unit tests
unittest
{
    // Test safe division
    assert(safeDiv(10.0, 2.0).get == 5.0);
    assert(safeDiv(1.0, 0.0).isNull);
    assert(safeDiv(0.0, 0.0).isNull);

    // Test safe logarithm
    import std.math : E;
    assert(abs(safeLn(E).get - 1.0) < EPSILON);
    assert(safeLn(0.0).isNull);
    assert(safeLn(-1.0).isNull);

    // Test safe sqrt
    assert(safeSqrt(4.0).get == 2.0);
    assert(safeSqrt(0.0).get == 0.0);
    assert(safeSqrt(-1.0).isNull);

    // Test normalization
    auto n = normalize([3.0, 4.0]);
    assert(!n.isNull);
    assert(abs(n.get[0] - 0.6) < EPSILON);
    assert(abs(n.get[1] - 0.8) < EPSILON);
    assert(normalize([0.0, 0.0, 0.0]).isNull);

    // Test safe exp
    assert(abs(safeExp(0.0).get - 1.0) < EPSILON);
    assert(safeExp(1000.0).isNull);

    // Test clamp with NaN
    assert(safeClamp(double.nan, 0.0, 1.0) == 0.0);
    assert(safeClamp(0.5, 0.0, 1.0) == 0.5);

    // Test mean
    assert(abs(safeMean([1.0, 2.0, 3.0, 4.0, 5.0]).get - 3.0) < EPSILON);
    assert(safeMean([]).isNull);

    // Test variance and std dev
    auto v = [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
    assert(abs(safeVariance(v).get - 4.0) < EPSILON);
    assert(abs(safeStdDev(v).get - 2.0) < EPSILON);
}
