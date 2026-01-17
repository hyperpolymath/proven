// SPDX-License-Identifier: PMPL-1.0

package proven

/**
 * Safe floating-point operations with NaN/Infinity prevention.
 *
 * Unlike integers which overflow, floating-point numbers silently produce
 * NaN or Infinity on edge cases. This module makes those edge cases explicit
 * through Option/Either types.
 */
object SafeFloat:

  /** Minimum positive value to consider non-zero (prevents denormal issues) */
  val Epsilon: Double = 1e-10

  /** Epsilon for single precision */
  val EpsilonF: Float = 1e-6f

  /**
   * Safe division with zero check.
   * Returns None if divisor is zero or too small.
   */
  def div(a: Double, b: Double): Option[Double] =
    if b.abs < Epsilon then None
    else
      val result = a / b
      if result.isNaN || result.isInfinite then None
      else Some(result)

  /**
   * Safe division for Float.
   */
  def divF(a: Float, b: Float): Option[Float] =
    if b.abs < EpsilonF then None
    else
      val result = a / b
      if result.isNaN || result.isInfinite then None
      else Some(result)

  /**
   * Safe natural logarithm (ln).
   * Returns None for non-positive values (which would produce NaN or -Infinity).
   */
  def ln(x: Double): Option[Double] =
    if x <= 0.0 then None
    else Some(Math.log(x))

  /**
   * Safe log base 10.
   */
  def log10(x: Double): Option[Double] =
    if x <= 0.0 then None
    else Some(Math.log10(x))

  /**
   * Safe log base 2.
   */
  def log2(x: Double): Option[Double] =
    if x <= 0.0 then None
    else Some(Math.log(x) / Math.log(2))

  /**
   * Safe square root.
   * Returns None for negative values.
   */
  def sqrt(x: Double): Option[Double] =
    if x < 0.0 then None
    else Some(Math.sqrt(x))

  /**
   * Safe power operation.
   * Returns None if result would be NaN or Infinity.
   */
  def pow(base: Double, exp: Double): Option[Double] =
    val result = Math.pow(base, exp)
    if result.isNaN || result.isInfinite then None
    else Some(result)

  /**
   * Safe exponential (e^x).
   * Returns None if result would overflow to Infinity.
   */
  def exp(x: Double): Option[Double] =
    val result = Math.exp(x)
    if result.isInfinite then None
    else Some(result)

  /**
   * Safe exponential for Float.
   */
  def expF(x: Float): Option[Float] =
    val result = Math.exp(x.toDouble).toFloat
    if result.isInfinite then None
    else Some(result)

  /**
   * Compute vector magnitude (L2 norm).
   */
  def magnitude(v: Seq[Double]): Double =
    Math.sqrt(v.map(x => x * x).sum)

  /**
   * Compute vector magnitude for Float.
   */
  def magnitudeF(v: Seq[Float]): Float =
    Math.sqrt(v.map(x => x * x).sum).toFloat

  /**
   * Safe vector normalization (unit vector).
   * Returns None if vector has zero magnitude.
   */
  def normalize(v: Seq[Double]): Option[Seq[Double]] =
    val mag = magnitude(v)
    if mag < Epsilon then None
    else Some(v.map(_ / mag))

  /**
   * Safe vector normalization for Float.
   */
  def normalizeF(v: Seq[Float]): Option[Seq[Float]] =
    val mag = magnitudeF(v)
    if mag < EpsilonF then None
    else Some(v.map(_ / mag))

  /**
   * Check if a float is finite (not NaN or Infinity).
   */
  def isFinite(x: Double): Boolean =
    !x.isNaN && !x.isInfinite

  /**
   * Check if a float is safe for division (non-zero and finite).
   */
  def isSafeDivisor(x: Double): Boolean =
    isFinite(x) && x.abs >= Epsilon

  /**
   * Clamp a float to a range, handling NaN by returning min.
   * Unlike standard clamp which may panic on NaN, this safely handles it.
   */
  def clamp(value: Double, min: Double, max: Double): Double =
    if value.isNaN then min
    else if value < min then min
    else if value > max then max
    else value

  /**
   * Safe reciprocal (1/x).
   */
  def reciprocal(x: Double): Option[Double] = div(1.0, x)

  /**
   * Compute mean of a vector safely.
   * Returns None for empty vectors.
   */
  def mean(v: Seq[Double]): Option[Double] =
    if v.isEmpty then None
    else div(v.sum, v.length.toDouble)

  /**
   * Compute mean for Float.
   */
  def meanF(v: Seq[Float]): Option[Float] =
    if v.isEmpty then None
    else divF(v.sum, v.length.toFloat)

  /**
   * Compute variance safely.
   */
  def variance(v: Seq[Double]): Option[Double] =
    mean(v).flatMap { m =>
      val sumSq = v.map(x => (x - m) * (x - m)).sum
      div(sumSq, v.length.toDouble)
    }

  /**
   * Compute standard deviation safely.
   */
  def stdDev(v: Seq[Double]): Option[Double] =
    variance(v).flatMap(sqrt)

  /**
   * Dot product of two vectors.
   */
  def dot(a: Seq[Double], b: Seq[Double]): Option[Double] =
    if a.length != b.length then None
    else Some(a.zip(b).map { case (x, y) => x * y }.sum)

  /**
   * Cosine similarity between two vectors.
   */
  def cosineSimilarity(a: Seq[Double], b: Seq[Double]): Option[Double] =
    for
      d <- dot(a, b)
      magA = magnitude(a)
      magB = magnitude(b)
      result <- div(d, magA * magB)
    yield result

  /**
   * Linear interpolation between two values.
   */
  def lerp(a: Double, b: Double, t: Double): Double =
    a + (b - a) * clamp(t, 0.0, 1.0)

  /**
   * Safe sigmoid function (1 / (1 + e^-x)).
   */
  def sigmoid(x: Double): Double =
    if x >= 0 then
      1.0 / (1.0 + Math.exp(-x))
    else
      val ex = Math.exp(x)
      ex / (1.0 + ex)

  /**
   * Safe softmax for a vector.
   */
  def softmax(v: Seq[Double]): Option[Seq[Double]] =
    if v.isEmpty then return None
    val maxVal = v.max
    val exps = v.map(x => Math.exp(x - maxVal))
    val sum = exps.sum
    if sum == 0.0 || sum.isInfinite then None
    else Some(exps.map(_ / sum))

  /**
   * Check if two doubles are approximately equal.
   */
  def approxEqual(a: Double, b: Double, epsilon: Double = Epsilon): Boolean =
    (a - b).abs < epsilon

  /**
   * Safe arc cosine.
   */
  def acos(x: Double): Option[Double] =
    if x < -1.0 || x > 1.0 then None
    else Some(Math.acos(x))

  /**
   * Safe arc sine.
   */
  def asin(x: Double): Option[Double] =
    if x < -1.0 || x > 1.0 then None
    else Some(Math.asin(x))
