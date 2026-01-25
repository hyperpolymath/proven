// SPDX-License-Identifier: PMPL-1.0

package proven

/**
 * Angle representation with safe conversions.
 */
case class Angle private (radians: Double):

  /**
   * Get value in degrees.
   */
  def degrees: Double = radians * 180.0 / Math.PI

  /**
   * Get value in gradians (grads).
   */
  def gradians: Double = radians * 200.0 / Math.PI

  /**
   * Get value in turns (full rotations).
   */
  def turns: Double = radians / (2 * Math.PI)

  /**
   * Normalize to [0, 2*PI).
   */
  def normalize: Angle =
    val normalized = radians % (2 * Math.PI)
    Angle(if normalized < 0 then normalized + 2 * Math.PI else normalized)

  /**
   * Normalize to [-PI, PI).
   */
  def normalizeSigned: Angle =
    val normalized = normalize.radians
    Angle(if normalized >= Math.PI then normalized - 2 * Math.PI else normalized)

  /**
   * Add two angles.
   */
  def +(other: Angle): Angle = Angle(radians + other.radians)

  /**
   * Subtract two angles.
   */
  def -(other: Angle): Angle = Angle(radians - other.radians)

  /**
   * Multiply angle by scalar.
   */
  def *(scalar: Double): Angle = Angle(radians * scalar)

  /**
   * Divide angle by scalar.
   */
  def /(scalar: Double): Option[Angle] =
    if scalar.abs < 1e-10 then None
    else Some(Angle(radians / scalar))

  /**
   * Negate the angle.
   */
  def unary_- : Angle = Angle(-radians)

  /**
   * Sine of the angle.
   */
  def sin: Double = Math.sin(radians)

  /**
   * Cosine of the angle.
   */
  def cos: Double = Math.cos(radians)

  /**
   * Tangent of the angle.
   */
  def tan: Option[Double] =
    val normalized = normalize.radians
    // tan is undefined at PI/2 and 3*PI/2
    if (normalized - Math.PI / 2).abs < 1e-10 || (normalized - 3 * Math.PI / 2).abs < 1e-10 then
      None
    else
      Some(Math.tan(radians))

  /**
   * Check if angle is acute (< 90 degrees).
   */
  def isAcute: Boolean =
    val norm = normalize.radians
    norm > 0 && norm < Math.PI / 2

  /**
   * Check if angle is right (= 90 degrees).
   */
  def isRight: Boolean =
    (normalize.radians - Math.PI / 2).abs < 1e-10

  /**
   * Check if angle is obtuse (90 < angle < 180 degrees).
   */
  def isObtuse: Boolean =
    val norm = normalize.radians
    norm > Math.PI / 2 && norm < Math.PI

  /**
   * Check if angle is straight (= 180 degrees).
   */
  def isStraight: Boolean =
    (normalize.radians - Math.PI).abs < 1e-10

  /**
   * Check if angle is reflex (> 180 degrees).
   */
  def isReflex: Boolean =
    val norm = normalize.radians
    norm > Math.PI && norm < 2 * Math.PI

  override def toString: String = f"${degrees}%.2f deg"

object Angle:
  val Zero: Angle = Angle(0)
  val RightAngle: Angle = Angle(Math.PI / 2)
  val StraightAngle: Angle = Angle(Math.PI)
  val FullRotation: Angle = Angle(2 * Math.PI)

  /**
   * Create angle from radians.
   */
  def fromRadians(rad: Double): Angle = new Angle(rad)

  /**
   * Create angle from degrees.
   */
  def fromDegrees(deg: Double): Angle = new Angle(deg * Math.PI / 180.0)

  /**
   * Create angle from gradians.
   */
  def fromGradians(grad: Double): Angle = new Angle(grad * Math.PI / 200.0)

  /**
   * Create angle from turns.
   */
  def fromTurns(turns: Double): Angle = new Angle(turns * 2 * Math.PI)

/**
 * Safe angle operations.
 */
object SafeAngle:

  /**
   * Create angle from radians.
   */
  def radians(rad: Double): Angle = Angle.fromRadians(rad)

  /**
   * Create angle from degrees.
   */
  def degrees(deg: Double): Angle = Angle.fromDegrees(deg)

  /**
   * Create angle from gradians.
   */
  def gradians(grad: Double): Angle = Angle.fromGradians(grad)

  /**
   * Create angle from turns.
   */
  def turns(t: Double): Angle = Angle.fromTurns(t)

  /**
   * Convert degrees to radians.
   */
  def degreesToRadians(deg: Double): Double = deg * Math.PI / 180.0

  /**
   * Convert radians to degrees.
   */
  def radiansToDegrees(rad: Double): Double = rad * 180.0 / Math.PI

  /**
   * Safe arc sine (returns angle).
   */
  def asin(x: Double): Option[Angle] =
    if x < -1.0 || x > 1.0 then None
    else Some(Angle.fromRadians(Math.asin(x)))

  /**
   * Safe arc cosine (returns angle).
   */
  def acos(x: Double): Option[Angle] =
    if x < -1.0 || x > 1.0 then None
    else Some(Angle.fromRadians(Math.acos(x)))

  /**
   * Arc tangent (always valid).
   */
  def atan(x: Double): Angle = Angle.fromRadians(Math.atan(x))

  /**
   * Two-argument arc tangent.
   */
  def atan2(y: Double, x: Double): Angle = Angle.fromRadians(Math.atan2(y, x))

  /**
   * Calculate angle between two 2D vectors.
   */
  def angleBetween(x1: Double, y1: Double, x2: Double, y2: Double): Option[Angle] =
    val dot = x1 * x2 + y1 * y2
    val mag1 = Math.sqrt(x1 * x1 + y1 * y1)
    val mag2 = Math.sqrt(x2 * x2 + y2 * y2)
    val denom = mag1 * mag2
    if denom < 1e-10 then None
    else
      val cosAngle = (dot / denom).max(-1.0).min(1.0)
      Some(Angle.fromRadians(Math.acos(cosAngle)))

  /**
   * Calculate bearing from one point to another (in degrees, 0 = North).
   */
  def bearing(fromX: Double, fromY: Double, toX: Double, toY: Double): Angle =
    val dx = toX - fromX
    val dy = toY - fromY
    val angle = Math.atan2(dx, dy) // Note: atan2(x, y) for bearing
    Angle.fromRadians(angle).normalize

  /**
   * Interpolate between two angles (shortest path).
   */
  def interpolate(a: Angle, b: Angle, t: Double): Angle =
    val factor = t.min(1.0).max(0.0)
    val diff = (b - a).normalizeSigned.radians
    Angle.fromRadians(a.radians + diff * factor)

  /**
   * Find the smallest angle difference.
   */
  def shortestDifference(a: Angle, b: Angle): Angle =
    (b - a).normalizeSigned

  /**
   * Check if an angle is within a range (inclusive).
   */
  def isInRange(angle: Angle, start: Angle, end: Angle): Boolean =
    val normAngle = angle.normalize.radians
    val normStart = start.normalize.radians
    val normEnd = end.normalize.radians

    if normStart <= normEnd then
      normAngle >= normStart && normAngle <= normEnd
    else
      // Range wraps around (e.g., 350 to 10 degrees)
      normAngle >= normStart || normAngle <= normEnd
