// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

/**
 * Safe mathematical operations with overflow protection.
 */
object SafeMath {

  /** Maximum safe Long value */
  val MaxLong: Long = Long.MaxValue

  /** Minimum safe Long value */
  val MinLong: Long = Long.MinValue

  /**
   * Checked addition that detects overflow.
   * @return Some(result) if successful, None on overflow
   */
  def add(a: Long, b: Long): Option[Long] = {
    try {
      Some(Math.addExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked subtraction that detects overflow.
   * @return Some(result) if successful, None on overflow
   */
  def sub(a: Long, b: Long): Option[Long] = {
    try {
      Some(Math.subtractExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked multiplication that detects overflow.
   * @return Some(result) if successful, None on overflow
   */
  def mul(a: Long, b: Long): Option[Long] = {
    try {
      Some(Math.multiplyExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked division that handles division by zero.
   * @return Some(result) if successful, None on division by zero or overflow
   */
  def div(numerator: Long, denominator: Long): Option[Long] = {
    if (denominator == 0) None
    else if (numerator == MinLong && denominator == -1) None
    else Some(numerator / denominator)
  }

  /**
   * Checked modulo operation.
   * @return Some(result) if successful, None if divisor is zero
   */
  def mod(a: Long, b: Long): Option[Long] = {
    if (b == 0) None
    else Some(a % b)
  }

  /**
   * Checked negation.
   * @return Some(-a) if successful, None if a == MinLong
   */
  def neg(a: Long): Option[Long] = {
    try {
      Some(Math.negateExact(a))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked absolute value.
   * @return Some(|a|) if successful, None if a == MinLong
   */
  def abs(a: Long): Option[Long] = {
    if (a == MinLong) None
    else Some(Math.abs(a))
  }

  /**
   * Checked power operation.
   * @return Some(base^exponent) if successful, None on overflow or negative exponent
   */
  def pow(base: Long, exponent: Int): Option[Long] = {
    if (exponent < 0) return None
    if (exponent == 0) return Some(1L)
    if (base == 0) return Some(0L)
    if (base == 1) return Some(1L)
    if (base == -1) return Some(if (exponent % 2 == 0) 1L else -1L)

    // Use BigInt for overflow detection
    val result = BigInt(base).pow(exponent)
    if (result > BigInt(MaxLong) || result < BigInt(MinLong)) None
    else Some(result.toLong)
  }

  /**
   * Clamp value to range.
   */
  def clamp(value: Long, min: Long, max: Long): Long = {
    if (value < min) min
    else if (value > max) max
    else value
  }

  /**
   * Check if addition would overflow without performing it.
   */
  def wouldAddOverflow(a: Long, b: Long): Boolean = add(a, b).isEmpty

  /**
   * Check if multiplication would overflow without performing it.
   */
  def wouldMulOverflow(a: Long, b: Long): Boolean = mul(a, b).isEmpty

  /**
   * Checked addition for Int.
   */
  def addInt(a: Int, b: Int): Option[Int] = {
    try {
      Some(Math.addExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked subtraction for Int.
   */
  def subInt(a: Int, b: Int): Option[Int] = {
    try {
      Some(Math.subtractExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }

  /**
   * Checked multiplication for Int.
   */
  def mulInt(a: Int, b: Int): Option[Int] = {
    try {
      Some(Math.multiplyExact(a, b))
    } catch {
      case _: ArithmeticException => None
    }
  }
}
