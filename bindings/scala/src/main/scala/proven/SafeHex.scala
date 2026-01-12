// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Hex encoding result.
 */
sealed trait HexResult[+A]:
  def isOk: Boolean
  def isError: Boolean = !isOk
  def value: Option[A]
  def error: Option[String]

  def map[B](f: A => B): HexResult[B] = this match
    case HexResult.Ok(v) => HexResult.Ok(f(v))
    case HexResult.Error(e) => HexResult.Error(e)

  def flatMap[B](f: A => HexResult[B]): HexResult[B] = this match
    case HexResult.Ok(v) => f(v)
    case HexResult.Error(e) => HexResult.Error(e)

  def getOrElse[B >: A](default: => B): B = value.getOrElse(default)

  def toOption: Option[A] = value

  def toEither: Either[String, A] = this match
    case HexResult.Ok(v) => Right(v)
    case HexResult.Error(e) => Left(e)

object HexResult:
  case class Ok[A](v: A) extends HexResult[A]:
    override def isOk: Boolean = true
    override def value: Option[A] = Some(v)
    override def error: Option[String] = None

  case class Error[A](msg: String) extends HexResult[A]:
    override def isOk: Boolean = false
    override def value: Option[A] = None
    override def error: Option[String] = Some(msg)

  def ok[A](v: A): HexResult[A] = Ok(v)
  def error[A](msg: String): HexResult[A] = Error(msg)

/**
 * Safe hexadecimal encoding and decoding operations.
 *
 * This object provides secure hexadecimal encoding/decoding with:
 * - Constant-time comparison to prevent timing attacks
 * - Strict validation of hex strings
 * - Support for various input/output formats
 */
object SafeHex:

  /** Lowercase hex characters. */
  private val HexCharsLower: Array[Char] = "0123456789abcdef".toCharArray

  /** Uppercase hex characters. */
  private val HexCharsUpper: Array[Char] = "0123456789ABCDEF".toCharArray

  /** Valid hex character pattern. */
  private val HexPattern = """^[0-9a-fA-F]*$""".r

  /**
   * Encode a byte array to lowercase hexadecimal string.
   */
  def encode(bytes: Array[Byte]): String =
    encodeWithCase(bytes, lowercase = true)

  /**
   * Encode a byte array to hexadecimal string with case option.
   */
  def encodeWithCase(bytes: Array[Byte], lowercase: Boolean): String =
    val chars = if lowercase then HexCharsLower else HexCharsUpper
    val result = new Array[Char](bytes.length * 2)

    var i = 0
    while i < bytes.length do
      val b = bytes(i) & 0xFF
      result(i * 2) = chars(b >> 4)
      result(i * 2 + 1) = chars(b & 0x0F)
      i += 1

    new String(result)

  /**
   * Encode a byte array to uppercase hexadecimal string.
   */
  def encodeUppercase(bytes: Array[Byte]): String =
    encodeWithCase(bytes, lowercase = false)

  /**
   * Encode a string (UTF-8) to lowercase hexadecimal.
   */
  def encodeString(input: String): String =
    encode(input.getBytes(StandardCharsets.UTF_8))

  /**
   * Encode a string (UTF-8) to uppercase hexadecimal.
   */
  def encodeStringUppercase(input: String): String =
    encodeUppercase(input.getBytes(StandardCharsets.UTF_8))

  /**
   * Decode a hexadecimal string to byte array.
   */
  def decode(hex: String): HexResult[Array[Byte]] =
    val cleaned = hex.trim

    // Check for empty input
    if cleaned.isEmpty then
      return HexResult.ok(Array.empty[Byte])

    // Remove optional 0x prefix
    val withoutPrefix = if cleaned.startsWith("0x") || cleaned.startsWith("0X") then
      cleaned.substring(2)
    else
      cleaned

    // Check for valid length (must be even)
    if withoutPrefix.length % 2 != 0 then
      return HexResult.error("invalid_length_odd")

    // Validate characters
    if HexPattern.findFirstIn(withoutPrefix).isEmpty then
      return HexResult.error("invalid_characters")

    Try {
      val result = new Array[Byte](withoutPrefix.length / 2)

      var i = 0
      while i < result.length do
        val high = hexCharToInt(withoutPrefix.charAt(i * 2))
        val low = hexCharToInt(withoutPrefix.charAt(i * 2 + 1))
        result(i) = ((high << 4) | low).toByte
        i += 1

      result
    }.fold(
      _ => HexResult.error("decode_failed"),
      HexResult.ok
    )

  /**
   * Decode a hexadecimal string to a UTF-8 string.
   */
  def decodeToString(hex: String): HexResult[String] =
    decode(hex).map(bytes => new String(bytes, StandardCharsets.UTF_8))

  /**
   * Check if a string is valid hexadecimal.
   */
  def isValid(hex: String): Boolean =
    val cleaned = hex.trim
    if cleaned.isEmpty then return true

    val withoutPrefix = if cleaned.startsWith("0x") || cleaned.startsWith("0X") then
      cleaned.substring(2)
    else
      cleaned

    withoutPrefix.length % 2 == 0 && HexPattern.findFirstIn(withoutPrefix).isDefined

  /**
   * Constant-time comparison of two hex strings.
   * Returns true if and only if both strings represent the same bytes.
   * This prevents timing attacks when comparing secret values.
   */
  def constantTimeEqual(a: String, b: String): Boolean =
    val bytesA = decode(a)
    val bytesB = decode(b)

    (bytesA, bytesB) match
      case (HexResult.Ok(arrA), HexResult.Ok(arrB)) =>
        constantTimeEqualBytes(arrA, arrB)
      case _ =>
        false

  /**
   * Constant-time comparison of two byte arrays.
   * Returns true if and only if both arrays are equal.
   * This prevents timing attacks when comparing secret values.
   */
  def constantTimeEqualBytes(a: Array[Byte], b: Array[Byte]): Boolean =
    if a.length != b.length then
      // Still do a comparison to maintain constant time for length check
      var dummy = 0
      var i = 0
      val len = Math.max(a.length, b.length)
      while i < len do
        dummy |= (if i < a.length then a(i) else 0) ^ (if i < b.length then b(i) else 0)
        i += 1
      false
    else
      var result = 0
      var i = 0
      while i < a.length do
        result |= a(i) ^ b(i)
        i += 1
      result == 0

  /**
   * Normalize a hex string to lowercase without prefix.
   */
  def normalize(hex: String): HexResult[String] =
    decode(hex).map(encode)

  /**
   * Normalize a hex string to uppercase without prefix.
   */
  def normalizeUppercase(hex: String): HexResult[String] =
    decode(hex).map(encodeUppercase)

  /**
   * Add 0x prefix to a hex string.
   */
  def withPrefix(hex: String): String =
    val cleaned = hex.trim
    if cleaned.startsWith("0x") || cleaned.startsWith("0X") then cleaned
    else s"0x$cleaned"

  /**
   * Remove 0x prefix from a hex string.
   */
  def withoutPrefix(hex: String): String =
    val cleaned = hex.trim
    if cleaned.startsWith("0x") || cleaned.startsWith("0X") then cleaned.substring(2)
    else cleaned

  /**
   * Format hex string with grouping for readability.
   *
   * @param hex       The hex string to format
   * @param groupSize Number of characters per group (default: 2)
   * @param separator Separator between groups (default: " ")
   */
  def format(hex: String, groupSize: Int = 2, separator: String = " "): HexResult[String] =
    val cleaned = withoutPrefix(hex.trim)
    if !isValid(cleaned) then
      return HexResult.error("invalid_hex")

    val normalized = cleaned.toLowerCase
    val groups = normalized.grouped(groupSize).toSeq
    HexResult.ok(groups.mkString(separator))

  /**
   * Parse a formatted hex string (with spaces or other separators).
   */
  def parseFormatted(hex: String): HexResult[Array[Byte]] =
    val cleaned = hex.replaceAll("[\\s:-]", "")
    decode(cleaned)

  /**
   * Convert a single byte to a two-character hex string.
   */
  def byteToHex(b: Byte): String =
    val chars = new Array[Char](2)
    val unsigned = b & 0xFF
    chars(0) = HexCharsLower(unsigned >> 4)
    chars(1) = HexCharsLower(unsigned & 0x0F)
    new String(chars)

  /**
   * Convert a single byte to an uppercase two-character hex string.
   */
  def byteToHexUppercase(b: Byte): String =
    val chars = new Array[Char](2)
    val unsigned = b & 0xFF
    chars(0) = HexCharsUpper(unsigned >> 4)
    chars(1) = HexCharsUpper(unsigned & 0x0F)
    new String(chars)

  /**
   * Parse a two-character hex string to a byte.
   */
  def hexToByte(hex: String): HexResult[Byte] =
    if hex.length != 2 then
      return HexResult.error("invalid_length")

    if HexPattern.findFirstIn(hex).isEmpty then
      return HexResult.error("invalid_characters")

    Try {
      val high = hexCharToInt(hex.charAt(0))
      val low = hexCharToInt(hex.charAt(1))
      ((high << 4) | low).toByte
    }.fold(
      _ => HexResult.error("parse_failed"),
      HexResult.ok
    )

  /**
   * Convert an integer to a hexadecimal string (minimum width).
   */
  def intToHex(value: Int, minWidth: Int = 0): String =
    val hex = java.lang.Integer.toHexString(value)
    if hex.length < minWidth then "0" * (minWidth - hex.length) + hex
    else hex

  /**
   * Convert a long to a hexadecimal string (minimum width).
   */
  def longToHex(value: Long, minWidth: Int = 0): String =
    val hex = java.lang.Long.toHexString(value)
    if hex.length < minWidth then "0" * (minWidth - hex.length) + hex
    else hex

  /**
   * Parse a hex string to an integer.
   */
  def hexToInt(hex: String): HexResult[Int] =
    val cleaned = withoutPrefix(hex.trim)
    if cleaned.isEmpty then
      return HexResult.error("empty_input")

    if HexPattern.findFirstIn(cleaned).isEmpty then
      return HexResult.error("invalid_characters")

    Try(java.lang.Integer.parseUnsignedInt(cleaned, 16)).fold(
      _ => HexResult.error("overflow_or_invalid"),
      HexResult.ok
    )

  /**
   * Parse a hex string to a long.
   */
  def hexToLong(hex: String): HexResult[Long] =
    val cleaned = withoutPrefix(hex.trim)
    if cleaned.isEmpty then
      return HexResult.error("empty_input")

    if HexPattern.findFirstIn(cleaned).isEmpty then
      return HexResult.error("invalid_characters")

    Try(java.lang.Long.parseUnsignedLong(cleaned, 16)).fold(
      _ => HexResult.error("overflow_or_invalid"),
      HexResult.ok
    )

  /**
   * XOR two hex strings of equal length.
   */
  def xor(a: String, b: String): HexResult[String] =
    val bytesA = decode(a)
    val bytesB = decode(b)

    (bytesA, bytesB) match
      case (HexResult.Ok(arrA), HexResult.Ok(arrB)) =>
        if arrA.length != arrB.length then
          HexResult.error("length_mismatch")
        else
          val result = new Array[Byte](arrA.length)
          var i = 0
          while i < arrA.length do
            result(i) = (arrA(i) ^ arrB(i)).toByte
            i += 1
          HexResult.ok(encode(result))
      case (HexResult.Error(e), _) => HexResult.error(e)
      case (_, HexResult.Error(e)) => HexResult.error(e)

  /**
   * Get the byte length of a hex string.
   */
  def byteLength(hex: String): Option[Int] =
    val cleaned = withoutPrefix(hex.trim)
    if isValid(cleaned) then Some(cleaned.length / 2)
    else None

  /**
   * Convert a hex character to its integer value.
   */
  private def hexCharToInt(c: Char): Int = c match
    case c if c >= '0' && c <= '9' => c - '0'
    case c if c >= 'a' && c <= 'f' => c - 'a' + 10
    case c if c >= 'A' && c <= 'F' => c - 'A' + 10
    case _ => throw new IllegalArgumentException(s"Invalid hex character: $c")

  // Extension methods for convenience
  extension (bytes: Array[Byte])
    /**
     * Encode byte array to lowercase hex string.
     */
    def toHex: String = encode(bytes)

    /**
     * Encode byte array to uppercase hex string.
     */
    def toHexUppercase: String = encodeUppercase(bytes)

  extension (s: String)
    /**
     * Decode hex string to byte array.
     */
    def fromHex: HexResult[Array[Byte]] = decode(s)

    /**
     * Check if string is valid hex.
     */
    def isHex: Boolean = isValid(s)

    /**
     * Encode string (UTF-8) to hex.
     */
    def toHexEncoded: String = encodeString(s)
