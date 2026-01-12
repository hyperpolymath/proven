// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.security.SecureRandom
import java.util.UUID as JavaUUID
import scala.util.Try

/**
 * UUID version as per RFC 4122.
 */
enum UuidVersion:
  case V1   // Time-based
  case V2   // DCE Security
  case V3   // Name-based (MD5)
  case V4   // Random
  case V5   // Name-based (SHA-1)
  case V6   // Reordered time-based (draft)
  case V7   // Unix Epoch time-based (draft)
  case V8   // Custom (draft)
  case Unknown

object UuidVersion:
  /**
   * Parse version from integer (0-15).
   */
  def fromInt(v: Int): UuidVersion = v match
    case 1 => V1
    case 2 => V2
    case 3 => V3
    case 4 => V4
    case 5 => V5
    case 6 => V6
    case 7 => V7
    case 8 => V8
    case _ => Unknown

/**
 * UUID variant as per RFC 4122.
 */
enum UuidVariant:
  case NcsReserved      // Reserved for NCS backward compatibility
  case Rfc4122          // The variant specified in RFC 4122
  case MicrosoftReserved // Reserved for Microsoft backward compatibility
  case FutureReserved   // Reserved for future definition
  case Unknown

object UuidVariant:
  /**
   * Parse variant from the variant bits.
   */
  def fromBits(variantByte: Byte): UuidVariant =
    val v = (variantByte & 0xFF) >> 4
    if (v & 0x8) == 0 then NcsReserved
    else if (v & 0xC) == 0x8 then Rfc4122
    else if (v & 0xE) == 0xC then MicrosoftReserved
    else if (v & 0xE) == 0xE then FutureReserved
    else Unknown

/**
 * Represents a validated UUID with parsed components.
 *
 * @param timeLow          The low field of the timestamp (32 bits)
 * @param timeMid          The middle field of the timestamp (16 bits)
 * @param timeHiAndVersion The high field of the timestamp multiplexed with version (16 bits)
 * @param clockSeqHiAndRes The high field of clock sequence multiplexed with variant (8 bits)
 * @param clockSeqLow      The low field of clock sequence (8 bits)
 * @param node             The spatially unique node identifier (48 bits)
 */
case class Uuid(
    timeLow: Long,
    timeMid: Int,
    timeHiAndVersion: Int,
    clockSeqHiAndRes: Int,
    clockSeqLow: Int,
    node: Long
):
  /**
   * Get the UUID version.
   */
  def version: UuidVersion =
    UuidVersion.fromInt((timeHiAndVersion >> 12) & 0x0F)

  /**
   * Get the UUID variant.
   */
  def variant: UuidVariant =
    UuidVariant.fromBits(clockSeqHiAndRes.toByte)

  /**
   * Convert to the standard hyphenated string format.
   */
  def format: String =
    f"${timeLow}%08x-${timeMid}%04x-${timeHiAndVersion}%04x-${clockSeqHiAndRes}%02x${clockSeqLow}%02x-${node}%012x"

  /**
   * Convert to uppercase hyphenated string format.
   */
  def formatUppercase: String = format.toUpperCase

  /**
   * Convert to compact format (no hyphens).
   */
  def formatCompact: String =
    f"${timeLow}%08x${timeMid}%04x${timeHiAndVersion}%04x${clockSeqHiAndRes}%02x${clockSeqLow}%02x${node}%012x"

  /**
   * Convert to URN format.
   */
  def formatUrn: String = s"urn:uuid:$format"

  /**
   * Convert to byte array (16 bytes, big-endian).
   */
  def toBytes: Array[Byte] =
    val bytes = new Array[Byte](16)
    bytes(0) = ((timeLow >> 24) & 0xFF).toByte
    bytes(1) = ((timeLow >> 16) & 0xFF).toByte
    bytes(2) = ((timeLow >> 8) & 0xFF).toByte
    bytes(3) = (timeLow & 0xFF).toByte
    bytes(4) = ((timeMid >> 8) & 0xFF).toByte
    bytes(5) = (timeMid & 0xFF).toByte
    bytes(6) = ((timeHiAndVersion >> 8) & 0xFF).toByte
    bytes(7) = (timeHiAndVersion & 0xFF).toByte
    bytes(8) = clockSeqHiAndRes.toByte
    bytes(9) = clockSeqLow.toByte
    bytes(10) = ((node >> 40) & 0xFF).toByte
    bytes(11) = ((node >> 32) & 0xFF).toByte
    bytes(12) = ((node >> 24) & 0xFF).toByte
    bytes(13) = ((node >> 16) & 0xFF).toByte
    bytes(14) = ((node >> 8) & 0xFF).toByte
    bytes(15) = (node & 0xFF).toByte
    bytes

  /**
   * Convert to java.util.UUID.
   */
  def toJavaUuid: JavaUUID =
    val msb = (timeLow << 32) | (timeMid.toLong << 16) | timeHiAndVersion.toLong
    val lsb = (clockSeqHiAndRes.toLong << 56) | (clockSeqLow.toLong << 48) | node
    new JavaUUID(msb, lsb)

  /**
   * Check if this is the nil UUID (all zeros).
   */
  def isNil: Boolean =
    timeLow == 0 && timeMid == 0 && timeHiAndVersion == 0 &&
      clockSeqHiAndRes == 0 && clockSeqLow == 0 && node == 0

  /**
   * Check if this is the max UUID (all ones).
   */
  def isMax: Boolean =
    timeLow == 0xFFFFFFFFL && timeMid == 0xFFFF && timeHiAndVersion == 0xFFFF &&
      clockSeqHiAndRes == 0xFF && clockSeqLow == 0xFF && node == 0xFFFFFFFFFFFFL

  override def toString: String = format

object Uuid:
  private val secureRandom = new SecureRandom()

  private val HexPattern = """^[0-9a-fA-F]+$""".r

  /** The nil UUID (all zeros). */
  val Nil: Uuid = Uuid(0, 0, 0, 0, 0, 0)

  /** The max UUID (all ones). */
  val Max: Uuid = Uuid(0xFFFFFFFFL, 0xFFFF, 0xFFFF, 0xFF, 0xFF, 0xFFFFFFFFFFFFL)

  /**
   * Parse a UUID string in various formats.
   * Supports: standard (8-4-4-4-12), compact (32 hex), URN (urn:uuid:...), braced ({...}).
   */
  def parse(input: String): Option[Uuid] =
    val trimmed = input.trim

    // Handle URN format
    val withoutUrn = if trimmed.toLowerCase.startsWith("urn:uuid:") then
      trimmed.substring(9)
    else
      trimmed

    // Handle braced format
    val withoutBraces = if withoutUrn.startsWith("{") && withoutUrn.endsWith("}") then
      withoutUrn.substring(1, withoutUrn.length - 1)
    else
      withoutUrn

    // Remove hyphens
    val hexOnly = withoutBraces.replace("-", "")

    // Validate length and characters
    if hexOnly.length != 32 then return None
    if HexPattern.findFirstIn(hexOnly).isEmpty then return None

    Try {
      val timeLow = java.lang.Long.parseUnsignedLong(hexOnly.substring(0, 8), 16)
      val timeMid = Integer.parseUnsignedInt(hexOnly.substring(8, 12), 16)
      val timeHiAndVersion = Integer.parseUnsignedInt(hexOnly.substring(12, 16), 16)
      val clockSeqHiAndRes = Integer.parseUnsignedInt(hexOnly.substring(16, 18), 16)
      val clockSeqLow = Integer.parseUnsignedInt(hexOnly.substring(18, 20), 16)
      val node = java.lang.Long.parseUnsignedLong(hexOnly.substring(20, 32), 16)

      Uuid(timeLow, timeMid, timeHiAndVersion, clockSeqHiAndRes, clockSeqLow, node)
    }.toOption

  /**
   * Create from a byte array (16 bytes, big-endian).
   */
  def fromBytes(bytes: Array[Byte]): Option[Uuid] =
    if bytes.length != 16 then return None

    val timeLow = ((bytes(0) & 0xFFL) << 24) | ((bytes(1) & 0xFFL) << 16) |
      ((bytes(2) & 0xFFL) << 8) | (bytes(3) & 0xFFL)
    val timeMid = ((bytes(4) & 0xFF) << 8) | (bytes(5) & 0xFF)
    val timeHiAndVersion = ((bytes(6) & 0xFF) << 8) | (bytes(7) & 0xFF)
    val clockSeqHiAndRes = bytes(8) & 0xFF
    val clockSeqLow = bytes(9) & 0xFF
    val node = ((bytes(10) & 0xFFL) << 40) | ((bytes(11) & 0xFFL) << 32) |
      ((bytes(12) & 0xFFL) << 24) | ((bytes(13) & 0xFFL) << 16) |
      ((bytes(14) & 0xFFL) << 8) | (bytes(15) & 0xFFL)

    Some(Uuid(timeLow, timeMid, timeHiAndVersion, clockSeqHiAndRes, clockSeqLow, node))

  /**
   * Create from java.util.UUID.
   */
  def fromJavaUuid(uuid: JavaUUID): Uuid =
    val msb = uuid.getMostSignificantBits
    val lsb = uuid.getLeastSignificantBits

    val timeLow = (msb >> 32) & 0xFFFFFFFFL
    val timeMid = ((msb >> 16) & 0xFFFF).toInt
    val timeHiAndVersion = (msb & 0xFFFF).toInt
    val clockSeqHiAndRes = ((lsb >> 56) & 0xFF).toInt
    val clockSeqLow = ((lsb >> 48) & 0xFF).toInt
    val node = lsb & 0xFFFFFFFFFFFFL

    Uuid(timeLow, timeMid, timeHiAndVersion, clockSeqHiAndRes, clockSeqLow, node)

  /**
   * Generate a random UUID (version 4).
   */
  def v4(): Uuid =
    val bytes = new Array[Byte](16)
    secureRandom.nextBytes(bytes)

    // Set version to 4
    bytes(6) = ((bytes(6) & 0x0F) | 0x40).toByte

    // Set variant to RFC 4122
    bytes(8) = ((bytes(8) & 0x3F) | 0x80).toByte

    fromBytes(bytes).get

  /**
   * Generate a random UUID (version 4) - alias for v4().
   */
  def random(): Uuid = v4()

/**
 * Safe UUID operations with validation and parsing.
 */
object SafeUuid:

  /**
   * Check if string is a valid UUID.
   */
  def isValid(input: String): Boolean = Uuid.parse(input).isDefined

  /**
   * Parse a UUID string.
   */
  def parse(input: String): Option[Uuid] = Uuid.parse(input)

  /**
   * Generate a new random UUID (version 4).
   */
  def generate(): Uuid = Uuid.v4()

  /**
   * Generate a new random UUID as a string.
   */
  def generateString(): String = Uuid.v4().format

  /**
   * Format a UUID to the standard hyphenated lowercase format.
   */
  def format(uuid: Uuid): String = uuid.format

  /**
   * Format a UUID to uppercase.
   */
  def formatUppercase(uuid: Uuid): String = uuid.formatUppercase

  /**
   * Format a UUID to compact format (no hyphens).
   */
  def formatCompact(uuid: Uuid): String = uuid.formatCompact

  /**
   * Format a UUID to URN format.
   */
  def formatUrn(uuid: Uuid): String = uuid.formatUrn

  /**
   * Get the nil UUID.
   */
  def nil: Uuid = Uuid.Nil

  /**
   * Get the max UUID.
   */
  def max: Uuid = Uuid.Max

  /**
   * Check if a string represents the nil UUID.
   */
  def isNil(input: String): Boolean =
    Uuid.parse(input).exists(_.isNil)

  /**
   * Check if a string represents the max UUID.
   */
  def isMax(input: String): Boolean =
    Uuid.parse(input).exists(_.isMax)

  /**
   * Get the version of a UUID string.
   */
  def getVersion(input: String): Option[UuidVersion] =
    Uuid.parse(input).map(_.version)

  /**
   * Get the variant of a UUID string.
   */
  def getVariant(input: String): Option[UuidVariant] =
    Uuid.parse(input).map(_.variant)

  /**
   * Compare two UUID strings for equality (case-insensitive, format-agnostic).
   */
  def areEqual(a: String, b: String): Boolean =
    (Uuid.parse(a), Uuid.parse(b)) match
      case (Some(uuidA), Some(uuidB)) => uuidA == uuidB
      case _ => false

  /**
   * Normalize a UUID string to lowercase hyphenated format.
   */
  def normalize(input: String): Option[String] =
    Uuid.parse(input).map(_.format)

  /**
   * Convert UUID string to byte array.
   */
  def toBytes(input: String): Option[Array[Byte]] =
    Uuid.parse(input).map(_.toBytes)

  /**
   * Create UUID from byte array.
   */
  def fromBytes(bytes: Array[Byte]): Option[Uuid] =
    Uuid.fromBytes(bytes)
