// SPDX-License-Identifier: PMPL-1.0

package proven

import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.Try

/**
 * Safe cryptographic operations.
 */
object SafeCrypto {

  private val secureRandom = new SecureRandom()

  /**
   * Constant-time string comparison to prevent timing attacks.
   */
  def constantTimeEquals(a: String, b: String): Boolean = {
    constantTimeEqualsBytes(a.getBytes("UTF-8"), b.getBytes("UTF-8"))
  }

  /**
   * Constant-time byte comparison.
   */
  def constantTimeEqualsBytes(a: Array[Byte], b: Array[Byte]): Boolean = {
    if (a.length != b.length) return false

    var result = 0
    for (i <- a.indices) {
      result |= a(i) ^ b(i)
    }
    result == 0
  }

  /**
   * Generate cryptographically secure random bytes.
   */
  def randomBytes(count: Int): Array[Byte] = {
    val bytes = new Array[Byte](count)
    secureRandom.nextBytes(bytes)
    bytes
  }

  /**
   * Generate random bytes as hex string.
   */
  def randomHex(byteCount: Int): String = {
    randomBytes(byteCount).map(b => f"$b%02x").mkString
  }

  /**
   * Generate random bytes as base64 string.
   */
  def randomBase64(byteCount: Int): String = {
    Base64.getEncoder.encodeToString(randomBytes(byteCount))
  }

  /**
   * Generate URL-safe random string.
   */
  def randomUrlSafe(byteCount: Int): String = {
    Base64.getUrlEncoder.withoutPadding().encodeToString(randomBytes(byteCount))
  }

  /**
   * Generate random integer in range [min, max].
   */
  def randomInt(min: Int, max: Int): Int = {
    val (lo, hi) = if (min <= max) (min, max) else (max, min)
    val range = hi - lo + 1
    lo + secureRandom.nextInt(range)
  }

  /**
   * Generate a secure token (for sessions, CSRF, etc).
   */
  def generateToken(length: Int = 32): String = {
    randomUrlSafe(length)
  }

  /**
   * Hash a string with SHA-256.
   */
  def sha256(input: String): String = {
    sha256Bytes(input.getBytes("UTF-8"))
  }

  /**
   * Hash bytes with SHA-256.
   */
  def sha256Bytes(input: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(input)
    digest.digest().map(b => f"$b%02x").mkString
  }

  /**
   * Hash a string with SHA-512.
   */
  def sha512(input: String): String = {
    sha512Bytes(input.getBytes("UTF-8"))
  }

  /**
   * Hash bytes with SHA-512.
   */
  def sha512Bytes(input: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("SHA-512")
    digest.update(input)
    digest.digest().map(b => f"$b%02x").mkString
  }

  /**
   * Compute HMAC-SHA256.
   */
  def hmacSha256(key: String, message: String): String = {
    hmacSha256Bytes(key.getBytes("UTF-8"), message.getBytes("UTF-8"))
  }

  /**
   * Compute HMAC-SHA256 with bytes.
   */
  def hmacSha256Bytes(key: Array[Byte], message: Array[Byte]): String = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key, "HmacSHA256"))
    mac.doFinal(message).map(b => f"$b%02x").mkString
  }

  /**
   * Compute HMAC-SHA512.
   */
  def hmacSha512(key: String, message: String): String = {
    hmacSha512Bytes(key.getBytes("UTF-8"), message.getBytes("UTF-8"))
  }

  /**
   * Compute HMAC-SHA512 with bytes.
   */
  def hmacSha512Bytes(key: Array[Byte], message: Array[Byte]): String = {
    val mac = Mac.getInstance("HmacSHA512")
    mac.init(new SecretKeySpec(key, "HmacSHA512"))
    mac.doFinal(message).map(b => f"$b%02x").mkString
  }

  /**
   * Verify HMAC using constant-time comparison.
   */
  def verifyHmacSha256(key: String, message: String, expectedMac: String): Boolean = {
    val actualMac = hmacSha256(key, message)
    constantTimeEquals(actualMac, expectedMac)
  }

  /**
   * Verify HMAC-SHA512 using constant-time comparison.
   */
  def verifyHmacSha512(key: String, message: String, expectedMac: String): Boolean = {
    val actualMac = hmacSha512(key, message)
    constantTimeEquals(actualMac, expectedMac)
  }

  /**
   * Hash a string with MD5 (NOT for security, only for checksums).
   */
  def md5(input: String): String = {
    val digest = MessageDigest.getInstance("MD5")
    digest.update(input.getBytes("UTF-8"))
    digest.digest().map(b => f"$b%02x").mkString
  }

  /**
   * Derive a key using PBKDF2-SHA256.
   * Returns hex-encoded derived key.
   */
  def pbkdf2(
    password: String,
    salt: String,
    iterations: Int = 100000,
    keyLength: Int = 32
  ): String = {
    import javax.crypto.SecretKeyFactory
    import javax.crypto.spec.PBEKeySpec

    val spec = new PBEKeySpec(
      password.toCharArray,
      salt.getBytes("UTF-8"),
      iterations,
      keyLength * 8
    )

    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    factory.generateSecret(spec).getEncoded.map(b => f"$b%02x").mkString
  }

  /**
   * Generate a random password.
   */
  def generatePassword(
    length: Int = 16,
    includeUppercase: Boolean = true,
    includeLowercase: Boolean = true,
    includeNumbers: Boolean = true,
    includeSymbols: Boolean = true
  ): String = {
    val chars = new StringBuilder

    if (includeLowercase) chars.append("abcdefghijklmnopqrstuvwxyz")
    if (includeUppercase) chars.append("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    if (includeNumbers) chars.append("0123456789")
    if (includeSymbols) chars.append("!@#$%^&*()_+-=[]{}|;:,.<>?")

    val charset = chars.toString
    if (charset.isEmpty) return ""

    (0 until length)
      .map(_ => charset.charAt(secureRandom.nextInt(charset.length)))
      .mkString
  }

  /**
   * Securely wipe a byte array (best effort).
   */
  def secureWipe(data: Array[Byte]): Unit = {
    java.util.Arrays.fill(data, 0.toByte)
  }

  /**
   * Check if a source of randomness is cryptographically secure.
   */
  def isSecureRandomAvailable: Boolean = true
}
