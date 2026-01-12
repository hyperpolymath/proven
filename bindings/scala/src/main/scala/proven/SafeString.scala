// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import java.net.{URLEncoder, URLDecoder}
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.Try

/**
 * Safe string operations with XSS and injection prevention.
 */
object SafeString {

  private val HtmlEntities: Map[Char, String] = Map(
    '&' -> "&amp;",
    '<' -> "&lt;",
    '>' -> "&gt;",
    '"' -> "&quot;",
    '\'' -> "&#x27;"
  )

  /**
   * Escape string for safe HTML output (XSS prevention).
   */
  def escapeHtml(input: String): String = {
    val sb = new StringBuilder(input.length)
    input.foreach { c =>
      sb.append(HtmlEntities.getOrElse(c, c.toString))
    }
    sb.toString()
  }

  /**
   * Escape string for SQL single quotes.
   * Note: PREFER PARAMETERIZED QUERIES! This is for edge cases only.
   */
  def escapeSql(input: String): String = {
    input.replace("'", "''")
  }

  /**
   * Escape string for JavaScript string context.
   */
  def escapeJs(input: String): String = {
    val sb = new StringBuilder(input.length)
    input.foreach {
      case '\\' => sb.append("\\\\")
      case '\'' => sb.append("\\'")
      case '"' => sb.append("\\\"")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case '<' => sb.append("\\x3C")
      case '>' => sb.append("\\x3E")
      case c => sb.append(c)
    }
    sb.toString()
  }

  /**
   * Escape string for shell command (single-quote wrapping).
   */
  def escapeShell(input: String): String = {
    "'" + input.replace("'", "'\\''") + "'"
  }

  /**
   * Escape special characters for use in regex patterns.
   */
  def escapeRegex(input: String): String = {
    java.util.regex.Pattern.quote(input)
  }

  /**
   * URL-encode a string.
   */
  def urlEncode(input: String): String = {
    URLEncoder.encode(input, StandardCharsets.UTF_8.name())
  }

  /**
   * URL-decode a string.
   */
  def urlDecode(input: String): Option[String] = {
    Try(URLDecoder.decode(input, StandardCharsets.UTF_8.name())).toOption
  }

  /**
   * Base64-encode a string.
   */
  def base64Encode(input: String): String = {
    Base64.getEncoder.encodeToString(input.getBytes(StandardCharsets.UTF_8))
  }

  /**
   * Base64-decode a string.
   */
  def base64Decode(input: String): Option[String] = {
    Try {
      new String(Base64.getDecoder.decode(input), StandardCharsets.UTF_8)
    }.toOption
  }

  /**
   * Safely truncate a string to a maximum length.
   */
  def truncate(input: String, maxLength: Int, suffix: String = "..."): String = {
    if (maxLength <= 0) ""
    else if (input.length <= maxLength) input
    else if (maxLength <= suffix.length) input.take(maxLength)
    else input.take(maxLength - suffix.length) + suffix
  }

  /**
   * Strip HTML tags from string (basic - not a full parser).
   */
  def stripHtml(input: String): String = {
    input.replaceAll("<[^>]*>", "")
  }

  /**
   * Check if string contains only alphanumeric characters.
   */
  def isAlphanumeric(input: String): Boolean = {
    input.nonEmpty && input.forall(_.isLetterOrDigit)
  }

  /**
   * Check if string contains only ASCII characters.
   */
  def isAscii(input: String): Boolean = {
    input.forall(c => c >= 0 && c <= 127)
  }

  /**
   * Sanitize string to contain only specified allowed characters.
   */
  def sanitize(input: String, allowed: String = "a-zA-Z0-9_-"): String = {
    val pattern = s"[^$allowed]".r
    pattern.replaceAllIn(input, "")
  }

  /**
   * Remove control characters from string.
   */
  def removeControlChars(input: String): String = {
    input.filterNot(c =>
      (c >= 0 && c <= 8) || c == 11 || c == 12 || (c >= 14 && c <= 31) || c == 127
    )
  }

  /**
   * Normalize whitespace (collapse multiple spaces, trim).
   */
  def normalizeWhitespace(input: String): String = {
    input.replaceAll("\\s+", " ").trim
  }

  /**
   * Check if string looks like it contains injection attempts.
   */
  def containsSuspiciousPatterns(input: String): Boolean = {
    val lowerInput = input.toLowerCase
    val patterns = Seq(
      "<script",
      "javascript:",
      "union\\s+select",
      ";\\s*drop\\s+table"
    )

    patterns.exists { pattern =>
      pattern.r.findFirstIn(lowerInput).isDefined
    } || "on\\w+\\s*=".r.findFirstIn(lowerInput).isDefined
  }

  /**
   * Escape XML special characters.
   */
  def escapeXml(input: String): String = {
    escapeHtml(input) // Same entities for basic XML escaping
  }

  /**
   * Convert string to slug (URL-friendly format).
   */
  def slugify(input: String): String = {
    input
      .toLowerCase
      .replaceAll("[^a-z0-9\\s-]", "")
      .replaceAll("\\s+", "-")
      .replaceAll("-+", "-")
      .stripPrefix("-")
      .stripSuffix("-")
  }
}
