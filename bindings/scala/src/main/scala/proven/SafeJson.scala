// SPDX-License-Identifier: PMPL-1.0

package proven

import scala.util.Try

/**
 * JSON value types.
 */
enum JsonValue:
  case JsonNull
  case JsonBool(value: Boolean)
  case JsonNumber(value: Double)
  case JsonString(value: String)
  case JsonArray(values: Seq[JsonValue])
  case JsonObject(fields: Map[String, JsonValue])

  /**
   * Get value as String if it is a string.
   */
  def asString: Option[String] = this match
    case JsonString(s) => Some(s)
    case _ => None

  /**
   * Get value as Double if it is a number.
   */
  def asNumber: Option[Double] = this match
    case JsonNumber(n) => Some(n)
    case _ => None

  /**
   * Get value as Boolean if it is a boolean.
   */
  def asBool: Option[Boolean] = this match
    case JsonBool(b) => Some(b)
    case _ => None

  /**
   * Get value as array if it is an array.
   */
  def asArray: Option[Seq[JsonValue]] = this match
    case JsonArray(arr) => Some(arr)
    case _ => None

  /**
   * Get value as object if it is an object.
   */
  def asObject: Option[Map[String, JsonValue]] = this match
    case JsonObject(obj) => Some(obj)
    case _ => None

  /**
   * Check if value is null.
   */
  def isNull: Boolean = this == JsonNull

  /**
   * Format as JSON string.
   */
  def format: String = this match
    case JsonNull => "null"
    case JsonBool(true) => "true"
    case JsonBool(false) => "false"
    case JsonNumber(n) =>
      if n.isWhole then n.toLong.toString else n.toString
    case JsonString(s) => "\"" + SafeJson.escapeString(s) + "\""
    case JsonArray(arr) => "[" + arr.map(_.format).mkString(",") + "]"
    case JsonObject(obj) =>
      "{" + obj.map { case (k, v) => "\"" + SafeJson.escapeString(k) + "\":" + v.format }.mkString(",") + "}"

/**
 * Safe JSON parsing and validation.
 */
object SafeJson:

  /**
   * Validate JSON syntax without full parsing.
   * Returns true if the string appears to be valid JSON.
   */
  def isValid(s: String): Boolean =
    var depthBrace = 0
    var depthBracket = 0
    var inString = false
    var escape = false

    for c <- s do
      if escape then
        escape = false
      else c match
        case '\\' if inString => escape = true
        case '"' => inString = !inString
        case '{' if !inString => depthBrace += 1
        case '}' if !inString => depthBrace -= 1
        case '[' if !inString => depthBracket += 1
        case ']' if !inString => depthBracket -= 1
        case _ =>

      if depthBrace < 0 || depthBracket < 0 then
        return false

    depthBrace == 0 && depthBracket == 0 && !inString

  /**
   * Check if string is a valid JSON object.
   */
  def isValidObject(s: String): Boolean =
    val trimmed = s.trim
    trimmed.startsWith("{") && trimmed.endsWith("}") && isValid(s)

  /**
   * Check if string is a valid JSON array.
   */
  def isValidArray(s: String): Boolean =
    val trimmed = s.trim
    trimmed.startsWith("[") && trimmed.endsWith("]") && isValid(s)

  /**
   * Escape a string for JSON.
   */
  def escapeString(s: String): String =
    val sb = new StringBuilder
    for c <- s do
      c match
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c if c < ' ' => sb.append("\\u%04x".format(c.toInt))
        case c => sb.append(c)
    sb.toString

  /**
   * Unescape a JSON string.
   */
  def unescapeString(s: String): Option[String] =
    Try {
      val sb = new StringBuilder
      var i = 0
      while i < s.length do
        s.charAt(i) match
          case '\\' if i + 1 < s.length =>
            s.charAt(i + 1) match
              case '"' => sb.append('"'); i += 2
              case '\\' => sb.append('\\'); i += 2
              case '/' => sb.append('/'); i += 2
              case 'b' => sb.append('\b'); i += 2
              case 'f' => sb.append('\f'); i += 2
              case 'n' => sb.append('\n'); i += 2
              case 'r' => sb.append('\r'); i += 2
              case 't' => sb.append('\t'); i += 2
              case 'u' if i + 5 < s.length =>
                val hex = s.substring(i + 2, i + 6)
                sb.append(Integer.parseInt(hex, 16).toChar)
                i += 6
              case _ => sb.append(s.charAt(i)); i += 1
          case c => sb.append(c); i += 1
      sb.toString
    }.toOption

  /**
   * Simple JSON path access (e.g., "user.name" or "items[0].id").
   * Returns the raw string value at the path.
   */
  def getPath(json: String, path: String): Option[String] =
    // Very basic implementation - for production use a proper JSON library
    if !isValid(json) then return None

    val parts = path.split("\\.")
    var current = json.trim

    for part <- parts do
      // Handle array index
      val arrayPattern = """(\w+)\[(\d+)\]""".r
      part match
        case arrayPattern(key, idx) =>
          // First navigate to key, then to array index
          current = extractField(current, key).getOrElse(return None)
          current = extractArrayElement(current, idx.toInt).getOrElse(return None)
        case key =>
          current = extractField(current, key).getOrElse(return None)

    Some(current)

  private def extractField(json: String, key: String): Option[String] =
    val pattern = s""""$key"\\s*:\\s*""".r
    pattern.findFirstMatchIn(json).map { m =>
      val start = m.end
      extractValue(json, start)
    }

  private def extractArrayElement(json: String, index: Int): Option[String] =
    if !json.trim.startsWith("[") then return None
    var depth = 0
    var inString = false
    var escape = false
    var elementStart = 1
    var elementIndex = 0

    for (c, i) <- json.zipWithIndex.drop(1) do
      if escape then
        escape = false
      else c match
        case '\\' if inString => escape = true
        case '"' => inString = !inString
        case '[' | '{' if !inString => depth += 1
        case ']' | '}' if !inString =>
          if depth == 0 then
            if elementIndex == index then
              return Some(json.substring(elementStart, i).trim)
            return None
          depth -= 1
        case ',' if !inString && depth == 0 =>
          if elementIndex == index then
            return Some(json.substring(elementStart, i).trim)
          elementIndex += 1
          elementStart = i + 1
        case _ =>
    None

  private def extractValue(json: String, start: Int): String =
    var depth = 0
    var inString = false
    var escape = false
    var end = start

    while end < json.length do
      val c = json.charAt(end)
      if escape then
        escape = false
        end += 1
      else c match
        case '\\' if inString => escape = true; end += 1
        case '"' => inString = !inString; end += 1
        case '[' | '{' if !inString => depth += 1; end += 1
        case ']' | '}' if !inString =>
          if depth == 0 then return json.substring(start, end).trim
          depth -= 1; end += 1
        case ',' if !inString && depth == 0 =>
          return json.substring(start, end).trim
        case _ => end += 1

    json.substring(start).trim

  /**
   * Create a JSON string value.
   */
  def string(s: String): JsonValue = JsonValue.JsonString(s)

  /**
   * Create a JSON number value.
   */
  def number(n: Double): JsonValue = JsonValue.JsonNumber(n)

  /**
   * Create a JSON boolean value.
   */
  def bool(b: Boolean): JsonValue = JsonValue.JsonBool(b)

  /**
   * Create a JSON null value.
   */
  def nil: JsonValue = JsonValue.JsonNull

  /**
   * Create a JSON array.
   */
  def array(values: JsonValue*): JsonValue = JsonValue.JsonArray(values)

  /**
   * Create a JSON object.
   */
  def obj(fields: (String, JsonValue)*): JsonValue = JsonValue.JsonObject(fields.toMap)
