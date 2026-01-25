// SPDX-License-Identifier: PMPL-1.0

package proven

import java.net.{URI, URLEncoder, URLDecoder}
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Parsed URL components.
 */
case class ParsedUrl(
    scheme: String,
    username: Option[String],
    password: Option[String],
    host: String,
    port: Option[Int],
    path: String,
    query: Option[String],
    fragment: Option[String]
):
  /**
   * Reconstruct the URL string.
   */
  def format: String =
    val sb = new StringBuilder
    sb.append(scheme).append("://")

    username.foreach { u =>
      sb.append(u)
      password.foreach(p => sb.append(":").append(p))
      sb.append("@")
    }

    sb.append(host)
    port.foreach(p => sb.append(":").append(p))
    sb.append(path)
    query.foreach(q => sb.append("?").append(q))
    fragment.foreach(f => sb.append("#").append(f))

    sb.toString

  /**
   * Get the origin (scheme + host + port).
   */
  def origin: String =
    val portPart = port.map(p => s":$p").getOrElse("")
    s"$scheme://$host$portPart"

  /**
   * Check if URL uses HTTPS.
   */
  def isSecure: Boolean = scheme.toLowerCase == "https"

  /**
   * Get query parameters as a map.
   */
  def queryParams: Map[String, String] =
    query.map { q =>
      q.split("&").flatMap { pair =>
        pair.split("=", 2) match
          case Array(key, value) => Some(key -> value)
          case Array(key) => Some(key -> "")
          case _ => None
      }.toMap
    }.getOrElse(Map.empty)

/**
 * Safe URL parsing and manipulation.
 */
object SafeUrl:

  /**
   * Parse a URL string.
   */
  def parse(url: String): Option[ParsedUrl] =
    val trimmed = url.trim
    if trimmed.isEmpty then return None

    Try {
      // Find scheme
      val schemeEnd = trimmed.indexOf("://")
      if schemeEnd == -1 then return None

      val scheme = trimmed.substring(0, schemeEnd).toLowerCase
      var rest = trimmed.substring(schemeEnd + 3)

      // Extract fragment
      val (restWithoutFragment, fragment) = rest.indexOf('#') match
        case -1 => (rest, None)
        case i => (rest.substring(0, i), Some(rest.substring(i + 1)))

      rest = restWithoutFragment

      // Extract query
      val (restWithoutQuery, query) = rest.indexOf('?') match
        case -1 => (rest, None)
        case i => (rest.substring(0, i), Some(rest.substring(i + 1)))

      rest = restWithoutQuery

      // Extract path
      val (authority, path) = rest.indexOf('/') match
        case -1 => (rest, "/")
        case i => (rest.substring(0, i), rest.substring(i))

      // Extract userinfo
      val (userinfo, hostport) = authority.lastIndexOf('@') match
        case -1 => (None, authority)
        case i => (Some(authority.substring(0, i)), authority.substring(i + 1))

      val (username, password) = userinfo match
        case None => (None, None)
        case Some(u) => u.indexOf(':') match
          case -1 => (Some(u), None)
          case i => (Some(u.substring(0, i)), Some(u.substring(i + 1)))

      // Extract port
      val (host, port) = if hostport.startsWith("[") then
        // IPv6
        val closeBracket = hostport.indexOf(']')
        if closeBracket == -1 then return None
        val h = hostport.substring(1, closeBracket)
        val p = hostport.substring(closeBracket + 1) match
          case s if s.startsWith(":") => Try(s.substring(1).toInt).toOption
          case _ => None
        (h, p)
      else
        hostport.lastIndexOf(':') match
          case -1 => (hostport, None)
          case i =>
            val portStr = hostport.substring(i + 1)
            Try(portStr.toInt).toOption match
              case Some(p) => (hostport.substring(0, i), Some(p))
              case None => (hostport, None)

      Some(ParsedUrl(
        scheme = scheme,
        username = username,
        password = password,
        host = host,
        port = port,
        path = path,
        query = query,
        fragment = fragment
      ))
    }.toOption.flatten

  /**
   * Check if a string is a valid URL.
   */
  def isValid(url: String): Boolean = parse(url).isDefined

  /**
   * Check if URL is valid and uses HTTP or HTTPS.
   */
  def isValidHttpUrl(url: String): Boolean =
    parse(url).exists(p => p.scheme == "http" || p.scheme == "https")

  /**
   * Extract the domain from a URL.
   */
  def getDomain(url: String): Option[String] =
    parse(url).map(_.host)

  /**
   * Extract the path from a URL.
   */
  def getPath(url: String): Option[String] =
    parse(url).map(_.path)

  /**
   * Extract query string from a URL.
   */
  def getQuery(url: String): Option[String] =
    parse(url).flatMap(_.query)

  /**
   * Extract query parameters as a map.
   */
  def getQueryParams(url: String): Map[String, String] =
    parse(url).map(_.queryParams).getOrElse(Map.empty)

  /**
   * URL-encode a string.
   */
  def encode(s: String): String =
    URLEncoder.encode(s, StandardCharsets.UTF_8)

  /**
   * URL-decode a string.
   */
  def decode(s: String): Option[String] =
    Try(URLDecoder.decode(s, StandardCharsets.UTF_8)).toOption

  /**
   * Build a query string from parameters.
   */
  def buildQueryString(params: Map[String, String]): String =
    params.map { case (k, v) => s"${encode(k)}=${encode(v)}" }.mkString("&")

  /**
   * Add query parameters to a URL.
   */
  def addQueryParams(url: String, params: Map[String, String]): Option[String] =
    parse(url).map { parsed =>
      val newQuery = parsed.query match
        case Some(q) => Some(q + "&" + buildQueryString(params))
        case None => Some(buildQueryString(params))
      parsed.copy(query = newQuery).format
    }

  /**
   * Check if URL points to localhost or private IP (SSRF protection).
   */
  def isPrivateUrl(url: String): Boolean =
    parse(url).exists { parsed =>
      val host = parsed.host.toLowerCase
      host == "localhost" ||
      host == "127.0.0.1" ||
      host == "::1" ||
      host.startsWith("10.") ||
      host.startsWith("192.168.") ||
      (host.startsWith("172.") && {
        Try {
          val second = host.split("\\.")(1).toInt
          second >= 16 && second <= 31
        }.getOrElse(false)
      })
    }

  /**
   * Normalize a URL (lowercase scheme and host, remove default port).
   */
  def normalize(url: String): Option[String] =
    parse(url).map { parsed =>
      val normalizedPort = (parsed.scheme, parsed.port) match
        case ("http", Some(80)) => None
        case ("https", Some(443)) => None
        case (_, p) => p
      parsed.copy(
        scheme = parsed.scheme.toLowerCase,
        host = parsed.host.toLowerCase,
        port = normalizedPort
      ).format
    }

  /**
   * Get the base URL (scheme + host + port, no path/query/fragment).
   */
  def getBaseUrl(url: String): Option[String] =
    parse(url).map(_.origin)

  /**
   * Join a base URL with a relative path.
   */
  def join(base: String, relative: String): Option[String] =
    parse(base).map { parsed =>
      if relative.startsWith("/") then
        parsed.copy(path = relative, query = None, fragment = None).format
      else
        val basePath = if parsed.path.endsWith("/") then parsed.path else parsed.path + "/"
        parsed.copy(path = basePath + relative, query = None, fragment = None).format
    }
