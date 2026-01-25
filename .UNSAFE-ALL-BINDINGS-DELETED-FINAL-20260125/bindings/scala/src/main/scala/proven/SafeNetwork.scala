// SPDX-License-Identifier: PMPL-1.0

package proven

import java.net.{InetAddress, URI}
import scala.util.Try

/**
 * Classification of IP addresses.
 */
sealed trait IpClassification
object IpClassification {
  case object Loopback extends IpClassification
  case object Private extends IpClassification
  case object Reserved extends IpClassification
  case object Public extends IpClassification
  case object Invalid extends IpClassification
}

/**
 * Parsed IPv4 address.
 */
case class IPv4Address(a: Int, b: Int, c: Int, d: Int) {

  /** Convert to integer representation */
  def toInt: Long = ((a.toLong & 0xFF) << 24) | ((b & 0xFF) << 16) | ((c & 0xFF) << 8) | (d & 0xFF)

  /** Check if this is a loopback address (127.0.0.0/8) */
  def isLoopback: Boolean = a == 127

  /** Check if this is a private address (RFC 1918) */
  def isPrivate: Boolean = {
    // 10.0.0.0/8
    if (a == 10) return true

    // 172.16.0.0/12
    if (a == 172 && b >= 16 && b <= 31) return true

    // 192.168.0.0/16
    if (a == 192 && b == 168) return true

    false
  }

  /** Check if this is a reserved address */
  def isReserved: Boolean = {
    // 0.0.0.0/8
    if (a == 0) return true

    // 100.64.0.0/10 - CGNAT
    if (a == 100 && b >= 64 && b <= 127) return true

    // 169.254.0.0/16 - Link-local
    if (a == 169 && b == 254) return true

    // 192.0.0.0/24 - IETF
    if (a == 192 && b == 0 && c == 0) return true

    // 192.0.2.0/24 - TEST-NET-1
    if (a == 192 && b == 0 && c == 2) return true

    // 198.51.100.0/24 - TEST-NET-2
    if (a == 198 && b == 51 && c == 100) return true

    // 203.0.113.0/24 - TEST-NET-3
    if (a == 203 && b == 0 && c == 113) return true

    // 224.0.0.0/4 - Multicast
    if (a >= 224 && a <= 239) return true

    // 240.0.0.0/4 - Reserved
    if (a >= 240) return true

    false
  }

  /** Check if this is a public address */
  def isPublic: Boolean = !isLoopback && !isPrivate && !isReserved

  /** Get classification of this address */
  def classification: IpClassification = {
    if (isLoopback) IpClassification.Loopback
    else if (isPrivate) IpClassification.Private
    else if (isReserved) IpClassification.Reserved
    else IpClassification.Public
  }

  /** Check if address is in a CIDR range */
  def isInRange(network: IPv4Address, prefixLength: Int): Boolean = {
    if (prefixLength < 0 || prefixLength > 32) return false

    val mask = if (prefixLength == 0) 0L else (0xFFFFFFFFL << (32 - prefixLength))
    (toInt & mask) == (network.toInt & mask)
  }

  override def toString: String = s"$a.$b.$c.$d"
}

object IPv4Address {
  private val OctetPattern = """^[0-9]+$""".r

  /**
   * Parse an IPv4 address string.
   */
  def parse(address: String): Option[IPv4Address] = {
    val parts = address.split("\\.")
    if (parts.length != 4) return None

    val octets = parts.flatMap { part =>
      // Check for empty parts
      if (part.isEmpty) return None

      // Check for leading zeros (invalid in strict parsing)
      if (part.length > 1 && part.startsWith("0")) return None

      // Check for non-digit characters
      if (OctetPattern.findFirstIn(part).isEmpty) return None

      // Parse and validate range
      Try(part.toInt).toOption.filter(o => o >= 0 && o <= 255)
    }

    if (octets.length != 4) None
    else Some(IPv4Address(octets(0), octets(1), octets(2), octets(3)))
  }
}

/**
 * Safe network validation and operations.
 */
object SafeNetwork {

  /**
   * Check if string is a valid IPv4 address.
   */
  def isValidIPv4(address: String): Boolean = IPv4Address.parse(address).isDefined

  /**
   * Check if string is a valid IPv6 address.
   */
  def isValidIPv6(address: String): Boolean = {
    Try {
      val inet = InetAddress.getByName(address)
      inet.getAddress.length == 16 && address.contains(":")
    }.getOrElse(false)
  }

  /**
   * Check if string is any valid IP address.
   */
  def isValidIP(address: String): Boolean = isValidIPv4(address) || isValidIPv6(address)

  /**
   * Parse an IPv4 address.
   */
  def parseIPv4(address: String): Option[IPv4Address] = IPv4Address.parse(address)

  /**
   * Classify an IPv4 address.
   */
  def classifyIPv4(address: String): IpClassification = {
    IPv4Address.parse(address) match {
      case Some(ip) => ip.classification
      case None => IpClassification.Invalid
    }
  }

  /**
   * Check if port number is valid (1-65535).
   */
  def isValidPort(port: Int): Boolean = port >= 1 && port <= 65535

  /**
   * Check if port is privileged (< 1024).
   */
  def isPrivilegedPort(port: Int): Boolean = port >= 1 && port < 1024

  /**
   * Check if host:port string is valid.
   */
  def isValidHostPort(hostPort: String): Boolean = {
    val lastColon = hostPort.lastIndexOf(':')
    if (lastColon == -1) return false

    val host = hostPort.substring(0, lastColon)
    val portStr = hostPort.substring(lastColon + 1)

    val port = Try(portStr.toInt).toOption
    if (port.isEmpty || !isValidPort(port.get)) return false

    isValidIP(host) || isValidHostname(host)
  }

  /**
   * Check if string is a valid hostname.
   */
  def isValidHostname(hostname: String): Boolean = {
    if (hostname.isEmpty || hostname.length > 253) return false

    val labels = hostname.split("\\.")
    labels.forall { label =>
      label.nonEmpty && label.length <= 63 &&
        """^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$""".r.findFirstIn(label).isDefined
    }
  }

  /**
   * Check if string is a valid URL.
   */
  def isValidUrl(url: String): Boolean = {
    Try {
      val uri = new URI(url)
      uri.getScheme != null && (uri.getScheme == "http" || uri.getScheme == "https")
    }.getOrElse(false)
  }

  /**
   * Parse a URL safely.
   */
  def parseUrl(url: String): Option[URI] = {
    Try(new URI(url)).toOption.filter(_.getScheme != null)
  }

  /**
   * Check if URL host is a private IP (SSRF protection).
   */
  def isPrivateUrl(url: String): Boolean = {
    parseUrl(url).exists { uri =>
      val host = uri.getHost
      if (host == null) return false

      // Check for localhost
      if (host == "localhost" || host == "127.0.0.1" || host == "::1") {
        return true
      }

      // Check IPv4
      IPv4Address.parse(host).exists { ip =>
        ip.isPrivate || ip.isLoopback || ip.isReserved
      }
    }
  }

  /**
   * Format IPv4 from octets.
   */
  def formatIPv4(a: Int, b: Int, c: Int, d: Int): String = s"$a.$b.$c.$d"
}
