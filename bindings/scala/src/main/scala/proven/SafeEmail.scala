// SPDX-License-Identifier: PMPL-1.0

package proven

/**
 * Email validation result with parsed components.
 */
sealed trait EmailResult {
  def isOk: Boolean
  def isError: Boolean = !isOk
  def localPart: Option[String]
  def domain: Option[String]
  def error: Option[String]

  def email: Option[String] = for {
    l <- localPart
    d <- domain
  } yield s"$l@$d"
}

object EmailResult {
  case class Ok(local: String, dom: String) extends EmailResult {
    override def isOk: Boolean = true
    override def localPart: Option[String] = Some(local)
    override def domain: Option[String] = Some(dom)
    override def error: Option[String] = None
  }

  case class Error(msg: String) extends EmailResult {
    override def isOk: Boolean = false
    override def localPart: Option[String] = None
    override def domain: Option[String] = None
    override def error: Option[String] = Some(msg)
  }

  def ok(localPart: String, domain: String): EmailResult = Ok(localPart, domain)
  def error(msg: String): EmailResult = Error(msg)
}

/**
 * Safe email validation and manipulation.
 */
object SafeEmail {

  /** Common disposable email domains */
  val DisposableDomains: Set[String] = Set(
    "tempmail.com",
    "throwaway.email",
    "guerrillamail.com",
    "mailinator.com",
    "10minutemail.com",
    "temp-mail.org",
    "fakeinbox.com",
    "trashmail.com",
    "yopmail.com",
    "sharklasers.com",
    "getairmail.com",
    "tempail.com",
    "discard.email",
    "maildrop.cc"
  )

  private val LocalPartPattern = """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+$""".r
  private val DomainPattern = """^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)*$""".r

  /**
   * Validate an email address (basic check).
   */
  def isValid(email: String): Boolean = parse(email).isOk

  /**
   * Parse and validate an email address.
   */
  def parse(email: String): EmailResult = {
    // Check for empty
    if (email.isEmpty) {
      return EmailResult.error("empty_email")
    }

    // Check length
    if (email.length > 254) {
      return EmailResult.error("email_too_long")
    }

    // Check for @ symbol
    val atIndex = email.indexOf('@')
    if (atIndex == -1) {
      return EmailResult.error("missing_at_symbol")
    }

    // Check for multiple @
    if (email.indexOf('@', atIndex + 1) != -1) {
      return EmailResult.error("multiple_at_symbols")
    }

    // Split at @
    val localPart = email.substring(0, atIndex)
    val domain = email.substring(atIndex + 1)

    // Validate local part
    if (localPart.isEmpty) {
      return EmailResult.error("empty_local_part")
    }
    if (localPart.length > 64) {
      return EmailResult.error("local_part_too_long")
    }
    if (localPart.startsWith(".") || localPart.endsWith(".")) {
      return EmailResult.error("local_part_dot_position")
    }
    if (localPart.contains("..")) {
      return EmailResult.error("local_part_consecutive_dots")
    }
    if (LocalPartPattern.findFirstIn(localPart).isEmpty) {
      return EmailResult.error("invalid_local_part_chars")
    }

    // Validate domain
    if (domain.isEmpty) {
      return EmailResult.error("empty_domain")
    }
    if (domain.length > 253) {
      return EmailResult.error("domain_too_long")
    }

    // Domain must have a dot (unless localhost)
    if (!domain.contains('.') && domain.toLowerCase != "localhost") {
      return EmailResult.error("domain_missing_dot")
    }

    // Domain can't start or end with dot or hyphen
    if (domain.startsWith(".") || domain.endsWith(".") ||
        domain.startsWith("-") || domain.endsWith("-")) {
      return EmailResult.error("invalid_domain_format")
    }

    if (DomainPattern.findFirstIn(domain).isEmpty) {
      return EmailResult.error("invalid_domain_chars")
    }

    EmailResult.ok(localPart, domain)
  }

  /**
   * Extract domain from email.
   */
  def getDomain(email: String): Option[String] = parse(email).domain

  /**
   * Extract local part from email.
   */
  def getLocalPart(email: String): Option[String] = parse(email).localPart

  /**
   * Normalize email (lowercase domain, preserve local part case).
   */
  def normalize(email: String): Option[String] = {
    parse(email) match {
      case EmailResult.Ok(local, dom) => Some(s"$local@${dom.toLowerCase}")
      case _ => None
    }
  }

  /**
   * Fully normalize email (lowercase everything).
   */
  def normalizeFull(email: String): Option[String] = {
    parse(email) match {
      case EmailResult.Ok(local, dom) =>
        Some(s"${local.toLowerCase}@${dom.toLowerCase}")
      case _ => None
    }
  }

  /**
   * Check if email is from a disposable service.
   */
  def isDisposable(email: String): Boolean = {
    getDomain(email).exists(d => DisposableDomains.contains(d.toLowerCase))
  }

  /**
   * Check if email is from a specific domain.
   */
  def isFromDomain(email: String, expectedDomain: String): Boolean = {
    getDomain(email).exists(_.toLowerCase == expectedDomain.toLowerCase)
  }

  /**
   * Check if email is from one of a list of allowed domains.
   */
  def isFromAllowedDomain(email: String, allowedDomains: Seq[String]): Boolean = {
    getDomain(email).exists { domain =>
      allowedDomains.exists(_.toLowerCase == domain.toLowerCase)
    }
  }

  /**
   * Get the TLD (top-level domain) of an email.
   */
  def getTld(email: String): Option[String] = {
    getDomain(email).flatMap { domain =>
      val lastDot = domain.lastIndexOf('.')
      if (lastDot == -1) None
      else Some(domain.substring(lastDot + 1).toLowerCase)
    }
  }

  /**
   * Obfuscate email for display (e.g., "u***r@example.com").
   */
  def obfuscate(email: String): Option[String] = {
    parse(email) match {
      case EmailResult.Ok(local, dom) =>
        val obfuscatedLocal = if (local.length <= 2) {
          "***"
        } else {
          s"${local.head}***${local.last}"
        }
        Some(s"$obfuscatedLocal@$dom")
      case _ => None
    }
  }

  /**
   * Check if two emails are equivalent (after normalization).
   */
  def areEquivalent(email1: String, email2: String): Boolean = {
    (normalizeFull(email1), normalizeFull(email2)) match {
      case (Some(n1), Some(n2)) => n1 == n2
      case _ => false
    }
  }
}
