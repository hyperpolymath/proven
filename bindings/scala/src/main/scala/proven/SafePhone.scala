// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import scala.util.Try

/**
 * Country calling codes with metadata per ITU-T E.164.
 */
enum CountryCode(
    val callingCode: Int,
    val alpha2: String,
    val name: String,
    val trunkPrefix: Option[String] = Some("0"),
    val nationalNumberLength: Range = 6 to 15
):
  // North America (NANP)
  case US extends CountryCode(1, "US", "United States", Some("1"), 10 to 10)
  case CA extends CountryCode(1, "CA", "Canada", Some("1"), 10 to 10)

  // Europe
  case GB extends CountryCode(44, "GB", "United Kingdom", Some("0"), 10 to 10)
  case DE extends CountryCode(49, "DE", "Germany", Some("0"), 10 to 11)
  case FR extends CountryCode(33, "FR", "France", Some("0"), 9 to 9)
  case IT extends CountryCode(39, "IT", "Italy", None, 9 to 11)
  case ES extends CountryCode(34, "ES", "Spain", None, 9 to 9)
  case NL extends CountryCode(31, "NL", "Netherlands", Some("0"), 9 to 9)
  case BE extends CountryCode(32, "BE", "Belgium", Some("0"), 8 to 9)
  case CH extends CountryCode(41, "CH", "Switzerland", Some("0"), 9 to 9)
  case AT extends CountryCode(43, "AT", "Austria", Some("0"), 10 to 13)
  case SE extends CountryCode(46, "SE", "Sweden", Some("0"), 7 to 13)
  case NO extends CountryCode(47, "NO", "Norway", None, 8 to 8)
  case DK extends CountryCode(45, "DK", "Denmark", None, 8 to 8)
  case FI extends CountryCode(358, "FI", "Finland", Some("0"), 6 to 11)
  case PL extends CountryCode(48, "PL", "Poland", None, 9 to 9)
  case PT extends CountryCode(351, "PT", "Portugal", None, 9 to 9)
  case GR extends CountryCode(30, "GR", "Greece", None, 10 to 10)
  case IE extends CountryCode(353, "IE", "Ireland", Some("0"), 7 to 9)
  case CZ extends CountryCode(420, "CZ", "Czech Republic", None, 9 to 9)
  case RO extends CountryCode(40, "RO", "Romania", Some("0"), 9 to 9)
  case HU extends CountryCode(36, "HU", "Hungary", Some("06"), 8 to 9)
  case UA extends CountryCode(380, "UA", "Ukraine", Some("0"), 9 to 9)
  case RU extends CountryCode(7, "RU", "Russia", Some("8"), 10 to 10)

  // Asia
  case JP extends CountryCode(81, "JP", "Japan", Some("0"), 9 to 10)
  case CN extends CountryCode(86, "CN", "China", Some("0"), 11 to 11)
  case IN extends CountryCode(91, "IN", "India", Some("0"), 10 to 10)
  case KR extends CountryCode(82, "KR", "South Korea", Some("0"), 9 to 11)
  case HK extends CountryCode(852, "HK", "Hong Kong", None, 8 to 8)
  case SG extends CountryCode(65, "SG", "Singapore", None, 8 to 8)
  case MY extends CountryCode(60, "MY", "Malaysia", Some("0"), 9 to 10)
  case TH extends CountryCode(66, "TH", "Thailand", Some("0"), 9 to 9)
  case PH extends CountryCode(63, "PH", "Philippines", Some("0"), 10 to 10)
  case ID extends CountryCode(62, "ID", "Indonesia", Some("0"), 9 to 12)
  case VN extends CountryCode(84, "VN", "Vietnam", Some("0"), 9 to 10)
  case TW extends CountryCode(886, "TW", "Taiwan", Some("0"), 9 to 9)
  case PK extends CountryCode(92, "PK", "Pakistan", Some("0"), 10 to 10)
  case BD extends CountryCode(880, "BD", "Bangladesh", Some("0"), 10 to 10)
  case AE extends CountryCode(971, "AE", "United Arab Emirates", Some("0"), 9 to 9)
  case SA extends CountryCode(966, "SA", "Saudi Arabia", Some("0"), 9 to 9)
  case IL extends CountryCode(972, "IL", "Israel", Some("0"), 9 to 9)
  case TR extends CountryCode(90, "TR", "Turkey", Some("0"), 10 to 10)

  // Oceania
  case AU extends CountryCode(61, "AU", "Australia", Some("0"), 9 to 9)
  case NZ extends CountryCode(64, "NZ", "New Zealand", Some("0"), 8 to 10)

  // Americas
  case MX extends CountryCode(52, "MX", "Mexico", Some("01"), 10 to 10)
  case BR extends CountryCode(55, "BR", "Brazil", Some("0"), 10 to 11)
  case AR extends CountryCode(54, "AR", "Argentina", Some("0"), 10 to 10)
  case CO extends CountryCode(57, "CO", "Colombia", Some("0"), 10 to 10)
  case CL extends CountryCode(56, "CL", "Chile", None, 9 to 9)
  case PE extends CountryCode(51, "PE", "Peru", Some("0"), 9 to 9)

  // Africa
  case ZA extends CountryCode(27, "ZA", "South Africa", Some("0"), 9 to 9)
  case EG extends CountryCode(20, "EG", "Egypt", Some("0"), 10 to 10)
  case NG extends CountryCode(234, "NG", "Nigeria", Some("0"), 10 to 10)
  case KE extends CountryCode(254, "KE", "Kenya", Some("0"), 9 to 9)
  case MA extends CountryCode(212, "MA", "Morocco", Some("0"), 9 to 9)

object CountryCode:
  private val byAlpha2: Map[String, CountryCode] =
    CountryCode.values.map(c => c.alpha2 -> c).toMap

  private val byCallingCode: Map[Int, Seq[CountryCode]] =
    CountryCode.values.groupBy(_.callingCode).view.mapValues(_.toSeq).toMap

  /**
   * Get country by ISO 3166-1 alpha-2 code.
   */
  def fromAlpha2(code: String): Option[CountryCode] =
    byAlpha2.get(code.toUpperCase.trim)

  /**
   * Get countries by calling code (may return multiple for shared codes like +1).
   */
  def fromCallingCode(code: Int): Seq[CountryCode] =
    byCallingCode.getOrElse(code, Seq.empty)

  /**
   * Check if an alpha-2 code is valid.
   */
  def isValidAlpha2(code: String): Boolean =
    fromAlpha2(code).isDefined

/**
 * Phone number type classification.
 */
enum PhoneNumberType:
  case Mobile
  case FixedLine
  case TollFree
  case Premium
  case SharedCost
  case Voip
  case PersonalNumber
  case Pager
  case Unknown

/**
 * Result of phone number parsing.
 */
sealed trait PhoneParseResult:
  def isOk: Boolean
  def isError: Boolean = !isOk
  def phoneNumber: Option[PhoneNumber]
  def error: Option[String]

object PhoneParseResult:
  case class Ok(number: PhoneNumber) extends PhoneParseResult:
    override def isOk: Boolean = true
    override def phoneNumber: Option[PhoneNumber] = Some(number)
    override def error: Option[String] = None

  case class Error(msg: String) extends PhoneParseResult:
    override def isOk: Boolean = false
    override def phoneNumber: Option[PhoneNumber] = None
    override def error: Option[String] = Some(msg)

  def ok(number: PhoneNumber): PhoneParseResult = Ok(number)
  def error(msg: String): PhoneParseResult = Error(msg)

/**
 * Represents a parsed and validated phone number.
 *
 * @param countryCode    The country calling code
 * @param nationalNumber The national significant number (without leading zeros)
 * @param extension      Optional extension
 */
case class PhoneNumber(
    countryCode: Int,
    nationalNumber: String,
    extension: Option[String] = None
):
  /**
   * Get the country (if determinable).
   */
  def country: Option[CountryCode] =
    CountryCode.fromCallingCode(countryCode).headOption

  /**
   * Format in E.164 format (+1234567890).
   */
  def formatE164: String =
    val ext = extension.map(e => s";ext=$e").getOrElse("")
    s"+$countryCode$nationalNumber$ext"

  /**
   * Format in international format (+1 234 567 890).
   */
  def formatInternational: String =
    val formatted = formatNationalNumber
    val ext = extension.map(e => s" ext. $e").getOrElse("")
    s"+$countryCode $formatted$ext"

  /**
   * Format in national format (with trunk prefix if applicable).
   */
  def formatNational: String =
    country match
      case Some(c) =>
        val prefix = c.trunkPrefix.getOrElse("")
        val formatted = formatNationalNumber
        val ext = extension.map(e => s" ext. $e").getOrElse("")
        s"$prefix$formatted$ext"
      case None =>
        formatNationalNumber

  /**
   * Format the national number with grouping (basic implementation).
   */
  private def formatNationalNumber: String =
    val len = nationalNumber.length
    // Apply basic grouping heuristics
    if len <= 4 then nationalNumber
    else if len <= 7 then s"${nationalNumber.take(3)} ${nationalNumber.drop(3)}"
    else if len <= 10 then
      s"${nationalNumber.take(3)} ${nationalNumber.slice(3, 6)} ${nationalNumber.drop(6)}"
    else
      s"${nationalNumber.take(3)} ${nationalNumber.slice(3, 7)} ${nationalNumber.drop(7)}"

  /**
   * Format in RFC 3966 tel URI format.
   */
  def formatRfc3966: String =
    val ext = extension.map(e => s";ext=$e").getOrElse("")
    s"tel:+$countryCode$nationalNumber$ext"

  /**
   * Check if this appears to be a mobile number (heuristic).
   */
  def isMobile: Boolean =
    country match
      case Some(CountryCode.US) | Some(CountryCode.CA) =>
        // NANP: all numbers are potentially mobile
        true
      case Some(CountryCode.GB) =>
        nationalNumber.startsWith("7")
      case Some(CountryCode.DE) =>
        nationalNumber.startsWith("15") || nationalNumber.startsWith("16") || nationalNumber.startsWith("17")
      case Some(CountryCode.FR) =>
        nationalNumber.startsWith("6") || nationalNumber.startsWith("7")
      case Some(CountryCode.AU) =>
        nationalNumber.startsWith("4")
      case Some(CountryCode.JP) =>
        nationalNumber.startsWith("90") || nationalNumber.startsWith("80") || nationalNumber.startsWith("70")
      case Some(CountryCode.CN) =>
        nationalNumber.startsWith("1")
      case Some(CountryCode.IN) =>
        nationalNumber.startsWith("9") || nationalNumber.startsWith("8") || nationalNumber.startsWith("7") || nationalNumber.startsWith("6")
      case _ =>
        false

  /**
   * Get the number type (basic heuristic).
   */
  def numberType: PhoneNumberType =
    if isMobile then PhoneNumberType.Mobile
    else PhoneNumberType.Unknown

  /**
   * Check if this is a toll-free number (US/CA only for now).
   */
  def isTollFree: Boolean =
    if countryCode == 1 then
      val areaCode = nationalNumber.take(3)
      Set("800", "888", "877", "866", "855", "844", "833").contains(areaCode)
    else false

  /**
   * Validate the national number length for the country.
   */
  def isValidLength: Boolean =
    country match
      case Some(c) => c.nationalNumberLength.contains(nationalNumber.length)
      case None => nationalNumber.length >= 6 && nationalNumber.length <= 15

  override def toString: String = formatE164

object PhoneNumber:
  private val DigitPattern = """^\d+$""".r

  /**
   * Parse a phone number string.
   */
  def parse(input: String, defaultCountry: Option[CountryCode] = None): PhoneParseResult =
    val cleaned = input.trim

    if cleaned.isEmpty then
      return PhoneParseResult.error("empty_input")

    // Extract extension if present
    val (numberPart, extension) = extractExtension(cleaned)

    // Remove all non-digit characters except leading +
    val hasPlus = numberPart.startsWith("+")
    val digitsOnly = numberPart.filter(_.isDigit)

    if digitsOnly.isEmpty then
      return PhoneParseResult.error("no_digits")

    if digitsOnly.length < 6 then
      return PhoneParseResult.error("too_short")

    if digitsOnly.length > 15 then
      return PhoneParseResult.error("too_long")

    // Determine country code
    if hasPlus then
      // International format
      parseWithCountryCode(digitsOnly, extension)
    else
      // National format - need default country
      defaultCountry match
        case Some(country) =>
          val nationalNumber = stripTrunkPrefix(digitsOnly, country)
          if country.nationalNumberLength.contains(nationalNumber.length) then
            PhoneParseResult.ok(PhoneNumber(country.callingCode, nationalNumber, extension))
          else
            PhoneParseResult.error("invalid_length")
        case None =>
          // Try to guess from the number
          parseWithCountryCode(digitsOnly, extension)

  /**
   * Extract extension from the number string.
   */
  private def extractExtension(input: String): (String, Option[String]) =
    val extPatterns = Seq("ext", "x", "ext.", "#")
    val lower = input.toLowerCase

    extPatterns.flatMap { pattern =>
      val idx = lower.indexOf(pattern)
      if idx > 0 then
        val numberPart = input.substring(0, idx).trim
        val extPart = input.substring(idx + pattern.length).trim.filter(_.isDigit)
        if extPart.nonEmpty then Some((numberPart, Some(extPart)))
        else Some((numberPart, None))
      else None
    }.headOption.getOrElse((input, None))

  /**
   * Strip the trunk prefix from a national number.
   */
  private def stripTrunkPrefix(number: String, country: CountryCode): String =
    country.trunkPrefix match
      case Some(prefix) if number.startsWith(prefix) =>
        number.drop(prefix.length)
      case _ =>
        number

  /**
   * Parse a number that includes the country code.
   */
  private def parseWithCountryCode(digits: String, extension: Option[String]): PhoneParseResult =
    // Try different country code lengths (1-3 digits)
    for ccLength <- 1 to 3 do
      if digits.length > ccLength then
        val cc = Try(digits.take(ccLength).toInt).getOrElse(0)
        val countries = CountryCode.fromCallingCode(cc)
        if countries.nonEmpty then
          val nationalNumber = digits.drop(ccLength)
          // Check if any country accepts this length
          val validCountry = countries.find(_.nationalNumberLength.contains(nationalNumber.length))
          validCountry match
            case Some(_) =>
              return PhoneParseResult.ok(PhoneNumber(cc, nationalNumber, extension))
            case None if countries.size == 1 =>
              // Single country, accept with warning
              return PhoneParseResult.ok(PhoneNumber(cc, nationalNumber, extension))
            case _ => // Continue trying
        end if
      end if
    end for

    PhoneParseResult.error("invalid_country_code")

/**
 * Safe phone number operations with validation and parsing.
 */
object SafePhone:

  /**
   * Check if a string is a valid phone number.
   */
  def isValid(input: String, defaultCountry: Option[CountryCode] = None): Boolean =
    PhoneNumber.parse(input, defaultCountry).isOk

  /**
   * Parse a phone number string.
   */
  def parse(input: String, defaultCountry: Option[CountryCode] = None): Option[PhoneNumber] =
    PhoneNumber.parse(input, defaultCountry).phoneNumber

  /**
   * Parse a phone number with detailed result.
   */
  def parseWithResult(input: String, defaultCountry: Option[CountryCode] = None): PhoneParseResult =
    PhoneNumber.parse(input, defaultCountry)

  /**
   * Format a phone number to E.164 format.
   */
  def formatE164(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map(_.formatE164)

  /**
   * Format a phone number to international format.
   */
  def formatInternational(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map(_.formatInternational)

  /**
   * Format a phone number to national format.
   */
  def formatNational(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map(_.formatNational)

  /**
   * Format a phone number to RFC 3966 tel URI.
   */
  def formatRfc3966(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map(_.formatRfc3966)

  /**
   * Get the country calling code from a phone number.
   */
  def getCountryCode(input: String, defaultCountry: Option[CountryCode] = None): Option[Int] =
    parse(input, defaultCountry).map(_.countryCode)

  /**
   * Get the country from a phone number.
   */
  def getCountry(input: String, defaultCountry: Option[CountryCode] = None): Option[CountryCode] =
    parse(input, defaultCountry).flatMap(_.country)

  /**
   * Get the national number (without country code).
   */
  def getNationalNumber(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map(_.nationalNumber)

  /**
   * Check if a phone number is a mobile number.
   */
  def isMobile(input: String, defaultCountry: Option[CountryCode] = None): Boolean =
    parse(input, defaultCountry).exists(_.isMobile)

  /**
   * Check if a phone number is toll-free.
   */
  def isTollFree(input: String, defaultCountry: Option[CountryCode] = None): Boolean =
    parse(input, defaultCountry).exists(_.isTollFree)

  /**
   * Compare two phone numbers for equality.
   */
  def areEqual(a: String, b: String, defaultCountry: Option[CountryCode] = None): Boolean =
    (parse(a, defaultCountry), parse(b, defaultCountry)) match
      case (Some(phoneA), Some(phoneB)) =>
        phoneA.countryCode == phoneB.countryCode && phoneA.nationalNumber == phoneB.nationalNumber
      case _ => false

  /**
   * Normalize a phone number to E.164 format.
   */
  def normalize(input: String, defaultCountry: Option[CountryCode] = None): Option[String] =
    formatE164(input, defaultCountry)

  /**
   * Extract only digits from a phone number string.
   */
  def extractDigits(input: String): String =
    input.filter(_.isDigit)

  /**
   * Mask a phone number for display (e.g., +1 *** *** 1234).
   */
  def mask(input: String, visibleDigits: Int = 4, defaultCountry: Option[CountryCode] = None): Option[String] =
    parse(input, defaultCountry).map { phone =>
      val national = phone.nationalNumber
      val masked = if national.length <= visibleDigits then
        national
      else
        "*" * (national.length - visibleDigits) + national.takeRight(visibleDigits)
      s"+${phone.countryCode} $masked"
    }

  /**
   * Get all supported country codes.
   */
  def allCountryCodes: Seq[CountryCode] =
    CountryCode.values.toSeq

  /**
   * Get a country by alpha-2 code.
   */
  def getCountryByAlpha2(code: String): Option[CountryCode] =
    CountryCode.fromAlpha2(code)

  /**
   * Get countries by calling code.
   */
  def getCountriesByCallingCode(code: Int): Seq[CountryCode] =
    CountryCode.fromCallingCode(code)
