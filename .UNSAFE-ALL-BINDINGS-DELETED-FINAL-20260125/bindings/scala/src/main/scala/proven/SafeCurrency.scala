// SPDX-License-Identifier: PMPL-1.0

package proven

import scala.util.Try

/**
 * ISO 4217 currency codes with metadata.
 */
enum CurrencyCode(
    val numericCode: Int,
    val minorUnits: Int,
    val name: String
):
  // Major currencies
  case USD extends CurrencyCode(840, 2, "US Dollar")
  case EUR extends CurrencyCode(978, 2, "Euro")
  case GBP extends CurrencyCode(826, 2, "Pound Sterling")
  case JPY extends CurrencyCode(392, 0, "Yen")
  case CHF extends CurrencyCode(756, 2, "Swiss Franc")
  case CAD extends CurrencyCode(124, 2, "Canadian Dollar")
  case AUD extends CurrencyCode(36, 2, "Australian Dollar")
  case NZD extends CurrencyCode(554, 2, "New Zealand Dollar")
  case CNY extends CurrencyCode(156, 2, "Yuan Renminbi")
  case HKD extends CurrencyCode(344, 2, "Hong Kong Dollar")
  case SGD extends CurrencyCode(702, 2, "Singapore Dollar")
  case SEK extends CurrencyCode(752, 2, "Swedish Krona")
  case NOK extends CurrencyCode(578, 2, "Norwegian Krone")
  case DKK extends CurrencyCode(208, 2, "Danish Krone")
  case KRW extends CurrencyCode(410, 0, "Won")
  case INR extends CurrencyCode(356, 2, "Indian Rupee")
  case RUB extends CurrencyCode(643, 2, "Russian Ruble")
  case BRL extends CurrencyCode(986, 2, "Brazilian Real")
  case ZAR extends CurrencyCode(710, 2, "Rand")
  case MXN extends CurrencyCode(484, 2, "Mexican Peso")
  case PLN extends CurrencyCode(985, 2, "Zloty")
  case TRY extends CurrencyCode(949, 2, "Turkish Lira")
  case THB extends CurrencyCode(764, 2, "Baht")
  case ILS extends CurrencyCode(376, 2, "New Israeli Sheqel")
  case AED extends CurrencyCode(784, 2, "UAE Dirham")
  case SAR extends CurrencyCode(682, 2, "Saudi Riyal")
  case CZK extends CurrencyCode(203, 2, "Czech Koruna")
  case HUF extends CurrencyCode(348, 2, "Forint")
  case PHP extends CurrencyCode(608, 2, "Philippine Peso")
  case TWD extends CurrencyCode(901, 2, "New Taiwan Dollar")
  case MYR extends CurrencyCode(458, 2, "Malaysian Ringgit")
  case IDR extends CurrencyCode(360, 2, "Rupiah")
  case VND extends CurrencyCode(704, 0, "Dong")
  case CLP extends CurrencyCode(152, 0, "Chilean Peso")
  case ARS extends CurrencyCode(32, 2, "Argentine Peso")
  case COP extends CurrencyCode(170, 2, "Colombian Peso")
  case PEN extends CurrencyCode(604, 2, "Sol")
  case EGP extends CurrencyCode(818, 2, "Egyptian Pound")
  case NGN extends CurrencyCode(566, 2, "Naira")
  case KES extends CurrencyCode(404, 2, "Kenyan Shilling")
  case GHS extends CurrencyCode(936, 2, "Ghana Cedi")
  case UAH extends CurrencyCode(980, 2, "Hryvnia")
  case RON extends CurrencyCode(946, 2, "Romanian Leu")
  case BGN extends CurrencyCode(975, 2, "Bulgarian Lev")
  case HRK extends CurrencyCode(191, 2, "Kuna")
  case ISK extends CurrencyCode(352, 0, "Iceland Krona")

  // Precious metals
  case XAU extends CurrencyCode(959, -1, "Gold")
  case XAG extends CurrencyCode(961, -1, "Silver")
  case XPT extends CurrencyCode(962, -1, "Platinum")
  case XPD extends CurrencyCode(964, -1, "Palladium")

  // Cryptocurrencies (non-ISO but commonly used)
  case BTC extends CurrencyCode(0, 8, "Bitcoin")
  case ETH extends CurrencyCode(0, 18, "Ether")
  case XRP extends CurrencyCode(0, 6, "XRP")
  case USDT extends CurrencyCode(0, 6, "Tether")
  case USDC extends CurrencyCode(0, 6, "USD Coin")

  /**
   * Get the multiplier for converting to minor units.
   */
  def minorUnitMultiplier: BigDecimal =
    if minorUnits < 0 then BigDecimal(1)
    else BigDecimal(10).pow(minorUnits)

  /**
   * Get the currency symbol (returns code if no symbol).
   */
  def symbol: String = this match
    case USD => "$"
    case EUR => "\u20AC"
    case GBP => "\u00A3"
    case JPY => "\u00A5"
    case CHF => "CHF"
    case CNY => "\u00A5"
    case INR => "\u20B9"
    case RUB => "\u20BD"
    case BRL => "R$"
    case KRW => "\u20A9"
    case TRY => "\u20BA"
    case THB => "\u0E3F"
    case ILS => "\u20AA"
    case PLN => "z\u0142"
    case BTC => "\u20BF"
    case ETH => "\u039E"
    case _ => toString

object CurrencyCode:
  private val byCode: Map[String, CurrencyCode] =
    CurrencyCode.values.map(c => c.toString -> c).toMap

  private val byNumeric: Map[Int, CurrencyCode] =
    CurrencyCode.values.filter(_.numericCode > 0).map(c => c.numericCode -> c).toMap

  /**
   * Parse a currency code from string (case-insensitive).
   */
  def parse(code: String): Option[CurrencyCode] =
    byCode.get(code.toUpperCase.trim)

  /**
   * Get currency by ISO numeric code.
   */
  def fromNumericCode(code: Int): Option[CurrencyCode] =
    byNumeric.get(code)

  /**
   * Check if a string is a valid currency code.
   */
  def isValid(code: String): Boolean =
    parse(code).isDefined

/**
 * Represents a monetary amount in a specific currency.
 * Internally stores the amount in minor units (e.g., cents for USD) to avoid floating-point issues.
 *
 * @param amountInMinorUnits The amount in the smallest currency unit
 * @param currency           The currency code
 */
case class Money private (
    amountInMinorUnits: Long,
    currency: CurrencyCode
):
  /**
   * Get the amount as a BigDecimal in major units.
   */
  def amount: BigDecimal =
    if currency.minorUnits <= 0 then BigDecimal(amountInMinorUnits)
    else BigDecimal(amountInMinorUnits) / currency.minorUnitMultiplier

  /**
   * Get the amount formatted with currency symbol.
   */
  def format: String =
    val formatted = formatAmount
    s"${currency.symbol}$formatted"

  /**
   * Get the amount formatted with currency code suffix.
   */
  def formatWithCode: String =
    val formatted = formatAmount
    s"$formatted ${currency.toString}"

  /**
   * Get the amount formatted without currency indicator.
   */
  def formatAmount: String =
    if currency.minorUnits <= 0 then
      amountInMinorUnits.toString
    else
      val scale = currency.minorUnits
      val major = amountInMinorUnits / currency.minorUnitMultiplier.toLong
      val minor = (amountInMinorUnits % currency.minorUnitMultiplier.toLong).abs
      f"$major%d.${minor.toString.reverse.padTo(scale, '0').reverse}"

  /**
   * Add two Money values (must be same currency).
   */
  def +(other: Money): Option[Money] =
    if currency != other.currency then None
    else
      Try(Math.addExact(amountInMinorUnits, other.amountInMinorUnits))
        .toOption
        .map(Money(_, currency))

  /**
   * Subtract two Money values (must be same currency).
   */
  def -(other: Money): Option[Money] =
    if currency != other.currency then None
    else
      Try(Math.subtractExact(amountInMinorUnits, other.amountInMinorUnits))
        .toOption
        .map(Money(_, currency))

  /**
   * Multiply by a scalar.
   */
  def *(scalar: Long): Option[Money] =
    Try(Math.multiplyExact(amountInMinorUnits, scalar))
      .toOption
      .map(Money(_, currency))

  /**
   * Multiply by a decimal scalar (rounds to nearest minor unit).
   */
  def *(scalar: BigDecimal): Option[Money] =
    val result = BigDecimal(amountInMinorUnits) * scalar
    if result > Long.MaxValue || result < Long.MinValue then None
    else Some(Money(result.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLong, currency))

  /**
   * Divide by a scalar.
   */
  def /(divisor: Long): Option[Money] =
    if divisor == 0 then None
    else Some(Money(amountInMinorUnits / divisor, currency))

  /**
   * Negate the amount.
   */
  def unary_- : Option[Money] =
    Try(Math.negateExact(amountInMinorUnits))
      .toOption
      .map(Money(_, currency))

  /**
   * Get the absolute value.
   */
  def abs: Money =
    Money(Math.abs(amountInMinorUnits), currency)

  /**
   * Check if zero.
   */
  def isZero: Boolean = amountInMinorUnits == 0

  /**
   * Check if positive.
   */
  def isPositive: Boolean = amountInMinorUnits > 0

  /**
   * Check if negative.
   */
  def isNegative: Boolean = amountInMinorUnits < 0

  /**
   * Compare with another Money (must be same currency).
   */
  def compare(other: Money): Option[Int] =
    if currency != other.currency then None
    else Some(amountInMinorUnits.compare(other.amountInMinorUnits))

  override def toString: String = format

object Money:
  /**
   * Create Money from a BigDecimal amount in major units.
   */
  def apply(amount: BigDecimal, currency: CurrencyCode): Option[Money] =
    val inMinorUnits = amount * currency.minorUnitMultiplier
    if inMinorUnits > Long.MaxValue || inMinorUnits < Long.MinValue then None
    else Some(new Money(inMinorUnits.setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toLong, currency))

  /**
   * Create Money from a double amount in major units.
   */
  def apply(amount: Double, currency: CurrencyCode): Option[Money] =
    apply(BigDecimal(amount), currency)

  /**
   * Create Money from minor units directly.
   */
  def fromMinorUnits(minorUnits: Long, currency: CurrencyCode): Money =
    new Money(minorUnits, currency)

  /**
   * Create zero Money in the given currency.
   */
  def zero(currency: CurrencyCode): Money =
    new Money(0, currency)

  /**
   * Parse a money string like "$123.45" or "123.45 USD".
   */
  def parse(input: String): Option[Money] =
    val trimmed = input.trim

    // Try "123.45 USD" format
    val withCodePattern = """^(-?[\d,.]+)\s*([A-Za-z]{3,4})$""".r
    trimmed match
      case withCodePattern(amountStr, codeStr) =>
        for
          currency <- CurrencyCode.parse(codeStr)
          amount <- Try(BigDecimal(amountStr.replace(",", ""))).toOption
          money <- Money(amount, currency)
        yield money
      case _ =>
        // Try "$123.45" format with common symbols
        val symbolMap = Map(
          "$" -> CurrencyCode.USD,
          "\u00A3" -> CurrencyCode.GBP,
          "\u20AC" -> CurrencyCode.EUR,
          "\u00A5" -> CurrencyCode.JPY,
          "\u20B9" -> CurrencyCode.INR
        )

        symbolMap.collectFirst {
          case (symbol, currency) if trimmed.startsWith(symbol) =>
            val amountStr = trimmed.stripPrefix(symbol).trim.replace(",", "")
            for
              amount <- Try(BigDecimal(amountStr)).toOption
              money <- Money(amount, currency)
            yield money
        }.flatten

/**
 * Safe currency operations with validation.
 */
object SafeCurrency:

  /**
   * Check if a string is a valid ISO 4217 currency code.
   */
  def isValidCode(code: String): Boolean =
    CurrencyCode.isValid(code)

  /**
   * Parse a currency code.
   */
  def parseCode(code: String): Option[CurrencyCode] =
    CurrencyCode.parse(code)

  /**
   * Get currency by numeric code.
   */
  def fromNumericCode(code: Int): Option[CurrencyCode] =
    CurrencyCode.fromNumericCode(code)

  /**
   * Get the number of minor units (decimal places) for a currency.
   */
  def getMinorUnits(code: String): Option[Int] =
    CurrencyCode.parse(code).map(_.minorUnits)

  /**
   * Get the currency symbol.
   */
  def getSymbol(code: String): Option[String] =
    CurrencyCode.parse(code).map(_.symbol)

  /**
   * Get the full currency name.
   */
  def getName(code: String): Option[String] =
    CurrencyCode.parse(code).map(_.name)

  /**
   * Create a Money value.
   */
  def money(amount: BigDecimal, currency: CurrencyCode): Option[Money] =
    Money(amount, currency)

  /**
   * Create a Money value from string code.
   */
  def money(amount: BigDecimal, currencyCode: String): Option[Money] =
    CurrencyCode.parse(currencyCode).flatMap(Money(amount, _))

  /**
   * Parse a money string.
   */
  def parseMoney(input: String): Option[Money] =
    Money.parse(input)

  /**
   * Format a monetary amount with the given currency.
   */
  def format(amount: BigDecimal, currency: CurrencyCode): String =
    Money(amount, currency).map(_.format).getOrElse(s"$amount ${currency.toString}")

  /**
   * Format a monetary amount with the given currency code.
   */
  def format(amount: BigDecimal, currencyCode: String): Option[String] =
    CurrencyCode.parse(currencyCode).flatMap(c => Money(amount, c).map(_.format))

  /**
   * Add two monetary amounts (must be same currency).
   */
  def add(a: Money, b: Money): Option[Money] = a + b

  /**
   * Subtract two monetary amounts (must be same currency).
   */
  def subtract(a: Money, b: Money): Option[Money] = a - b

  /**
   * Check if two currencies are the same.
   */
  def sameCurrency(a: Money, b: Money): Boolean =
    a.currency == b.currency

  /**
   * Convert amount from one currency to another using an exchange rate.
   * Rate should be: 1 unit of fromCurrency = rate units of toCurrency.
   */
  def convert(
      amount: Money,
      toCurrency: CurrencyCode,
      exchangeRate: BigDecimal
  ): Option[Money] =
    val converted = amount.amount * exchangeRate
    Money(converted, toCurrency)

  /**
   * Get all available currency codes.
   */
  def allCurrencyCodes: Seq[CurrencyCode] =
    CurrencyCode.values.toSeq

  /**
   * Get all fiat currency codes (exclude crypto and precious metals).
   */
  def fiatCurrencyCodes: Seq[CurrencyCode] =
    CurrencyCode.values.filter { c =>
      c.numericCode > 0 && c.numericCode < 900
    }.toSeq

  /**
   * Get all cryptocurrency codes.
   */
  def cryptoCurrencyCodes: Seq[CurrencyCode] =
    Seq(CurrencyCode.BTC, CurrencyCode.ETH, CurrencyCode.XRP, CurrencyCode.USDT, CurrencyCode.USDC)

  /**
   * Get all precious metal codes.
   */
  def preciousMetalCodes: Seq[CurrencyCode] =
    Seq(CurrencyCode.XAU, CurrencyCode.XAG, CurrencyCode.XPT, CurrencyCode.XPD)
