// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * ISO 4217 currency codes (common currencies).
 *
 * @property code The ISO 4217 three-letter code.
 * @property decimals Number of decimal places for the currency.
 * @property symbol Currency symbol for display.
 * @property displayName Human-readable currency name.
 */
enum class CurrencyCode(
    val code: String,
    val decimals: Int,
    val symbol: String,
    val displayName: String
) {
    USD("USD", 2, "$", "US Dollar"),
    EUR("EUR", 2, "\u20AC", "Euro"),
    GBP("GBP", 2, "\u00A3", "British Pound Sterling"),
    JPY("JPY", 0, "\u00A5", "Japanese Yen"),
    CHF("CHF", 2, "Fr", "Swiss Franc"),
    CAD("CAD", 2, "C$", "Canadian Dollar"),
    AUD("AUD", 2, "A$", "Australian Dollar"),
    NZD("NZD", 2, "NZ$", "New Zealand Dollar"),
    CNY("CNY", 2, "\u00A5", "Chinese Yuan"),
    INR("INR", 2, "\u20B9", "Indian Rupee"),
    BRL("BRL", 2, "R$", "Brazilian Real"),
    MXN("MXN", 2, "Mex$", "Mexican Peso"),
    KRW("KRW", 0, "\u20A9", "South Korean Won"),
    SGD("SGD", 2, "S$", "Singapore Dollar"),
    HKD("HKD", 2, "HK$", "Hong Kong Dollar"),
    SEK("SEK", 2, "kr", "Swedish Krona"),
    NOK("NOK", 2, "kr", "Norwegian Krone"),
    DKK("DKK", 2, "kr", "Danish Krone"),
    PLN("PLN", 2, "z\u0142", "Polish Zloty"),
    RUB("RUB", 2, "\u20BD", "Russian Ruble"),
    ZAR("ZAR", 2, "R", "South African Rand"),
    TRY("TRY", 2, "\u20BA", "Turkish Lira"),
    THB("THB", 2, "\u0E3F", "Thai Baht"),
    MYR("MYR", 2, "RM", "Malaysian Ringgit"),
    IDR("IDR", 2, "Rp", "Indonesian Rupiah"),
    PHP("PHP", 2, "\u20B1", "Philippine Peso"),
    VND("VND", 0, "\u20AB", "Vietnamese Dong"),
    AED("AED", 2, "\u062F.\u0625", "UAE Dirham"),
    SAR("SAR", 2, "\uFDFC", "Saudi Riyal"),
    ILS("ILS", 2, "\u20AA", "Israeli New Shekel"),
    CZK("CZK", 2, "K\u010D", "Czech Koruna"),
    HUF("HUF", 2, "Ft", "Hungarian Forint"),
    RON("RON", 2, "lei", "Romanian Leu"),
    BGN("BGN", 2, "\u043B\u0432", "Bulgarian Lev"),
    HRK("HRK", 2, "kn", "Croatian Kuna"),
    ISK("ISK", 0, "kr", "Icelandic Krona"),
    CLP("CLP", 0, "$", "Chilean Peso"),
    COP("COP", 2, "$", "Colombian Peso"),
    PEN("PEN", 2, "S/", "Peruvian Sol"),
    ARS("ARS", 2, "$", "Argentine Peso"),
    BTC("BTC", 8, "\u20BF", "Bitcoin"),
    ETH("ETH", 18, "\u039E", "Ethereum");

    companion object {
        /**
         * Parse currency code from string (case-insensitive).
         *
         * @param codeString The currency code string to parse.
         * @return The matching CurrencyCode or null if not found.
         */
        fun parse(codeString: String): CurrencyCode? =
            entries.find { it.code.equals(codeString.trim(), ignoreCase = true) }

        /**
         * Check if string is valid currency code.
         *
         * @param codeString The string to validate.
         * @return True if the string is a valid currency code.
         */
        fun isValid(codeString: String): Boolean = parse(codeString) != null
    }
}

/**
 * Currency operation errors.
 */
sealed class CurrencyError {
    data class InvalidAmount(val message: String) : CurrencyError()
    data class CurrencyMismatch(val expected: CurrencyCode, val actual: CurrencyCode) : CurrencyError()
    data object DivisionByZero : CurrencyError()
    data object Overflow : CurrencyError()
    data class InvalidCurrencyCode(val code: String) : CurrencyError()

    override fun toString(): String = when (this) {
        is InvalidAmount -> "Invalid amount: $message"
        is CurrencyMismatch -> "Currency mismatch: expected $expected, got $actual"
        is DivisionByZero -> "Division by zero"
        is Overflow -> "Arithmetic overflow"
        is InvalidCurrencyCode -> "Invalid currency code: $code"
    }
}

/**
 * Monetary value in minor units (cents, satoshis, etc.).
 * Uses minor units to avoid floating-point precision issues.
 *
 * @property minorUnits The value in the smallest currency unit.
 * @property currency The currency code.
 */
data class Money(
    val minorUnits: Long,
    val currency: CurrencyCode
) : Comparable<Money> {

    /**
     * Get the major units (dollars, euros, etc.) as a Long (truncated).
     */
    val majorUnits: Long
        get() {
            val divisor = divisorForCurrency(currency)
            return minorUnits / divisor
        }

    /**
     * Check if amount is zero.
     */
    val isZero: Boolean get() = minorUnits == 0L

    /**
     * Check if amount is positive.
     */
    val isPositive: Boolean get() = minorUnits > 0

    /**
     * Check if amount is negative.
     */
    val isNegative: Boolean get() = minorUnits < 0

    /**
     * Add two monetary values of the same currency.
     */
    operator fun plus(other: Money): Money {
        require(currency == other.currency) {
            "Cannot add different currencies: $currency and ${other.currency}"
        }
        return Money(
            Math.addExact(minorUnits, other.minorUnits),
            currency
        )
    }

    /**
     * Subtract two monetary values of the same currency.
     */
    operator fun minus(other: Money): Money {
        require(currency == other.currency) {
            "Cannot subtract different currencies: $currency and ${other.currency}"
        }
        return Money(
            Math.subtractExact(minorUnits, other.minorUnits),
            currency
        )
    }

    /**
     * Multiply money by a scalar.
     */
    operator fun times(multiplier: Long): Money =
        Money(Math.multiplyExact(minorUnits, multiplier), currency)

    /**
     * Multiply money by a scalar (Int).
     */
    operator fun times(multiplier: Int): Money = times(multiplier.toLong())

    /**
     * Divide money by a scalar.
     */
    operator fun div(divisor: Long): Money {
        require(divisor != 0L) { "Cannot divide by zero" }
        return Money(minorUnits / divisor, currency)
    }

    /**
     * Divide money by a scalar (Int).
     */
    operator fun div(divisor: Int): Money = div(divisor.toLong())

    /**
     * Negate the monetary value.
     */
    operator fun unaryMinus(): Money = Money(-minorUnits, currency)

    /**
     * Get the absolute value.
     */
    fun abs(): Money = Money(kotlin.math.abs(minorUnits), currency)

    override fun compareTo(other: Money): Int {
        require(currency == other.currency) {
            "Cannot compare different currencies: $currency and ${other.currency}"
        }
        return minorUnits.compareTo(other.minorUnits)
    }

    /**
     * Format with currency symbol (e.g., "$123.45").
     */
    fun formatWithSymbol(): String {
        val sign = if (minorUnits < 0) "-" else ""
        val absUnits = kotlin.math.abs(minorUnits)
        val divisor = divisorForCurrency(currency)
        val major = absUnits / divisor
        val minor = absUnits % divisor

        return if (currency.decimals == 0) {
            "$sign${currency.symbol}$major"
        } else {
            "$sign${currency.symbol}$major.${minor.toString().padStart(currency.decimals, '0')}"
        }
    }

    /**
     * Format with currency code (e.g., "123.45 USD").
     */
    fun formatWithCode(): String {
        val sign = if (minorUnits < 0) "-" else ""
        val absUnits = kotlin.math.abs(minorUnits)
        val divisor = divisorForCurrency(currency)
        val major = absUnits / divisor
        val minor = absUnits % divisor

        return if (currency.decimals == 0) {
            "$sign$major ${currency.code}"
        } else {
            "$sign$major.${minor.toString().padStart(currency.decimals, '0')} ${currency.code}"
        }
    }

    override fun toString(): String = formatWithCode()

    companion object {
        /**
         * Create money from major units (dollars, euros, etc.).
         */
        fun fromMajorUnits(amount: Long, currency: CurrencyCode): Money {
            val multiplier = divisorForCurrency(currency)
            return Money(Math.multiplyExact(amount, multiplier), currency)
        }

        /**
         * Create money from minor units (cents, pence, etc.).
         */
        fun fromMinorUnits(amount: Long, currency: CurrencyCode): Money =
            Money(amount, currency)

        /**
         * Create zero amount for a currency.
         */
        fun zero(currency: CurrencyCode): Money = Money(0, currency)

        private fun divisorForCurrency(currency: CurrencyCode): Long {
            var result = 1L
            repeat(currency.decimals) { result *= 10 }
            return result
        }
    }
}

/**
 * Safe currency operations.
 */
object SafeCurrency {

    /**
     * Parse currency code from string.
     */
    fun parseCurrencyCode(code: String): CurrencyCode? = CurrencyCode.parse(code)

    /**
     * Check if string is valid currency code.
     */
    fun isValidCurrencyCode(code: String): Boolean = CurrencyCode.isValid(code)

    /**
     * Create money from major units with safe arithmetic.
     *
     * @return Result containing Money or an ArithmeticException on overflow.
     */
    fun fromMajorUnits(amount: Long, currency: CurrencyCode): Result<Money> = runCatching {
        Money.fromMajorUnits(amount, currency)
    }

    /**
     * Create money from minor units.
     */
    fun fromMinorUnits(amount: Long, currency: CurrencyCode): Money =
        Money.fromMinorUnits(amount, currency)

    /**
     * Safely add two monetary values.
     *
     * @return Result containing the sum or an error.
     */
    fun add(a: Money, b: Money): Result<Money> = runCatching { a + b }

    /**
     * Safely subtract two monetary values.
     *
     * @return Result containing the difference or an error.
     */
    fun subtract(a: Money, b: Money): Result<Money> = runCatching { a - b }

    /**
     * Safely multiply money by a scalar.
     *
     * @return Result containing the product or an error on overflow.
     */
    fun multiply(money: Money, multiplier: Long): Result<Money> = runCatching {
        money * multiplier
    }

    /**
     * Safely divide money by a scalar.
     *
     * @return Result containing the quotient or an error on division by zero.
     */
    fun divide(money: Money, divisor: Long): Result<Money> = runCatching {
        money / divisor
    }
}
