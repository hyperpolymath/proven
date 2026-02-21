// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Phone number type classification.
 */
enum class PhoneNumberType {
    MOBILE,
    FIXED_LINE,
    TOLL_FREE,
    PREMIUM_RATE,
    SHARED_COST,
    VOIP,
    PERSONAL_NUMBER,
    PAGER,
    UAN,  // Universal Access Number
    UNKNOWN
}

/**
 * ITU country calling codes (common ones).
 *
 * @property code The numeric country calling code value.
 * @property displayPrefix The formatted prefix with + sign.
 */
enum class CountryCode(val code: Int, val displayPrefix: String) {
    CC1(1, "+1"),         // USA, Canada, Caribbean
    CC7(7, "+7"),         // Russia, Kazakhstan
    CC20(20, "+20"),      // Egypt
    CC27(27, "+27"),      // South Africa
    CC30(30, "+30"),      // Greece
    CC31(31, "+31"),      // Netherlands
    CC32(32, "+32"),      // Belgium
    CC33(33, "+33"),      // France
    CC34(34, "+34"),      // Spain
    CC36(36, "+36"),      // Hungary
    CC39(39, "+39"),      // Italy
    CC40(40, "+40"),      // Romania
    CC41(41, "+41"),      // Switzerland
    CC43(43, "+43"),      // Austria
    CC44(44, "+44"),      // UK
    CC45(45, "+45"),      // Denmark
    CC46(46, "+46"),      // Sweden
    CC47(47, "+47"),      // Norway
    CC48(48, "+48"),      // Poland
    CC49(49, "+49"),      // Germany
    CC51(51, "+51"),      // Peru
    CC52(52, "+52"),      // Mexico
    CC53(53, "+53"),      // Cuba
    CC54(54, "+54"),      // Argentina
    CC55(55, "+55"),      // Brazil
    CC56(56, "+56"),      // Chile
    CC57(57, "+57"),      // Colombia
    CC58(58, "+58"),      // Venezuela
    CC60(60, "+60"),      // Malaysia
    CC61(61, "+61"),      // Australia
    CC62(62, "+62"),      // Indonesia
    CC63(63, "+63"),      // Philippines
    CC64(64, "+64"),      // New Zealand
    CC65(65, "+65"),      // Singapore
    CC66(66, "+66"),      // Thailand
    CC81(81, "+81"),      // Japan
    CC82(82, "+82"),      // South Korea
    CC84(84, "+84"),      // Vietnam
    CC86(86, "+86"),      // China
    CC90(90, "+90"),      // Turkey
    CC91(91, "+91"),      // India
    CC92(92, "+92"),      // Pakistan
    CC93(93, "+93"),      // Afghanistan
    CC94(94, "+94"),      // Sri Lanka
    CC95(95, "+95"),      // Myanmar
    CC98(98, "+98"),      // Iran
    CC212(212, "+212"),   // Morocco
    CC213(213, "+213"),   // Algeria
    CC216(216, "+216"),   // Tunisia
    CC218(218, "+218"),   // Libya
    CC234(234, "+234"),   // Nigeria
    CC254(254, "+254"),   // Kenya
    CC255(255, "+255"),   // Tanzania
    CC256(256, "+256"),   // Uganda
    CC351(351, "+351"),   // Portugal
    CC352(352, "+352"),   // Luxembourg
    CC353(353, "+353"),   // Ireland
    CC354(354, "+354"),   // Iceland
    CC358(358, "+358"),   // Finland
    CC380(380, "+380"),   // Ukraine
    CC381(381, "+381"),   // Serbia
    CC385(385, "+385"),   // Croatia
    CC386(386, "+386"),   // Slovenia
    CC420(420, "+420"),   // Czech Republic
    CC421(421, "+421"),   // Slovakia
    CC852(852, "+852"),   // Hong Kong
    CC853(853, "+853"),   // Macau
    CC855(855, "+855"),   // Cambodia
    CC856(856, "+856"),   // Laos
    CC880(880, "+880"),   // Bangladesh
    CC886(886, "+886"),   // Taiwan
    CC960(960, "+960"),   // Maldives
    CC961(961, "+961"),   // Lebanon
    CC962(962, "+962"),   // Jordan
    CC963(963, "+963"),   // Syria
    CC964(964, "+964"),   // Iraq
    CC965(965, "+965"),   // Kuwait
    CC966(966, "+966"),   // Saudi Arabia
    CC967(967, "+967"),   // Yemen
    CC968(968, "+968"),   // Oman
    CC971(971, "+971"),   // UAE
    CC972(972, "+972"),   // Israel
    CC973(973, "+973"),   // Bahrain
    CC974(974, "+974"),   // Qatar
    CC975(975, "+975"),   // Bhutan
    CC976(976, "+976"),   // Mongolia
    CC977(977, "+977"),   // Nepal
    CC992(992, "+992"),   // Tajikistan
    CC993(993, "+993"),   // Turkmenistan
    CC994(994, "+994"),   // Azerbaijan
    CC995(995, "+995"),   // Georgia
    CC996(996, "+996"),   // Kyrgyzstan
    CC998(998, "+998");   // Uzbekistan

    companion object {
        private val byCode: Map<Int, CountryCode> = entries.associateBy { it.code }

        /**
         * Find country code by numeric value.
         */
        fun fromCode(code: Int): CountryCode? = byCode[code]

        /**
         * Parse country code from string digits.
         */
        fun parse(digits: String): Pair<CountryCode, String>? {
            // Try 3-digit codes first, then 2-digit, then 1-digit
            for (length in 3 downTo 1) {
                if (digits.length >= length) {
                    val prefix = digits.substring(0, length)
                    val codeValue = prefix.toIntOrNull()
                    if (codeValue != null) {
                        val countryCode = fromCode(codeValue)
                        if (countryCode != null) {
                            return Pair(countryCode, digits.substring(length))
                        }
                    }
                }
            }
            return null
        }
    }
}

/**
 * Phone number parsing errors.
 */
sealed class PhoneError {
    data class InvalidCharacter(val character: Char, val position: Int) : PhoneError()
    data class TooShort(val length: Int) : PhoneError()
    data class TooLong(val length: Int) : PhoneError()
    data class InvalidCountryCode(val code: String) : PhoneError()
    data class InvalidNationalNumber(val number: String) : PhoneError()
    data object EmptyInput : PhoneError()

    override fun toString(): String = when (this) {
        is InvalidCharacter -> "Invalid character '$character' at position $position"
        is TooShort -> "Phone number too short: $length digits"
        is TooLong -> "Phone number too long: $length digits"
        is InvalidCountryCode -> "Invalid country code: $code"
        is InvalidNationalNumber -> "Invalid national number: $number"
        is EmptyInput -> "Empty input"
    }
}

/**
 * Validated phone number in E.164 format.
 *
 * @property countryCode The country calling code.
 * @property nationalNumber The national number (without country code).
 */
data class PhoneNumber(
    val countryCode: CountryCode,
    val nationalNumber: String
) {
    /**
     * Get the full number as digits only (without +).
     */
    val digits: String
        get() = "${countryCode.code}$nationalNumber"

    /**
     * Get the total length of the phone number.
     */
    val length: Int
        get() = digits.length

    /**
     * Format in E.164 format (+CCNNNN...).
     */
    fun formatE164(): String = "+$digits"

    /**
     * Format with spaces for readability (e.g., +1 555 123 4567).
     */
    fun formatInternational(): String {
        val formatted = formatNationalNumber(nationalNumber)
        return "${countryCode.displayPrefix} $formatted"
    }

    override fun toString(): String = formatE164()

    private fun formatNationalNumber(number: String): String {
        val len = number.length
        return when {
            len <= 4 -> number
            len <= 7 -> "${number.substring(0, 3)} ${number.substring(3)}"
            len <= 10 -> "${number.substring(0, 3)} ${number.substring(3, 6)} ${number.substring(6)}"
            else -> number
        }
    }

    companion object {
        /**
         * Parse phone number from string (E.164 format or digits).
         *
         * @param input The phone number string to parse.
         * @return Result containing the parsed PhoneNumber or an error.
         */
        fun parse(input: String): Result<PhoneNumber> = runCatching {
            val trimmed = input.trim()
            if (trimmed.isEmpty()) {
                throw IllegalArgumentException(PhoneError.EmptyInput.toString())
            }

            val (digits, startsWithPlus) = extractDigits(trimmed)
            val digitCount = digits.length

            if (digitCount < 7) {
                throw IllegalArgumentException(PhoneError.TooShort(digitCount).toString())
            }
            if (digitCount > 15) {
                throw IllegalArgumentException(PhoneError.TooLong(digitCount).toString())
            }

            val (countryCode, nationalNumber) = CountryCode.parse(digits)
                ?: throw IllegalArgumentException(
                    PhoneError.InvalidCountryCode(digits.take(3)).toString()
                )

            if (nationalNumber.length < 4) {
                throw IllegalArgumentException(
                    PhoneError.InvalidNationalNumber(nationalNumber).toString()
                )
            }

            PhoneNumber(countryCode, nationalNumber)
        }

        /**
         * Check if string is a valid phone number.
         */
        fun isValid(input: String): Boolean = parse(input).isSuccess

        private fun extractDigits(input: String): Pair<String, Boolean> {
            val startsWithPlus = input.startsWith("+")
            val digits = input.filter { it.isDigit() }
            return Pair(digits, startsWithPlus)
        }
    }
}

/**
 * Safe phone number operations.
 */
object SafePhone {
    /**
     * Parse phone number from string.
     *
     * @param input The phone number string to parse.
     * @return The parsed PhoneNumber or null if invalid.
     */
    fun parse(input: String): PhoneNumber? = PhoneNumber.parse(input).getOrNull()

    /**
     * Parse phone number from string, returning Result.
     *
     * @param input The phone number string to parse.
     * @return Result containing the parsed PhoneNumber or an error.
     */
    fun parseResult(input: String): Result<PhoneNumber> = PhoneNumber.parse(input)

    /**
     * Check if string is a valid phone number.
     *
     * @param input The string to validate.
     * @return True if the string is a valid phone number.
     */
    fun isValid(input: String): Boolean = PhoneNumber.isValid(input)

    /**
     * Format phone number in E.164 format.
     *
     * @param phoneNumber The phone number to format.
     * @return The E.164 formatted string.
     */
    fun formatE164(phoneNumber: PhoneNumber): String = phoneNumber.formatE164()

    /**
     * Format phone number for international display.
     *
     * @param phoneNumber The phone number to format.
     * @return The formatted string with spaces.
     */
    fun formatInternational(phoneNumber: PhoneNumber): String = phoneNumber.formatInternational()

    /**
     * Get the country code from a phone number.
     *
     * @param phoneNumber The phone number.
     * @return The country code.
     */
    fun getCountryCode(phoneNumber: PhoneNumber): CountryCode = phoneNumber.countryCode

    /**
     * Get the national number from a phone number.
     *
     * @param phoneNumber The phone number.
     * @return The national number without country code.
     */
    fun getNationalNumber(phoneNumber: PhoneNumber): String = phoneNumber.nationalNumber

    /**
     * Get the full number as digits only.
     *
     * @param phoneNumber The phone number.
     * @return The digits without + prefix.
     */
    fun getDigits(phoneNumber: PhoneNumber): String = phoneNumber.digits
}
