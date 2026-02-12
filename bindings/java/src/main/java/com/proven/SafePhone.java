// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;
import java.util.regex.Pattern;

/**
 * Safe phone number validation following E.164.
 * Provides phone number parsing, validation, and formatting.
 * Calls native verified code via JNI when available.
 */
public final class SafePhone {
    private SafePhone() {}

    /** Maximum E.164 phone number length (including country code). */
    public static final int MAX_DIGITS = 15;

    /** Minimum phone number length. */
    public static final int MIN_DIGITS = 7;

    /** Basic E.164 pattern. */
    private static final Pattern E164_PATTERN = Pattern.compile("^\\+[1-9]\\d{6,14}$");

    /**
     * Country calling codes.
     */
    public enum CountryCode {
        UNKNOWN(0, "", "Unknown"),
        US(1, "US", "United States"),
        RU(7, "RU", "Russia"),
        EG(20, "EG", "Egypt"),
        ZA(27, "ZA", "South Africa"),
        GR(30, "GR", "Greece"),
        NL(31, "NL", "Netherlands"),
        BE(32, "BE", "Belgium"),
        FR(33, "FR", "France"),
        ES(34, "ES", "Spain"),
        IT(39, "IT", "Italy"),
        CH(41, "CH", "Switzerland"),
        AT(43, "AT", "Austria"),
        GB(44, "GB", "United Kingdom"),
        DK(45, "DK", "Denmark"),
        SE(46, "SE", "Sweden"),
        NO(47, "NO", "Norway"),
        PL(48, "PL", "Poland"),
        DE(49, "DE", "Germany"),
        MX(52, "MX", "Mexico"),
        BR(55, "BR", "Brazil"),
        AU(61, "AU", "Australia"),
        ID(62, "ID", "Indonesia"),
        PH(63, "PH", "Philippines"),
        NZ(64, "NZ", "New Zealand"),
        SG(65, "SG", "Singapore"),
        TH(66, "TH", "Thailand"),
        JP(81, "JP", "Japan"),
        KR(82, "KR", "South Korea"),
        VN(84, "VN", "Vietnam"),
        CN(86, "CN", "China"),
        TR(90, "TR", "Turkey"),
        IN(91, "IN", "India"),
        PK(92, "PK", "Pakistan"),
        IL(972, "IL", "Israel"),
        AE(971, "AE", "UAE"),
        SA(966, "SA", "Saudi Arabia");

        private final int callingCode;
        private final String isoAlpha2;
        private final String name;

        CountryCode(int callingCode, String isoAlpha2, String name) {
            this.callingCode = callingCode;
            this.isoAlpha2 = isoAlpha2;
            this.name = name;
        }

        public int getCallingCode() { return callingCode; }
        public String getIsoAlpha2() { return isoAlpha2; }
        public String getName() { return name; }

        public static CountryCode fromCallingCode(int code) {
            for (CountryCode c : values()) {
                if (c.callingCode == code) return c;
            }
            return UNKNOWN;
        }

        public static CountryCode fromIsoAlpha2(String iso) {
            if (iso == null) return UNKNOWN;
            String upper = iso.toUpperCase();
            for (CountryCode c : values()) {
                if (c.isoAlpha2.equals(upper)) return c;
            }
            return UNKNOWN;
        }
    }

    /**
     * Parsed phone number.
     */
    public record PhoneNumber(CountryCode countryCode, String nationalNumber) {
        /**
         * Format as E.164.
         */
        public String formatE164() {
            return "+" + countryCode.getCallingCode() + nationalNumber;
        }

        /**
         * Format as international.
         */
        public String formatInternational() {
            return "+" + countryCode.getCallingCode() + " " + formatNationalWithSpaces();
        }

        /**
         * Format with spaces in national number.
         */
        private String formatNationalWithSpaces() {
            if (nationalNumber.length() <= 4) return nationalNumber;
            if (nationalNumber.length() <= 7) {
                return nationalNumber.substring(0, 3) + " " +
                       nationalNumber.substring(3);
            }
            // Standard format: XXX XXX XXXX
            return nationalNumber.substring(0, 3) + " " +
                   nationalNumber.substring(3, 6) + " " +
                   nationalNumber.substring(6);
        }

        /**
         * Get total digit count.
         */
        public int digitCount() {
            return String.valueOf(countryCode.getCallingCode()).length() + nationalNumber.length();
        }

        @Override
        public String toString() {
            return formatE164();
        }
    }

    /**
     * Check if string is a valid phone number.
     */
    public static boolean isValid(String phone) {
        if (phone == null || phone.isEmpty()) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativePhoneIsValid(phone.getBytes(StandardCharsets.UTF_8));
        }
        return isValidPure(phone);
    }

    /**
     * Parse phone number from string.
     */
    public static ProvenResult<PhoneNumber> parse(String phone) {
        if (phone == null || phone.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty phone number");
        }

        // Extract digits
        String digits = extractDigits(phone);
        if (digits.length() < MIN_DIGITS) {
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Phone number too short");
        }
        if (digits.length() > MAX_DIGITS) {
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Phone number too long");
        }

        // Detect country code
        CountryCode country = detectCountryCode(digits);
        if (country == CountryCode.UNKNOWN) {
            // Try assuming US if starts with area code
            if (digits.length() == 10) {
                return ProvenResult.ok(new PhoneNumber(CountryCode.US, digits));
            }
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Unknown country code");
        }

        String ccDigits = String.valueOf(country.getCallingCode());
        String national = digits.substring(ccDigits.length());

        return ProvenResult.ok(new PhoneNumber(country, national));
    }

    /**
     * Parse phone number with explicit country code.
     */
    public static ProvenResult<PhoneNumber> parseWithCountry(String phone, CountryCode country) {
        if (phone == null || phone.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty phone number");
        }

        String digits = extractDigits(phone);
        if (digits.length() < MIN_DIGITS - 3) { // Allow shorter national numbers
            return ProvenResult.err(ProvenStatus.ERR_VALIDATION_FAILED, "Phone number too short");
        }

        return ProvenResult.ok(new PhoneNumber(country, digits));
    }

    /**
     * Format phone number as E.164.
     */
    public static ProvenResult<String> formatE164(String phone) {
        return parse(phone).map(PhoneNumber::formatE164);
    }

    /**
     * Format phone number as international format.
     */
    public static ProvenResult<String> formatInternational(String phone) {
        return parse(phone).map(PhoneNumber::formatInternational);
    }

    /**
     * Extract only digits from phone string.
     */
    public static String extractDigits(String phone) {
        if (phone == null) return "";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < phone.length(); i++) {
            char c = phone.charAt(i);
            if (c >= '0' && c <= '9') {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * Detect country code from digit string.
     */
    private static CountryCode detectCountryCode(String digits) {
        // Try 3-digit codes first
        if (digits.length() >= 3) {
            int code3 = Integer.parseInt(digits.substring(0, 3));
            CountryCode cc = CountryCode.fromCallingCode(code3);
            if (cc != CountryCode.UNKNOWN) return cc;
        }

        // Try 2-digit codes
        if (digits.length() >= 2) {
            int code2 = Integer.parseInt(digits.substring(0, 2));
            CountryCode cc = CountryCode.fromCallingCode(code2);
            if (cc != CountryCode.UNKNOWN) return cc;
        }

        // Try 1-digit codes
        if (digits.length() >= 1) {
            int code1 = Integer.parseInt(digits.substring(0, 1));
            CountryCode cc = CountryCode.fromCallingCode(code1);
            if (cc != CountryCode.UNKNOWN) return cc;
        }

        return CountryCode.UNKNOWN;
    }

    private static boolean isValidPure(String phone) {
        String digits = extractDigits(phone);
        if (digits.length() < MIN_DIGITS || digits.length() > MAX_DIGITS) {
            return false;
        }

        // Check for E.164 format
        if (phone.startsWith("+")) {
            return E164_PATTERN.matcher(phone.replaceAll("[^+0-9]", "")).matches();
        }

        // Allow national format
        return true;
    }
}
