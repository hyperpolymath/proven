// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.nio.charset.StandardCharsets;

/**
 * Safe currency operations with type-safe monetary values.
 * Provides ISO 4217 currency codes and overflow-checked arithmetic.
 * Calls native verified code via JNI when available.
 */
public final class SafeCurrency {
    private SafeCurrency() {}

    /**
     * ISO 4217 currency codes.
     */
    public enum CurrencyCode {
        USD(0, "USD", "US Dollar", "$", 2, 840),
        EUR(1, "EUR", "Euro", "\u20AC", 2, 978),
        GBP(2, "GBP", "British Pound", "\u00A3", 2, 826),
        JPY(3, "JPY", "Japanese Yen", "\u00A5", 0, 392),
        CHF(4, "CHF", "Swiss Franc", "CHF", 2, 756),
        CAD(5, "CAD", "Canadian Dollar", "C$", 2, 124),
        AUD(6, "AUD", "Australian Dollar", "A$", 2, 36),
        NZD(7, "NZD", "New Zealand Dollar", "NZ$", 2, 554),
        CNY(8, "CNY", "Chinese Yuan", "\u00A5", 2, 156),
        INR(9, "INR", "Indian Rupee", "\u20B9", 2, 356),
        BRL(10, "BRL", "Brazilian Real", "R$", 2, 986),
        MXN(11, "MXN", "Mexican Peso", "$", 2, 484),
        KRW(12, "KRW", "South Korean Won", "\u20A9", 0, 410),
        SGD(13, "SGD", "Singapore Dollar", "S$", 2, 702),
        HKD(14, "HKD", "Hong Kong Dollar", "HK$", 2, 344),
        SEK(15, "SEK", "Swedish Krona", "kr", 2, 752),
        NOK(16, "NOK", "Norwegian Krone", "kr", 2, 578),
        DKK(17, "DKK", "Danish Krone", "kr", 2, 208),
        PLN(18, "PLN", "Polish Zloty", "z\u0142", 2, 985),
        ZAR(19, "ZAR", "South African Rand", "R", 2, 710),
        BTC(20, "BTC", "Bitcoin", "\u20BF", 8, 0),
        ETH(21, "ETH", "Ethereum", "\u039E", 18, 0);

        private final int ordinalValue;
        private final String isoCode;
        private final String name;
        private final String symbol;
        private final int decimalPlaces;
        private final int isoNumeric;

        CurrencyCode(int ordinalValue, String isoCode, String name, String symbol,
                     int decimalPlaces, int isoNumeric) {
            this.ordinalValue = ordinalValue;
            this.isoCode = isoCode;
            this.name = name;
            this.symbol = symbol;
            this.decimalPlaces = decimalPlaces;
            this.isoNumeric = isoNumeric;
        }

        public int getOrdinalValue() { return ordinalValue; }
        public String getIsoCode() { return isoCode; }
        public String getName() { return name; }
        public String getSymbol() { return symbol; }
        public int getDecimalPlaces() { return decimalPlaces; }
        public int getIsoNumeric() { return isoNumeric; }

        /**
         * Get multiplier for converting major to minor units.
         */
        public long getMinorMultiplier() {
            return (long) Math.pow(10, decimalPlaces);
        }

        public static CurrencyCode fromIsoCode(String code) {
            if (code == null) return null;
            String upper = code.toUpperCase();
            for (CurrencyCode c : values()) {
                if (c.isoCode.equals(upper)) return c;
            }
            return null;
        }

        public static CurrencyCode fromOrdinal(int ordinal) {
            for (CurrencyCode c : values()) {
                if (c.ordinalValue == ordinal) return c;
            }
            return null;
        }
    }

    /**
     * Type-safe monetary value stored in minor units.
     */
    public record Money(long minorUnits, CurrencyCode currency) {
        /**
         * Create money from major units (dollars, euros, etc.).
         */
        public static Money fromMajor(long amount, CurrencyCode currency) {
            return new Money(amount * currency.getMinorMultiplier(), currency);
        }

        /**
         * Create money from minor units (cents, satoshis, etc.).
         */
        public static Money fromMinor(long amount, CurrencyCode currency) {
            return new Money(amount, currency);
        }

        /**
         * Create zero money.
         */
        public static Money zero(CurrencyCode currency) {
            return new Money(0, currency);
        }

        /**
         * Get major units (truncated).
         */
        public long getMajor() {
            return minorUnits / currency.getMinorMultiplier();
        }

        /**
         * Get fractional part in minor units.
         */
        public long getFraction() {
            return Math.abs(minorUnits % currency.getMinorMultiplier());
        }

        /**
         * Check if zero.
         */
        public boolean isZero() {
            return minorUnits == 0;
        }

        /**
         * Check if positive.
         */
        public boolean isPositive() {
            return minorUnits > 0;
        }

        /**
         * Check if negative.
         */
        public boolean isNegative() {
            return minorUnits < 0;
        }

        /**
         * Format as string with symbol.
         */
        public String format() {
            long major = getMajor();
            long fraction = getFraction();
            int decimals = currency.getDecimalPlaces();

            if (decimals == 0) {
                return (minorUnits < 0 ? "-" : "") + currency.getSymbol() + Math.abs(major);
            }

            String fractionStr = String.format("%0" + decimals + "d", fraction);
            return (minorUnits < 0 ? "-" : "") + currency.getSymbol() +
                   Math.abs(major) + "." + fractionStr;
        }

        /**
         * Format as plain decimal string.
         */
        public String formatPlain() {
            long major = getMajor();
            long fraction = getFraction();
            int decimals = currency.getDecimalPlaces();

            if (decimals == 0) {
                return String.valueOf(major);
            }

            String fractionStr = String.format("%0" + decimals + "d", fraction);
            return (minorUnits < 0 ? "-" : "") + Math.abs(major) + "." + fractionStr;
        }

        /**
         * Format with ISO code suffix.
         */
        public String formatIso() {
            return formatPlain() + " " + currency.getIsoCode();
        }

        @Override
        public String toString() {
            return format();
        }
    }

    /**
     * Add two money values.
     */
    public static ProvenResult<Money> add(Money a, Money b) {
        if (a.currency() != b.currency()) {
            return ProvenResult.err(ProvenStatus.ERR_CURRENCY_MISMATCH,
                "Cannot add different currencies: " + a.currency().getIsoCode() +
                " and " + b.currency().getIsoCode());
        }

        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMoneyAdd(
                a.minorUnits(), b.minorUnits(), a.currency().getOrdinalValue(), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(new Money(result, a.currency())) : ProvenResult.err(s);
        }

        return SafeMath.addSafe(a.minorUnits(), b.minorUnits())
            .map(sum -> new Money(sum, a.currency()))
            .map(m -> m)
            .flatMap(m -> ProvenResult.ok(m));
    }

    /**
     * Subtract two money values.
     */
    public static ProvenResult<Money> subtract(Money a, Money b) {
        if (a.currency() != b.currency()) {
            return ProvenResult.err(ProvenStatus.ERR_CURRENCY_MISMATCH,
                "Cannot subtract different currencies");
        }

        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMoneySub(
                a.minorUnits(), b.minorUnits(), a.currency().getOrdinalValue(), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(new Money(result, a.currency())) : ProvenResult.err(s);
        }

        return SafeMath.subSafe(a.minorUnits(), b.minorUnits())
            .map(diff -> new Money(diff, a.currency()))
            .flatMap(m -> ProvenResult.ok(m));
    }

    /**
     * Multiply money by scalar.
     */
    public static ProvenResult<Money> multiply(Money m, long scalar) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMoneyMul(
                m.minorUnits(), scalar, m.currency().getOrdinalValue(), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(new Money(result, m.currency())) : ProvenResult.err(s);
        }

        return SafeMath.mulSafe(m.minorUnits(), scalar)
            .map(product -> new Money(product, m.currency()))
            .flatMap(money -> ProvenResult.ok(money));
    }

    /**
     * Divide money by scalar.
     */
    public static ProvenResult<Money> divide(Money m, long divisor) {
        if (ProvenNative.isNativeLoaded()) {
            int[] status = new int[1];
            long result = ProvenNative.nativeMoneyDiv(
                m.minorUnits(), divisor, m.currency().getOrdinalValue(), status);
            ProvenStatus s = ProvenStatus.fromCode(status[0]);
            return s.isOk() ? ProvenResult.ok(new Money(result, m.currency())) : ProvenResult.err(s);
        }

        return SafeMath.divSafe(m.minorUnits(), divisor)
            .map(quotient -> new Money(quotient, m.currency()))
            .flatMap(money -> ProvenResult.ok(money));
    }

    /**
     * Get absolute value of money.
     */
    public static Money abs(Money m) {
        return m.minorUnits() < 0 ? new Money(-m.minorUnits(), m.currency()) : m;
    }

    /**
     * Negate money value.
     */
    public static ProvenResult<Money> negate(Money m) {
        return SafeMath.negateSafe(m.minorUnits())
            .map(negated -> new Money(negated, m.currency()))
            .flatMap(money -> ProvenResult.ok(money));
    }

    /**
     * Compare two money values.
     */
    public static int compare(Money a, Money b) {
        if (a.currency() != b.currency()) return 0;
        return Long.compare(a.minorUnits(), b.minorUnits());
    }

    /**
     * Check if currency code is valid.
     */
    public static boolean isValidCode(String code) {
        if (code == null) return false;
        if (ProvenNative.isNativeLoaded()) {
            return ProvenNative.nativeCurrencyIsValidCode(code.getBytes(StandardCharsets.UTF_8));
        }
        return CurrencyCode.fromIsoCode(code) != null;
    }

    /**
     * Parse money from string.
     */
    public static ProvenResult<Money> parse(String amount, CurrencyCode currency) {
        if (amount == null || amount.isEmpty()) {
            return ProvenResult.err(ProvenStatus.ERR_EMPTY_INPUT, "Empty amount");
        }

        try {
            // Remove currency symbol and whitespace
            String clean = amount.replaceAll("[^0-9.\\-]", "");
            if (clean.isEmpty()) {
                return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "No numeric value found");
            }

            // Parse as decimal
            double value = Double.parseDouble(clean);
            long minor = Math.round(value * currency.getMinorMultiplier());
            return ProvenResult.ok(new Money(minor, currency));
        } catch (NumberFormatException e) {
            return ProvenResult.err(ProvenStatus.ERR_PARSE_FAILURE, "Invalid amount format");
        }
    }
}
