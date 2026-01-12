// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe currency operations with type-safe monetary values.
 * All values stored as minor units (cents) to avoid floating-point errors.
 */
module proven.safe_currency;

import std.array : appender;
import std.conv : to;
import std.math : pow;
import std.string : toLower;
import std.typecons : Nullable, nullable;

/// ISO 4217 currency codes
enum CurrencyCode
{
    USD,
    EUR,
    GBP,
    JPY,
    CHF,
    CAD,
    AUD,
    NZD,
    CNY,
    INR,
    BRL,
    MXN,
    KRW,
    SGD,
    HKD,
    SEK,
    NOK,
    DKK,
    PLN,
    RUB,
    ZAR,
    TRY,
    THB,
    MYR,
    IDR,
    PHP,
    VND,
    AED,
    SAR,
    ILS,
    CZK,
    HUF,
    RON,
    BGN,
    HRK,
    ISK,
    CLP,
    COP,
    PEN,
    ARS,
    BTC,
    ETH,
}

/// Get the number of decimal places for a currency
ubyte currencyDecimals(CurrencyCode code) pure nothrow @safe @nogc
{
    switch (code)
    {
    case CurrencyCode.JPY, CurrencyCode.KRW, CurrencyCode.VND:
        return 0;
    case CurrencyCode.BTC, CurrencyCode.ETH:
        return 8; // Capped at 8 for practical use
    default:
        return 2;
    }
}

/// Get the currency symbol
string currencySymbol(CurrencyCode code) pure nothrow @safe
{
    final switch (code)
    {
    case CurrencyCode.USD:
        return "$";
    case CurrencyCode.EUR:
        return "\u20AC"; // Euro sign
    case CurrencyCode.GBP:
        return "\u00A3"; // Pound sign
    case CurrencyCode.JPY, CurrencyCode.CNY:
        return "\u00A5"; // Yen sign
    case CurrencyCode.CHF:
        return "Fr";
    case CurrencyCode.CAD:
        return "C$";
    case CurrencyCode.AUD:
        return "A$";
    case CurrencyCode.NZD:
        return "NZ$";
    case CurrencyCode.INR:
        return "\u20B9"; // Indian Rupee
    case CurrencyCode.BRL:
        return "R$";
    case CurrencyCode.KRW:
        return "\u20A9"; // Won sign
    case CurrencyCode.RUB:
        return "\u20BD"; // Ruble sign
    case CurrencyCode.TRY:
        return "\u20BA"; // Turkish Lira
    case CurrencyCode.THB:
        return "\u0E3F"; // Thai Baht
    case CurrencyCode.PHP:
        return "\u20B1"; // Philippine Peso
    case CurrencyCode.VND:
        return "\u20AB"; // Vietnamese Dong
    case CurrencyCode.ILS:
        return "\u20AA"; // Shekel
    case CurrencyCode.BTC:
        return "\u20BF"; // Bitcoin
    case CurrencyCode.ETH:
        return "\u039E"; // Xi (Ethereum)
    case CurrencyCode.MXN, CurrencyCode.SGD, CurrencyCode.HKD,
        CurrencyCode.SEK, CurrencyCode.NOK, CurrencyCode.DKK,
        CurrencyCode.PLN, CurrencyCode.ZAR, CurrencyCode.MYR,
        CurrencyCode.IDR, CurrencyCode.AED, CurrencyCode.SAR,
        CurrencyCode.CZK, CurrencyCode.HUF, CurrencyCode.RON,
        CurrencyCode.BGN, CurrencyCode.HRK, CurrencyCode.ISK,
        CurrencyCode.CLP, CurrencyCode.COP, CurrencyCode.PEN,
        CurrencyCode.ARS:
        return "";
    }
}

/// Get the currency name
string currencyName(CurrencyCode code) pure nothrow @safe
{
    final switch (code)
    {
    case CurrencyCode.USD:
        return "US Dollar";
    case CurrencyCode.EUR:
        return "Euro";
    case CurrencyCode.GBP:
        return "British Pound";
    case CurrencyCode.JPY:
        return "Japanese Yen";
    case CurrencyCode.CHF:
        return "Swiss Franc";
    case CurrencyCode.CAD:
        return "Canadian Dollar";
    case CurrencyCode.AUD:
        return "Australian Dollar";
    case CurrencyCode.NZD:
        return "New Zealand Dollar";
    case CurrencyCode.CNY:
        return "Chinese Yuan";
    case CurrencyCode.INR:
        return "Indian Rupee";
    case CurrencyCode.BRL:
        return "Brazilian Real";
    case CurrencyCode.MXN:
        return "Mexican Peso";
    case CurrencyCode.KRW:
        return "South Korean Won";
    case CurrencyCode.SGD:
        return "Singapore Dollar";
    case CurrencyCode.HKD:
        return "Hong Kong Dollar";
    case CurrencyCode.SEK:
        return "Swedish Krona";
    case CurrencyCode.NOK:
        return "Norwegian Krone";
    case CurrencyCode.DKK:
        return "Danish Krone";
    case CurrencyCode.PLN:
        return "Polish Zloty";
    case CurrencyCode.RUB:
        return "Russian Ruble";
    case CurrencyCode.ZAR:
        return "South African Rand";
    case CurrencyCode.TRY:
        return "Turkish Lira";
    case CurrencyCode.THB:
        return "Thai Baht";
    case CurrencyCode.MYR:
        return "Malaysian Ringgit";
    case CurrencyCode.IDR:
        return "Indonesian Rupiah";
    case CurrencyCode.PHP:
        return "Philippine Peso";
    case CurrencyCode.VND:
        return "Vietnamese Dong";
    case CurrencyCode.AED:
        return "UAE Dirham";
    case CurrencyCode.SAR:
        return "Saudi Riyal";
    case CurrencyCode.ILS:
        return "Israeli Shekel";
    case CurrencyCode.CZK:
        return "Czech Koruna";
    case CurrencyCode.HUF:
        return "Hungarian Forint";
    case CurrencyCode.RON:
        return "Romanian Leu";
    case CurrencyCode.BGN:
        return "Bulgarian Lev";
    case CurrencyCode.HRK:
        return "Croatian Kuna";
    case CurrencyCode.ISK:
        return "Icelandic Krona";
    case CurrencyCode.CLP:
        return "Chilean Peso";
    case CurrencyCode.COP:
        return "Colombian Peso";
    case CurrencyCode.PEN:
        return "Peruvian Sol";
    case CurrencyCode.ARS:
        return "Argentine Peso";
    case CurrencyCode.BTC:
        return "Bitcoin";
    case CurrencyCode.ETH:
        return "Ethereum";
    }
}

/// Type-safe monetary value
struct Money
{
    /// Amount in minor units (cents, satoshis, etc.)
    long minorUnits;

    /// The currency code
    CurrencyCode currency;

    /// Create from major units (dollars, euros, etc.)
    static Money fromMajor(long major, CurrencyCode currency) pure nothrow @safe
    {
        immutable decimals = currencyDecimals(currency);
        immutable multiplier = cast(long) pow(10.0, decimals);
        return Money(major * multiplier, currency);
    }

    /// Create from minor units (cents, pence, etc.)
    static Money fromMinor(long minor, CurrencyCode currency) pure nothrow @safe @nogc
    {
        return Money(minor, currency);
    }

    /// Zero amount for a currency
    static Money zero(CurrencyCode currency) pure nothrow @safe @nogc
    {
        return Money(0, currency);
    }

    /// Get major units (truncated)
    long getMajor() const pure nothrow @safe
    {
        immutable decimals = currencyDecimals(currency);
        immutable divisor = cast(long) pow(10.0, decimals);
        return minorUnits / divisor;
    }

    /// Get minor units
    long getMinor() const pure nothrow @safe @nogc
    {
        return minorUnits;
    }

    /// Get the fractional part in minor units
    long getFraction() const pure nothrow @safe
    {
        immutable decimals = currencyDecimals(currency);
        immutable divisor = cast(long) pow(10.0, decimals);
        immutable absUnits = minorUnits < 0 ? -minorUnits : minorUnits;
        return absUnits % divisor;
    }

    /// Check if currencies match (for safe operations)
    private bool currencyMatches(const Money other) const pure nothrow @safe @nogc
    {
        return currency == other.currency;
    }

    /// Add two monetary values (same currency enforced at runtime)
    Nullable!Money add(const Money other) const pure nothrow @safe @nogc
    {
        if (!currencyMatches(other))
            return Nullable!Money.init;
        return nullable(Money(minorUnits + other.minorUnits, currency));
    }

    /// Subtract two monetary values
    Nullable!Money sub(const Money other) const pure nothrow @safe @nogc
    {
        if (!currencyMatches(other))
            return Nullable!Money.init;
        return nullable(Money(minorUnits - other.minorUnits, currency));
    }

    /// Multiply by scalar
    Money mul(long scalar) const pure nothrow @safe @nogc
    {
        return Money(minorUnits * scalar, currency);
    }

    /// Divide by scalar (truncates)
    Nullable!Money div(long scalar) const pure nothrow @safe @nogc
    {
        if (scalar == 0)
            return Nullable!Money.init;
        return nullable(Money(minorUnits / scalar, currency));
    }

    /// Negate
    Money negate() const pure nothrow @safe @nogc
    {
        return Money(-minorUnits, currency);
    }

    /// Absolute value
    Money abs() const pure nothrow @safe @nogc
    {
        return Money(minorUnits < 0 ? -minorUnits : minorUnits, currency);
    }

    /// Check if zero
    bool isZero() const pure nothrow @safe @nogc
    {
        return minorUnits == 0;
    }

    /// Check if positive
    bool isPositive() const pure nothrow @safe @nogc
    {
        return minorUnits > 0;
    }

    /// Check if negative
    bool isNegative() const pure nothrow @safe @nogc
    {
        return minorUnits < 0;
    }

    /// Operator overloads
    Money opBinary(string op)(long scalar) const pure nothrow @safe @nogc
        if (op == "*")
    {
        return mul(scalar);
    }

    Money opUnary(string op)() const pure nothrow @safe @nogc
        if (op == "-")
    {
        return negate();
    }

    /// Comparison operators (only valid for same currency)
    int opCmp(const Money other) const pure nothrow @safe @nogc
    {
        // Note: Comparing different currencies is undefined behavior
        if (minorUnits < other.minorUnits)
            return -1;
        if (minorUnits > other.minorUnits)
            return 1;
        return 0;
    }

    bool opEquals(const Money other) const pure nothrow @safe @nogc
    {
        return currency == other.currency && minorUnits == other.minorUnits;
    }
}

/// Format money as string with currency symbol
string formatMoney(const Money money) pure @safe
{
    import std.format : format;

    immutable decimals = currencyDecimals(money.currency);
    immutable divisor = cast(long) pow(10.0, decimals);
    immutable absUnits = money.minorUnits < 0 ? -money.minorUnits : money.minorUnits;
    immutable major = absUnits / divisor;
    immutable minor = absUnits % divisor;
    immutable symbol = currencySymbol(money.currency);
    immutable sign = money.minorUnits < 0 ? "-" : "";

    if (decimals == 0)
    {
        return format!"%s%s%d"(sign, symbol, major);
    }
    else
    {
        // Format with proper decimal places
        return format!"%s%s%d.%0*d"(sign, symbol, major, decimals, minor);
    }
}

/// Format money with currency code
string formatMoneyWithCode(const Money money) pure @safe
{
    import std.conv : to;
    import std.format : format;

    immutable decimals = currencyDecimals(money.currency);
    immutable divisor = cast(long) pow(10.0, decimals);
    immutable absUnits = money.minorUnits < 0 ? -money.minorUnits : money.minorUnits;
    immutable major = absUnits / divisor;
    immutable minor = absUnits % divisor;
    immutable sign = money.minorUnits < 0 ? "-" : "";

    if (decimals == 0)
    {
        return format!"%s%d %s"(sign, major, money.currency.to!string);
    }
    else
    {
        return format!"%s%d.%0*d %s"(sign, major, decimals, minor, money.currency.to!string);
    }
}

/// Parse currency code from string
Nullable!CurrencyCode parseCurrencyCode(string str) pure @safe
{
    import std.traits : EnumMembers;

    immutable upperStr = str.toLower();

    foreach (code; EnumMembers!CurrencyCode)
    {
        import std.conv : to;

        if (code.to!string.toLower() == upperStr)
            return nullable(code);
    }
    return Nullable!CurrencyCode.init;
}

/// Check if string is valid currency code
bool isValidCurrencyCode(string str) pure @safe
{
    return !parseCurrencyCode(str).isNull;
}

// Unit tests
unittest
{
    // Test basic operations
    immutable usd100 = Money.fromMajor(100, CurrencyCode.USD);
    immutable usd50 = Money.fromMajor(50, CurrencyCode.USD);

    assert(usd100.getMinor() == 10000);
    assert(usd100.getMajor() == 100);

    auto sum = usd100.add(usd50);
    assert(!sum.isNull);
    assert(sum.get.getMinor() == 15000);

    auto diff = usd100.sub(usd50);
    assert(!diff.isNull);
    assert(diff.get.getMinor() == 5000);

    assert(usd100.mul(2).getMinor() == 20000);

    // Test zero decimals currency
    immutable jpy1000 = Money.fromMajor(1000, CurrencyCode.JPY);
    assert(jpy1000.getMinor() == 1000);
    assert(jpy1000.getMajor() == 1000);

    // Test currency mismatch
    auto invalidSum = usd100.add(jpy1000);
    assert(invalidSum.isNull);

    // Test currency parsing
    assert(parseCurrencyCode("USD").get == CurrencyCode.USD);
    assert(parseCurrencyCode("usd").get == CurrencyCode.USD);
    assert(parseCurrencyCode("XYZ").isNull);

    // Test formatting
    assert(formatMoney(Money.fromMajor(42, CurrencyCode.USD)) == "$42.00");
    assert(formatMoney(Money.fromMinor(4250, CurrencyCode.USD)) == "$42.50");
    assert(formatMoney(Money.fromMinor(-4250, CurrencyCode.USD)) == "-$42.50");

    // Test comparison
    assert(usd100 > usd50);
    assert(usd50 < usd100);
    assert(usd100 == Money.fromMajor(100, CurrencyCode.USD));
}
