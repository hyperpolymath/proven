// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe phone number validation and formatting operations.
 * Follows ITU-T E.164 recommendation.
 */
module proven.safe_phone;

import std.array : appender;
import std.format : format;
import std.typecons : Nullable, nullable;

/// Phone number types
enum PhoneNumberType
{
    mobile,
    fixedLine,
    tollFree,
    premiumRate,
    voip,
    unknown,
}

/// Common country calling codes (ITU-T E.164)
enum CountryCode : ushort
{
    usCA = 1,     /// USA, Canada
    ru = 7,       /// Russia
    eg = 20,      /// Egypt
    za = 27,      /// South Africa
    gr = 30,      /// Greece
    nl = 31,      /// Netherlands
    be = 32,      /// Belgium
    fr = 33,      /// France
    es = 34,      /// Spain
    it = 39,      /// Italy
    ch = 41,      /// Switzerland
    at = 43,      /// Austria
    uk = 44,      /// United Kingdom
    dk = 45,      /// Denmark
    se = 46,      /// Sweden
    no = 47,      /// Norway
    pl = 48,      /// Poland
    de = 49,      /// Germany
    mx = 52,      /// Mexico
    br = 55,      /// Brazil
    au = 61,      /// Australia
    id = 62,      /// Indonesia
    ph = 63,      /// Philippines
    nz = 64,      /// New Zealand
    sg = 65,      /// Singapore
    th = 66,      /// Thailand
    jp = 81,      /// Japan
    kr = 82,      /// South Korea
    vn = 84,      /// Vietnam
    cn = 86,      /// China
    tr = 90,      /// Turkey
    inCountry = 91, /// India (avoiding 'in' keyword)
    pk = 92,      /// Pakistan
    unknown = 0,
}

/// Get the country code numeric value
ushort countryCodeValue(CountryCode code) pure nothrow @safe @nogc
{
    return cast(ushort) code;
}

/// Get the country name for a country code
string countryCodeName(CountryCode code) pure nothrow @safe
{
    final switch (code)
    {
    case CountryCode.usCA:
        return "United States/Canada";
    case CountryCode.ru:
        return "Russia";
    case CountryCode.eg:
        return "Egypt";
    case CountryCode.za:
        return "South Africa";
    case CountryCode.gr:
        return "Greece";
    case CountryCode.nl:
        return "Netherlands";
    case CountryCode.be:
        return "Belgium";
    case CountryCode.fr:
        return "France";
    case CountryCode.es:
        return "Spain";
    case CountryCode.it:
        return "Italy";
    case CountryCode.ch:
        return "Switzerland";
    case CountryCode.at:
        return "Austria";
    case CountryCode.uk:
        return "United Kingdom";
    case CountryCode.dk:
        return "Denmark";
    case CountryCode.se:
        return "Sweden";
    case CountryCode.no:
        return "Norway";
    case CountryCode.pl:
        return "Poland";
    case CountryCode.de:
        return "Germany";
    case CountryCode.mx:
        return "Mexico";
    case CountryCode.br:
        return "Brazil";
    case CountryCode.au:
        return "Australia";
    case CountryCode.id:
        return "Indonesia";
    case CountryCode.ph:
        return "Philippines";
    case CountryCode.nz:
        return "New Zealand";
    case CountryCode.sg:
        return "Singapore";
    case CountryCode.th:
        return "Thailand";
    case CountryCode.jp:
        return "Japan";
    case CountryCode.kr:
        return "South Korea";
    case CountryCode.vn:
        return "Vietnam";
    case CountryCode.cn:
        return "China";
    case CountryCode.tr:
        return "Turkey";
    case CountryCode.inCountry:
        return "India";
    case CountryCode.pk:
        return "Pakistan";
    case CountryCode.unknown:
        return "Unknown";
    }
}

/// Validated phone number
struct PhoneNumber
{
    /// Country calling code
    CountryCode countryCode;

    /// National number digits (max 15 as per E.164)
    char[15] nationalNumber;

    /// Length of the national number
    ubyte nationalLen;

    /// Get the national number as a string
    string getNationalNumber() const pure @safe
    {
        return nationalNumber[0 .. nationalLen].idup;
    }

    /// Get total digit count (country code + national number)
    size_t digitCount() const pure nothrow @safe @nogc
    {
        immutable ccVal = countryCodeValue(countryCode);
        size_t ccDigits = 1;
        if (ccVal >= 100)
            ccDigits = 3;
        else if (ccVal >= 10)
            ccDigits = 2;
        return ccDigits + nationalLen;
    }
}

/// Phone number parsing result
struct PhoneResult
{
    PhoneNumber phone;
    string error;
    bool ok;

    static PhoneResult success(PhoneNumber phone)
    {
        return PhoneResult(phone, "", true);
    }

    static PhoneResult failure(string error)
    {
        return PhoneResult(PhoneNumber.init, error, false);
    }
}

/// Parse phone number from string
PhoneResult parsePhone(string input) pure @safe
{
    if (input.length == 0)
        return PhoneResult.failure("Empty input");

    // Extract digits only
    char[20] digits;
    size_t digitCount = 0;
    size_t start = 0;

    // Skip leading +
    if (input[0] == '+')
        start = 1;

    foreach (c; input[start .. $])
    {
        if (c >= '0' && c <= '9')
        {
            if (digitCount >= 20)
                return PhoneResult.failure("Phone number too long");
            digits[digitCount] = c;
            digitCount++;
        }
        else if (c != ' ' && c != '-' && c != '(' && c != ')')
        {
            return PhoneResult.failure("Invalid character in phone number");
        }
    }

    if (digitCount < 7)
        return PhoneResult.failure("Phone number too short");
    if (digitCount > 15)
        return PhoneResult.failure("Phone number too long (E.164 max is 15 digits)");

    // Try to parse country code (1-3 digits)
    auto ccResult = parseCountryCode(digits[0 .. digitCount]);
    if (ccResult.isNull)
        return PhoneResult.failure("Invalid country code");

    immutable cc = ccResult.get;
    immutable ccLen = countryCodeDigitLen(countryCodeValue(cc));
    immutable nationalStart = ccLen;

    if (digitCount - nationalStart < 4)
        return PhoneResult.failure("Invalid national number (too short)");

    PhoneNumber result;
    result.countryCode = cc;
    result.nationalLen = cast(ubyte)(digitCount - nationalStart);

    foreach (i; 0 .. result.nationalLen)
    {
        result.nationalNumber[i] = digits[nationalStart + i];
    }

    return PhoneResult.success(result);
}

/// Format phone number in E.164 format (+CCNNNN...)
string formatE164(const PhoneNumber phone) pure @safe
{
    auto result = appender!string;
    result.reserve(16);

    result ~= '+';
    result ~= format!"%d"(countryCodeValue(phone.countryCode));
    result ~= phone.nationalNumber[0 .. phone.nationalLen];

    return result[];
}

/// Format phone number with international formatting (spaces)
string formatInternational(const PhoneNumber phone) pure @safe
{
    auto result = appender!string;
    result.reserve(20);

    immutable ccVal = countryCodeValue(phone.countryCode);
    immutable national = phone.nationalNumber[0 .. phone.nationalLen];

    result ~= '+';
    result ~= format!"%d"(ccVal);
    result ~= ' ';

    // Simple grouping: 3-3-remaining
    if (phone.nationalLen <= 4)
    {
        result ~= national;
    }
    else if (phone.nationalLen <= 7)
    {
        result ~= national[0 .. 3];
        result ~= ' ';
        result ~= national[3 .. $];
    }
    else
    {
        result ~= national[0 .. 3];
        result ~= ' ';
        result ~= national[3 .. 6];
        result ~= ' ';
        result ~= national[6 .. $];
    }

    return result[];
}

/// Format phone number for display (national format)
string formatNational(const PhoneNumber phone) pure @safe
{
    auto result = appender!string;
    result.reserve(15);

    immutable national = phone.nationalNumber[0 .. phone.nationalLen];

    // Simple grouping based on length
    if (phone.nationalLen <= 4)
    {
        result ~= national;
    }
    else if (phone.nationalLen <= 7)
    {
        result ~= national[0 .. 3];
        result ~= '-';
        result ~= national[3 .. $];
    }
    else if (phone.nationalLen == 10)
    {
        // US-style formatting: (XXX) XXX-XXXX
        result ~= '(';
        result ~= national[0 .. 3];
        result ~= ") ";
        result ~= national[3 .. 6];
        result ~= '-';
        result ~= national[6 .. $];
    }
    else
    {
        result ~= national[0 .. 3];
        result ~= ' ';
        result ~= national[3 .. 6];
        result ~= ' ';
        result ~= national[6 .. $];
    }

    return result[];
}

/// Check if string is a valid phone number
bool isValidPhone(string input) pure @safe
{
    return parsePhone(input).ok;
}

/// Get the number of digits in a country code
private size_t countryCodeDigitLen(ushort cc) pure nothrow @safe @nogc
{
    if (cc >= 100)
        return 3;
    if (cc >= 10)
        return 2;
    return 1;
}

/// Try to parse a country code value
private Nullable!CountryCode tryParseCC(char[] digits) pure nothrow @safe @nogc
{
    ushort value = 0;
    foreach (d; digits)
    {
        value = cast(ushort)(value * 10 + (d - '0'));
    }

    switch (value)
    {
    case 1:
        return nullable(CountryCode.usCA);
    case 7:
        return nullable(CountryCode.ru);
    case 20:
        return nullable(CountryCode.eg);
    case 27:
        return nullable(CountryCode.za);
    case 30:
        return nullable(CountryCode.gr);
    case 31:
        return nullable(CountryCode.nl);
    case 32:
        return nullable(CountryCode.be);
    case 33:
        return nullable(CountryCode.fr);
    case 34:
        return nullable(CountryCode.es);
    case 39:
        return nullable(CountryCode.it);
    case 41:
        return nullable(CountryCode.ch);
    case 43:
        return nullable(CountryCode.at);
    case 44:
        return nullable(CountryCode.uk);
    case 45:
        return nullable(CountryCode.dk);
    case 46:
        return nullable(CountryCode.se);
    case 47:
        return nullable(CountryCode.no);
    case 48:
        return nullable(CountryCode.pl);
    case 49:
        return nullable(CountryCode.de);
    case 52:
        return nullable(CountryCode.mx);
    case 55:
        return nullable(CountryCode.br);
    case 61:
        return nullable(CountryCode.au);
    case 62:
        return nullable(CountryCode.id);
    case 63:
        return nullable(CountryCode.ph);
    case 64:
        return nullable(CountryCode.nz);
    case 65:
        return nullable(CountryCode.sg);
    case 66:
        return nullable(CountryCode.th);
    case 81:
        return nullable(CountryCode.jp);
    case 82:
        return nullable(CountryCode.kr);
    case 84:
        return nullable(CountryCode.vn);
    case 86:
        return nullable(CountryCode.cn);
    case 90:
        return nullable(CountryCode.tr);
    case 91:
        return nullable(CountryCode.inCountry);
    case 92:
        return nullable(CountryCode.pk);
    default:
        return Nullable!CountryCode.init;
    }
}

/// Parse country code from digits
private Nullable!CountryCode parseCountryCode(char[] digits) pure nothrow @safe @nogc
{
    // Try 3-digit codes first, then 2, then 1
    if (digits.length >= 3)
    {
        auto cc = tryParseCC(digits[0 .. 3]);
        if (!cc.isNull)
            return cc;
    }
    if (digits.length >= 2)
    {
        auto cc = tryParseCC(digits[0 .. 2]);
        if (!cc.isNull)
            return cc;
    }
    if (digits.length >= 1)
    {
        auto cc = tryParseCC(digits[0 .. 1]);
        if (!cc.isNull)
            return cc;
    }
    return Nullable!CountryCode.init;
}

// Unit tests
unittest
{
    // Test parsing valid phone number
    auto result = parsePhone("+1 555 123 4567");
    assert(result.ok);
    assert(result.phone.countryCode == CountryCode.usCA);
    assert(result.phone.nationalLen == 10);

    // Test different formats
    assert(parsePhone("+44 20 7946 0958").ok);
    assert(parsePhone("14155551234").ok);
    assert(parsePhone("+81-3-1234-5678").ok);

    // Test invalid phones
    assert(!parsePhone("123").ok); // Too short
    assert(!parsePhone("").ok); // Empty

    // Test E.164 formatting
    auto phone = parsePhone("+1 555 123 4567").phone;
    assert(formatE164(phone) == "+15551234567");

    // Test international formatting
    immutable intl = formatInternational(phone);
    assert(intl == "+1 555 123 4567");

    // Test national formatting (US-style)
    assert(formatNational(phone) == "(555) 123-4567");

    // Test country code names
    assert(countryCodeName(CountryCode.usCA) == "United States/Canada");
    assert(countryCodeName(CountryCode.uk) == "United Kingdom");
}
