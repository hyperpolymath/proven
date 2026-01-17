// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe hexadecimal encoding and decoding operations.
 * Includes constant-time comparison for security-sensitive contexts.
 */
module proven.safe_hex;

import std.algorithm : map;
import std.array : appender;
import std.ascii : toLower;
import std.typecons : Nullable, nullable;

/// Hex operation result type
struct HexResult(T)
{
    T value;
    string error;
    bool ok;

    static HexResult!T success(T value)
    {
        return HexResult!T(value, "", true);
    }

    static HexResult!T failure(string error)
    {
        return HexResult!T(T.init, error, false);
    }
}

/// Check if character is valid hexadecimal
bool isHexChar(char c) pure nothrow @safe @nogc
{
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

/// Convert hex character to nibble value (0-15)
Nullable!ubyte hexCharToNibble(char c) pure nothrow @safe @nogc
{
    if (c >= '0' && c <= '9')
        return nullable(cast(ubyte)(c - '0'));
    if (c >= 'a' && c <= 'f')
        return nullable(cast(ubyte)(c - 'a' + 10));
    if (c >= 'A' && c <= 'F')
        return nullable(cast(ubyte)(c - 'A' + 10));
    return Nullable!ubyte.init;
}

/// Convert nibble (0-15) to lowercase hex character
char nibbleToHexChar(ubyte n) pure nothrow @safe @nogc
{
    return cast(char)(n < 10 ? '0' + n : 'a' + n - 10);
}

/// Convert nibble (0-15) to uppercase hex character
char nibbleToHexCharUpper(ubyte n) pure nothrow @safe @nogc
{
    return cast(char)(n < 10 ? '0' + n : 'A' + n - 10);
}

/// Encode single byte to two hex characters (lowercase)
void encodeByte(ubyte b, ref char[2] output) pure nothrow @safe @nogc
{
    output[0] = nibbleToHexChar(b >> 4);
    output[1] = nibbleToHexChar(b & 0x0F);
}

/// Encode single byte to two hex characters (uppercase)
void encodeByteUpper(ubyte b, ref char[2] output) pure nothrow @safe @nogc
{
    output[0] = nibbleToHexCharUpper(b >> 4);
    output[1] = nibbleToHexCharUpper(b & 0x0F);
}

/// Encode bytes to hex string (lowercase)
string encode(const(ubyte)[] bytes) pure @safe
{
    auto result = appender!string;
    result.reserve(bytes.length * 2);

    foreach (b; bytes)
    {
        result ~= nibbleToHexChar(b >> 4);
        result ~= nibbleToHexChar(b & 0x0F);
    }

    return result[];
}

/// Encode bytes to hex string (uppercase)
string encodeUpper(const(ubyte)[] bytes) pure @safe
{
    auto result = appender!string;
    result.reserve(bytes.length * 2);

    foreach (b; bytes)
    {
        result ~= nibbleToHexCharUpper(b >> 4);
        result ~= nibbleToHexCharUpper(b & 0x0F);
    }

    return result[];
}

/// Decode two hex characters to a byte
Nullable!ubyte decodeHexByte(char h1, char h2) pure nothrow @safe @nogc
{
    immutable n1 = hexCharToNibble(h1);
    immutable n2 = hexCharToNibble(h2);
    if (n1.isNull || n2.isNull)
        return Nullable!ubyte.init;
    return nullable(cast(ubyte)((n1.get << 4) | n2.get));
}

/// Decode hex string to bytes
HexResult!(ubyte[]) decode(string hex) pure @safe
{
    if (hex.length % 2 != 0)
        return HexResult!(ubyte[]).failure("Odd hex string length");

    ubyte[] result;
    result.reserve(hex.length / 2);

    for (size_t i = 0; i < hex.length; i += 2)
    {
        immutable b = decodeHexByte(hex[i], hex[i + 1]);
        if (b.isNull)
            return HexResult!(ubyte[]).failure("Invalid hexadecimal character");
        result ~= b.get;
    }

    return HexResult!(ubyte[]).success(result);
}

/// Validate hex string (all characters are valid hex)
bool isValidHex(string str) pure nothrow @safe @nogc
{
    foreach (c; str)
    {
        if (!isHexChar(c))
            return false;
    }
    return true;
}

/// Validate hex string with even length (suitable for byte decoding)
bool isValidHexBytes(string str) pure nothrow @safe @nogc
{
    return str.length % 2 == 0 && isValidHex(str);
}

/// Format hex with spaces between bytes
HexResult!string formatSpaced(string hex) pure @safe
{
    if (hex.length % 2 != 0)
        return HexResult!string.failure("Odd hex string length");
    if (hex.length == 0)
        return HexResult!string.success("");

    immutable pairs = hex.length / 2;
    auto result = appender!string;
    result.reserve(pairs * 2 + pairs - 1);

    for (size_t i = 0; i < hex.length; i += 2)
    {
        if (i > 0)
            result ~= ' ';
        result ~= hex[i];
        result ~= hex[i + 1];
    }

    return HexResult!string.success(result[]);
}

/// Format hex with colons between bytes (MAC address style)
HexResult!string formatColons(string hex) pure @safe
{
    if (hex.length % 2 != 0)
        return HexResult!string.failure("Odd hex string length");
    if (hex.length == 0)
        return HexResult!string.success("");

    immutable pairs = hex.length / 2;
    auto result = appender!string;
    result.reserve(pairs * 2 + pairs - 1);

    for (size_t i = 0; i < hex.length; i += 2)
    {
        if (i > 0)
            result ~= ':';
        result ~= hex[i];
        result ~= hex[i + 1];
    }

    return HexResult!string.success(result[]);
}

/// Constant-time comparison of two byte arrays (timing attack resistant)
bool constantTimeEqual(const(ubyte)[] a, const(ubyte)[] b) pure nothrow @safe @nogc
{
    if (a.length != b.length)
        return false;

    ubyte diff = 0;
    foreach (i; 0 .. a.length)
    {
        diff |= a[i] ^ b[i];
    }

    return diff == 0;
}

/// Constant-time comparison of two hex strings (case-insensitive, timing attack resistant)
bool constantTimeEqualHex(string a, string b) pure nothrow @safe
{
    if (a.length != b.length)
        return false;

    ubyte diff = 0;
    foreach (i; 0 .. a.length)
    {
        immutable la = toLower(a[i]);
        immutable lb = toLower(b[i]);
        diff |= cast(ubyte)(la ^ lb);
    }

    return diff == 0;
}

/// Convert integer to hex string with minimum width (zero-padded)
string intToHex(ulong value, size_t minWidth = 0) pure @safe
{
    char[16] temp;
    size_t len = 0;

    if (value == 0)
    {
        temp[0] = '0';
        len = 1;
    }
    else
    {
        ulong v = value;
        while (v > 0)
        {
            temp[15 - len] = nibbleToHexChar(cast(ubyte)(v & 0xF));
            v >>= 4;
            len++;
        }
    }

    immutable actualLen = len > minWidth ? len : minWidth;
    auto result = appender!string;
    result.reserve(actualLen);

    // Add zero padding
    foreach (_; 0 .. actualLen - len)
    {
        result ~= '0';
    }

    // Copy digits
    foreach (i; 0 .. len)
    {
        result ~= temp[16 - len + i];
    }

    return result[];
}

/// Parse hex string to unsigned integer
Nullable!ulong hexToInt(string hex) pure nothrow @safe
{
    if (hex.length == 0 || hex.length > 16)
        return Nullable!ulong.init;

    ulong result = 0;
    foreach (c; hex)
    {
        immutable nibble = hexCharToNibble(c);
        if (nibble.isNull)
            return Nullable!ulong.init;
        result = (result << 4) | nibble.get;
    }

    return nullable(result);
}

/// Remove formatting characters (spaces, colons, hyphens) from hex string
string stripHexFormatting(string hex) pure @safe
{
    auto result = appender!string;
    result.reserve(hex.length);

    foreach (c; hex)
    {
        if (isHexChar(c))
            result ~= c;
    }

    return result[];
}

// Unit tests
unittest
{
    // Test encoding
    assert(encode([0xFF, 0x00, 0xAB]) == "ff00ab");
    assert(encodeUpper([0xFF, 0x00, 0xAB]) == "FF00AB");

    // Test decoding
    auto decoded = decode("ff00ab");
    assert(decoded.ok);
    assert(decoded.value == [0xFF, 0x00, 0xAB]);

    // Test invalid decode
    auto invalidOdd = decode("ff00a");
    assert(!invalidOdd.ok);
    auto invalidChar = decode("gg00");
    assert(!invalidChar.ok);

    // Test validation
    assert(isValidHex("abcdef0123456789"));
    assert(isValidHex("ABCDEF"));
    assert(!isValidHex("xyz"));
    assert(isValidHexBytes("aabb"));
    assert(!isValidHexBytes("aab")); // odd length

    // Test formatting
    auto spaced = formatSpaced("aabbcc");
    assert(spaced.ok);
    assert(spaced.value == "aa bb cc");

    auto colons = formatColons("aabbcc");
    assert(colons.ok);
    assert(colons.value == "aa:bb:cc");

    // Test constant-time comparison
    assert(constantTimeEqual([0xAA, 0xBB], [0xAA, 0xBB]));
    assert(!constantTimeEqual([0xAA, 0xBB], [0xAA, 0xBC]));
    assert(constantTimeEqualHex("aabb", "AABB"));
    assert(constantTimeEqualHex("ff00", "FF00"));
    assert(!constantTimeEqualHex("aabb", "aab0"));

    // Test integer to hex
    assert(intToHex(255, 2) == "ff");
    assert(intToHex(255, 4) == "00ff");
    assert(intToHex(0, 1) == "0");
    assert(intToHex(0xDEADBEEF) == "deadbeef");

    // Test hex to integer
    assert(hexToInt("ff").get == 255);
    assert(hexToInt("DEADBEEF").get == 0xDEADBEEF);
    assert(hexToInt("").isNull);

    // Test strip formatting
    assert(stripHexFormatting("aa bb cc") == "aabbcc");
    assert(stripHexFormatting("aa:bb:cc") == "aabbcc");
    assert(stripHexFormatting("aa-bb-cc") == "aabbcc");
}
