// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe string operations for XSS prevention and sanitization.
 */
module proven.safe_string;

import std.array : Appender, appender;
import std.base64 : Base64;
import std.conv : to;
import std.string : toLower, strip, replace;
import std.uni : isAlphaNum;
import std.algorithm : map, filter;
import std.range : chain;
import std.typecons : Nullable, nullable;

/// Escape HTML special characters.
string escapeHtml(string input) pure @safe
{
    auto result = appender!string;
    result.reserve(input.length * 2);

    foreach (c; input)
    {
        switch (c)
        {
        case '&':
            result ~= "&amp;";
            break;
        case '<':
            result ~= "&lt;";
            break;
        case '>':
            result ~= "&gt;";
            break;
        case '"':
            result ~= "&quot;";
            break;
        case '\'':
            result ~= "&#x27;";
            break;
        default:
            result ~= c;
        }
    }
    return result[];
}

/// Escape for SQL (single quotes).
string escapeSql(string input) pure @safe
{
    return input.replace("'", "''");
}

/// Escape for JavaScript strings.
string escapeJs(string input) pure @safe
{
    auto result = appender!string;
    result.reserve(input.length * 2);

    foreach (c; input)
    {
        switch (c)
        {
        case '\\':
            result ~= "\\\\";
            break;
        case '"':
            result ~= "\\\"";
            break;
        case '\'':
            result ~= "\\'";
            break;
        case '\n':
            result ~= "\\n";
            break;
        case '\r':
            result ~= "\\r";
            break;
        case '\t':
            result ~= "\\t";
            break;
        case '<':
            result ~= "\\x3c";
            break;
        case '>':
            result ~= "\\x3e";
            break;
        default:
            result ~= c;
        }
    }
    return result[];
}

/// URL encode a string.
string urlEncode(string input) pure @safe
{
    import std.format : format;

    auto result = appender!string;
    result.reserve(input.length * 3);

    foreach (c; input)
    {
        if (isUrlSafe(c))
        {
            result ~= c;
        }
        else
        {
            result ~= format!"%%%02X"(cast(ubyte) c);
        }
    }
    return result[];
}

/// Check if character is URL-safe.
private bool isUrlSafe(char c) pure nothrow @safe @nogc
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
        c == '-' || c == '_' || c == '.' || c == '~';
}

/// Sanitize string to only allow safe characters.
string sanitize(string input, string allowed) pure @safe
{
    auto result = appender!string;
    result.reserve(input.length);

    foreach (c; input)
    {
        import std.algorithm : canFind;

        if (allowed.canFind(c))
        {
            result ~= c;
        }
    }
    return result[];
}

/// Default sanitization (alphanumeric + underscore + hyphen).
string sanitizeDefault(string input) pure @safe
{
    return sanitize(input, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-");
}

/// Convert to slug.
string slugify(string input) pure @safe
{
    string result = input.toLower();

    auto filtered = appender!string;
    foreach (c; result)
    {
        if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == ' ' || c == '-')
        {
            filtered ~= c;
        }
    }
    result = filtered[];

    // Replace spaces with hyphens
    result = result.replace(" ", "-");

    // Collapse multiple hyphens
    while (result.length > 0)
    {
        auto newResult = result.replace("--", "-");
        if (newResult == result)
            break;
        result = newResult;
    }

    // Trim hyphens
    while (result.length > 0 && result[0] == '-')
        result = result[1 .. $];
    while (result.length > 0 && result[$ - 1] == '-')
        result = result[0 .. $ - 1];

    return result;
}

/// Truncate string safely.
string truncate(string input, size_t maxLength, string suffix = "...") pure @safe
{
    if (input.length <= maxLength)
        return input;
    if (maxLength <= suffix.length)
        return suffix;
    return input[0 .. maxLength - suffix.length] ~ suffix;
}

/// Remove control characters.
string stripControlChars(string input) pure @safe
{
    auto result = appender!string;
    result.reserve(input.length);

    foreach (c; input)
    {
        if (c >= 32 && c != 127)
        {
            result ~= c;
        }
    }
    return result[];
}

/// Normalize whitespace.
string normalizeWhitespace(string input) pure @safe
{
    auto result = appender!string;
    bool prevSpace = false;

    foreach (c; input.strip())
    {
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
        {
            if (!prevSpace)
            {
                result ~= ' ';
                prevSpace = true;
            }
        }
        else
        {
            result ~= c;
            prevSpace = false;
        }
    }
    return result[];
}

/// Check if string contains only ASCII.
bool isAsciiOnly(string input) pure nothrow @safe @nogc
{
    foreach (c; input)
    {
        if (c >= 128)
            return false;
    }
    return true;
}

/// Check if string contains only printable ASCII.
bool isPrintableAscii(string input) pure nothrow @safe @nogc
{
    foreach (c; input)
    {
        if (c < 32 || c >= 127)
            return false;
    }
    return true;
}

/// Base64 encode.
string base64Encode(string input) @safe
{
    return Base64.encode(cast(const(ubyte)[]) input);
}

/// Base64 decode.
Nullable!string base64Decode(string input) @safe
{
    try
    {
        auto decoded = Base64.decode(input);
        return nullable(cast(string) decoded);
    }
    catch (Exception)
    {
        return Nullable!string.init;
    }
}

/// Hex encode bytes.
string hexEncode(const(ubyte)[] input) pure @safe
{
    import std.format : format;

    auto result = appender!string;
    result.reserve(input.length * 2);

    foreach (b; input)
    {
        result ~= format!"%02x"(b);
    }
    return result[];
}

/// Hex decode to bytes.
Nullable!(ubyte[]) hexDecode(string input) pure @safe
{
    if (input.length % 2 != 0)
        return typeof(return).init;

    ubyte[] result;
    result.reserve(input.length / 2);

    for (size_t i = 0; i < input.length; i += 2)
    {
        auto high = hexCharToInt(input[i]);
        auto low = hexCharToInt(input[i + 1]);
        if (high.isNull || low.isNull)
            return typeof(return).init;
        result ~= cast(ubyte)((high.get << 4) | low.get);
    }
    return nullable(result);
}

/// Convert hex character to integer.
private Nullable!int hexCharToInt(char c) pure nothrow @safe @nogc
{
    if (c >= '0' && c <= '9')
        return nullable(cast(int)(c - '0'));
    if (c >= 'a' && c <= 'f')
        return nullable(cast(int)(c - 'a' + 10));
    if (c >= 'A' && c <= 'F')
        return nullable(cast(int)(c - 'A' + 10));
    return Nullable!int.init;
}

// Unit tests
unittest
{
    assert(escapeHtml("<script>") == "&lt;script&gt;");
    assert(escapeSql("O'Brien") == "O''Brien");
    assert(slugify("Hello World!") == "hello-world");
    assert(truncate("Hello World", 8) == "Hello...");
}
