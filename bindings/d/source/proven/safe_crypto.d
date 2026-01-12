// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe cryptographic operations.
 */
module proven.safe_crypto;

import std.digest.sha : SHA256, SHA512;
import std.digest.md : MD5;
import std.digest.hmac : HMAC;
import std.digest : toHexString, LetterCase;
import std.base64 : Base64;
import std.array : appender;
import std.random : Random, unpredictableSeed;
import std.string : toLower;
import std.typecons : Nullable, nullable;

/// Constant-time byte array comparison to prevent timing attacks.
bool constantTimeEquals(const(ubyte)[] a, const(ubyte)[] b) pure nothrow @safe @nogc
{
    if (a.length != b.length)
        return false;

    ubyte result = 0;
    foreach (i; 0 .. a.length)
    {
        result |= a[i] ^ b[i];
    }
    return result == 0;
}

/// Constant-time string comparison.
bool constantTimeEqualsString(string a, string b) pure nothrow @safe
{
    return constantTimeEquals(cast(const(ubyte)[]) a, cast(const(ubyte)[]) b);
}

/// Generate cryptographically secure random bytes.
ubyte[] randomBytes(size_t count) @trusted
{
    import core.stdc.stdlib : malloc, free;
    import core.stdc.stdio : FILE, fopen, fread, fclose;

    ubyte[] result = new ubyte[count];

    // Try /dev/urandom on POSIX systems
    version (Posix)
    {
        auto f = fopen("/dev/urandom", "rb");
        if (f !is null)
        {
            scope (exit)
                fclose(f);
            fread(result.ptr, 1, count, f);
            return result;
        }
    }

    // Fallback to std.random (less secure but better than nothing)
    auto rng = Random(unpredictableSeed);
    foreach (ref b; result)
    {
        b = cast(ubyte) rng.front;
        rng.popFront();
    }
    return result;
}

/// Convert bytes to hex string.
string bytesToHex(const(ubyte)[] bytes) pure @safe
{
    return toHexString!(LetterCase.lower)(bytes).idup;
}

/// Generate random bytes as hex string.
string randomHex(size_t byteCount) @trusted
{
    return bytesToHex(randomBytes(byteCount));
}

/// Generate random bytes as base64 string.
string randomBase64(size_t byteCount) @trusted
{
    return Base64.encode(randomBytes(byteCount));
}

/// Generate URL-safe random string.
string randomUrlSafe(size_t byteCount) @trusted
{
    import std.string : replace, stripRight;

    string encoded = Base64.encode(randomBytes(byteCount));
    return encoded.replace("+", "-").replace("/", "_").stripRight("=");
}

/// Generate random integer in range [min, max].
int randomInt(int minVal, int maxVal) @trusted
{
    if (minVal > maxVal)
    {
        auto tmp = minVal;
        minVal = maxVal;
        maxVal = tmp;
    }

    if (minVal == maxVal)
        return minVal;

    auto bytes = randomBytes(4);
    uint value = (cast(uint) bytes[0]) | (cast(uint) bytes[1] << 8) | (
        cast(uint) bytes[2] << 16) | (cast(uint) bytes[3] << 24);
    return minVal + cast(int)(value % cast(uint)(maxVal - minVal + 1));
}

/// Generate a secure token.
string generateToken(size_t length = 32) @trusted
{
    return randomUrlSafe(length);
}

/// Generate token with default length.
string generateTokenDefault() @trusted
{
    return generateToken(32);
}

/// Hash a string with SHA-256.
string sha256Hash(string input) pure @safe
{
    auto digest = SHA256();
    digest.put(cast(const(ubyte)[]) input);
    return bytesToHex(digest.finish()[]);
}

/// Hash bytes with SHA-256.
string sha256HashBytes(const(ubyte)[] input) pure @safe
{
    auto digest = SHA256();
    digest.put(input);
    return bytesToHex(digest.finish()[]);
}

/// Hash a string with SHA-512.
string sha512Hash(string input) pure @safe
{
    auto digest = SHA512();
    digest.put(cast(const(ubyte)[]) input);
    return bytesToHex(digest.finish()[]);
}

/// Hash bytes with SHA-512.
string sha512HashBytes(const(ubyte)[] input) pure @safe
{
    auto digest = SHA512();
    digest.put(input);
    return bytesToHex(digest.finish()[]);
}

/// Compute HMAC-SHA256.
string hmacSha256(string key, string message) pure @safe
{
    auto hmac = HMAC!SHA256(cast(const(ubyte)[]) key);
    hmac.put(cast(const(ubyte)[]) message);
    return bytesToHex(hmac.finish()[]);
}

/// Compute HMAC-SHA256 with bytes.
string hmacSha256Bytes(const(ubyte)[] key, const(ubyte)[] message) pure @safe
{
    auto hmac = HMAC!SHA256(key);
    hmac.put(message);
    return bytesToHex(hmac.finish()[]);
}

/// Compute HMAC-SHA512.
string hmacSha512(string key, string message) pure @safe
{
    auto hmac = HMAC!SHA512(cast(const(ubyte)[]) key);
    hmac.put(cast(const(ubyte)[]) message);
    return bytesToHex(hmac.finish()[]);
}

/// Verify HMAC using constant-time comparison.
bool verifyHmacSha256(string key, string message, string expectedMac) pure @safe
{
    return constantTimeEqualsString(hmacSha256(key, message), expectedMac);
}

/// Verify HMAC-SHA512 using constant-time comparison.
bool verifyHmacSha512(string key, string message, string expectedMac) pure @safe
{
    return constantTimeEqualsString(hmacSha512(key, message), expectedMac);
}

/// Hash a string with MD5 (NOT for security, only for checksums).
string md5Hash(string input) pure @safe
{
    auto digest = MD5();
    digest.put(cast(const(ubyte)[]) input);
    return bytesToHex(digest.finish()[]);
}

/// Generate a random password.
string generatePassword(size_t length, bool includeUppercase = true, bool includeLowercase = true,
    bool includeNumbers = true, bool includeSymbols = true) @trusted
{
    string chars = "";
    if (includeLowercase)
        chars ~= "abcdefghijklmnopqrstuvwxyz";
    if (includeUppercase)
        chars ~= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    if (includeNumbers)
        chars ~= "0123456789";
    if (includeSymbols)
        chars ~= "!@#$%^&*()_+-=[]{}|;:,.<>?";

    if (chars.length == 0)
        return "";

    auto bytes = randomBytes(length);
    auto result = appender!string;
    result.reserve(length);

    foreach (b; bytes)
    {
        result ~= chars[b % chars.length];
    }
    return result[];
}

/// Generate password with defaults.
string generatePasswordDefault() @trusted
{
    return generatePassword(16, true, true, true, true);
}

/// Securely wipe a byte array (best effort).
void secureWipe(ref ubyte[] data) nothrow @safe @nogc
{
    foreach (ref b; data)
    {
        b = 0;
    }
}

// Unit tests
unittest
{
    assert(sha256Hash("test") == "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08");
    assert(constantTimeEqualsString("hello", "hello"));
    assert(!constantTimeEqualsString("hello", "world"));
}
