// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * Safe UUID generation, parsing, and validation operations.
 * Follows RFC 4122 specification.
 */
module proven.safe_uuid;

import std.array : appender;
import std.algorithm : canFind;
import std.random : Random, unpredictableSeed;
import std.typecons : Nullable, nullable;

/// UUID version types (RFC 4122)
enum UuidVersion : ubyte
{
    nil = 0,
    v1 = 1, /// Time-based UUID
    v2 = 2, /// DCE Security UUID
    v3 = 3, /// Name-based UUID using MD5
    v4 = 4, /// Random UUID
    v5 = 5, /// Name-based UUID using SHA-1
}

/// UUID variant types (RFC 4122)
enum UuidVariant
{
    ncs,      /// Reserved for NCS backward compatibility
    rfc4122,  /// The variant specified in RFC 4122
    microsoft, /// Reserved for Microsoft backward compatibility
    future,    /// Reserved for future use
}

/// A validated UUID (128 bits / 16 bytes)
struct UUID
{
    /// The 16 bytes comprising the UUID
    ubyte[16] bytes;

    /// The nil UUID (all zeros)
    static immutable UUID nil = UUID([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

    /// DNS namespace UUID for v3/v5 generation
    static immutable UUID namespaceDns = UUID([
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ]);

    /// URL namespace UUID for v3/v5 generation
    static immutable UUID namespaceUrl = UUID([
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ]);

    /// OID namespace UUID for v3/v5 generation
    static immutable UUID namespaceOid = UUID([
        0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ]);

    /// X.500 namespace UUID for v3/v5 generation
    static immutable UUID namespaceX500 = UUID([
        0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
    ]);

    /// Get the UUID version
    UuidVersion getVersion() const pure nothrow @safe @nogc
    {
        immutable versionNibble = (bytes[6] >> 4) & 0x0F;
        switch (versionNibble)
        {
        case 1:
            return UuidVersion.v1;
        case 2:
            return UuidVersion.v2;
        case 3:
            return UuidVersion.v3;
        case 4:
            return UuidVersion.v4;
        case 5:
            return UuidVersion.v5;
        default:
            return UuidVersion.nil;
        }
    }

    /// Get the UUID variant
    UuidVariant getVariant() const pure nothrow @safe @nogc
    {
        immutable variantByte = bytes[8];
        if ((variantByte >> 7) == 0)
            return UuidVariant.ncs;
        if ((variantByte >> 6) == 0b10)
            return UuidVariant.rfc4122;
        if ((variantByte >> 5) == 0b110)
            return UuidVariant.microsoft;
        return UuidVariant.future;
    }

    /// Check if this is the nil UUID
    bool isNil() const pure nothrow @safe @nogc
    {
        foreach (b; bytes)
        {
            if (b != 0)
                return false;
        }
        return true;
    }

    /// Compare two UUIDs for equality
    bool opEquals(const UUID other) const pure nothrow @safe @nogc
    {
        return bytes == other.bytes;
    }

    /// Compare two UUIDs for ordering
    int opCmp(const UUID other) const pure nothrow @safe @nogc
    {
        foreach (i; 0 .. 16)
        {
            if (bytes[i] < other.bytes[i])
                return -1;
            if (bytes[i] > other.bytes[i])
                return 1;
        }
        return 0;
    }

    /// Hash function for associative arrays
    size_t toHash() const pure nothrow @safe @nogc
    {
        size_t hash = 0;
        foreach (b; bytes)
        {
            hash = hash * 31 + b;
        }
        return hash;
    }
}

/// UUID parsing and formatting error types
struct UuidResult
{
    UUID uuid;
    string error;
    bool ok;

    static UuidResult success(UUID uuid)
    {
        return UuidResult(uuid, "", true);
    }

    static UuidResult failure(string error)
    {
        return UuidResult(UUID.nil, error, false);
    }
}

/// Parse UUID from canonical string format (8-4-4-4-12)
UuidResult parseUuid(string str) pure @safe
{
    if (str.length != 36)
        return UuidResult.failure("Invalid UUID length (expected 36 characters)");

    // Verify hyphens at correct positions
    if (str[8] != '-' || str[13] != '-' || str[18] != '-' || str[23] != '-')
        return UuidResult.failure("Invalid UUID format (missing hyphens)");

    UUID result;
    size_t byteIdx = 0;
    size_t strIdx = 0;

    while (byteIdx < 16)
    {
        // Skip hyphens
        if (strIdx == 8 || strIdx == 13 || strIdx == 18 || strIdx == 23)
            strIdx++;

        immutable high = hexCharToNibble(str[strIdx]);
        immutable low = hexCharToNibble(str[strIdx + 1]);

        if (high.isNull || low.isNull)
            return UuidResult.failure("Invalid hexadecimal character");

        result.bytes[byteIdx] = cast(ubyte)((high.get << 4) | low.get);
        byteIdx++;
        strIdx += 2;
    }

    return UuidResult.success(result);
}

/// Format UUID to canonical string (8-4-4-4-12)
string formatUuid(const UUID uuid) pure @safe
{
    immutable hexChars = "0123456789abcdef";
    auto result = appender!string;
    result.reserve(36);

    foreach (i, b; uuid.bytes)
    {
        result ~= hexChars[b >> 4];
        result ~= hexChars[b & 0x0F];

        // Add hyphens at positions after bytes 4, 6, 8, 10
        if (i == 3 || i == 5 || i == 7 || i == 9)
            result ~= '-';
    }

    return result[];
}

/// Format UUID to uppercase string
string formatUuidUpper(const UUID uuid) pure @safe
{
    immutable hexChars = "0123456789ABCDEF";
    auto result = appender!string;
    result.reserve(36);

    foreach (i, b; uuid.bytes)
    {
        result ~= hexChars[b >> 4];
        result ~= hexChars[b & 0x0F];

        if (i == 3 || i == 5 || i == 7 || i == 9)
            result ~= '-';
    }

    return result[];
}

/// Generate a v4 (random) UUID from provided random bytes
UUID v4FromBytes(ubyte[16] randomBytes) pure nothrow @safe @nogc
{
    UUID uuid;
    uuid.bytes = randomBytes;

    // Set version to 4
    uuid.bytes[6] = (uuid.bytes[6] & 0x0F) | 0x40;

    // Set variant to RFC 4122
    uuid.bytes[8] = (uuid.bytes[8] & 0x3F) | 0x80;

    return uuid;
}

/// Generate a v4 (random) UUID
UUID generateUuidV4() @trusted
{
    import core.stdc.stdio : FILE, fopen, fread, fclose;

    ubyte[16] randomBytes;

    // Try /dev/urandom on POSIX systems
    version (Posix)
    {
        auto urandomFile = fopen("/dev/urandom", "rb");
        if (urandomFile !is null)
        {
            scope (exit)
                fclose(urandomFile);
            fread(randomBytes.ptr, 1, 16, urandomFile);
            return v4FromBytes(randomBytes);
        }
    }

    // Fallback to std.random
    auto rng = Random(unpredictableSeed);
    foreach (ref b; randomBytes)
    {
        b = cast(ubyte) rng.front;
        rng.popFront();
    }

    return v4FromBytes(randomBytes);
}

/// Check if string is valid UUID format
bool isValidUuid(string str) pure @safe
{
    return parseUuid(str).ok;
}

/// Convert hex character to nibble value (0-15)
private Nullable!ubyte hexCharToNibble(char c) pure nothrow @safe @nogc
{
    if (c >= '0' && c <= '9')
        return nullable(cast(ubyte)(c - '0'));
    if (c >= 'a' && c <= 'f')
        return nullable(cast(ubyte)(c - 'a' + 10));
    if (c >= 'A' && c <= 'F')
        return nullable(cast(ubyte)(c - 'A' + 10));
    return Nullable!ubyte.init;
}

// Unit tests
unittest
{
    // Test parsing valid UUID
    auto result = parseUuid("550e8400-e29b-41d4-a716-446655440000");
    assert(result.ok);
    assert(!result.uuid.isNil);
    assert(result.uuid.getVersion == UuidVersion.v4);

    // Test parsing nil UUID
    auto nilResult = parseUuid("00000000-0000-0000-0000-000000000000");
    assert(nilResult.ok);
    assert(nilResult.uuid.isNil);

    // Test formatting
    auto uuid = parseUuid("550e8400-e29b-41d4-a716-446655440000");
    assert(formatUuid(uuid.uuid) == "550e8400-e29b-41d4-a716-446655440000");

    // Test v4 generation
    auto v4Uuid = generateUuidV4();
    assert(v4Uuid.getVersion == UuidVersion.v4);
    assert(v4Uuid.getVariant == UuidVariant.rfc4122);

    // Test invalid UUID
    assert(!isValidUuid("not-a-uuid"));
    assert(!isValidUuid("550e8400e29b-41d4-a716-446655440000")); // Missing hyphen

    // Test comparison
    auto uuid1 = parseUuid("550e8400-e29b-41d4-a716-446655440000").uuid;
    auto uuid2 = parseUuid("550e8400-e29b-41d4-a716-446655440000").uuid;
    auto uuid3 = parseUuid("650e8400-e29b-41d4-a716-446655440000").uuid;
    assert(uuid1 == uuid2);
    assert(uuid1 != uuid3);
    assert(uuid1 < uuid3);
}
