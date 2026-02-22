// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe UUID generation, parsing, and validation operations.
 *
 * Thin FFI wrapper around libproven's SafeUUID module. UUID generation
 * (v4 random) and parsing are performed in formally verified Idris 2 code.
 * This module only marshals data to/from the C ABI.
 */
module proven.safe_uuid;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// A validated UUID (128 bits / 16 bytes).
struct UUID
{
    /// The 16 bytes comprising the UUID.
    ubyte[16] bytes;

    /// The nil UUID (all zeros).
    static immutable UUID nil = UUID([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]);

    /// Check if this is the nil UUID.
    bool isNil() const @trusted nothrow @nogc
    {
        ProvenUUID u;
        u.bytes = bytes;
        return proven_uuid_is_nil(u);
    }

    /// Get the UUID version.
    ubyte getVersion() const @trusted nothrow @nogc
    {
        ProvenUUID u;
        u.bytes = bytes;
        return proven_uuid_version(u);
    }

    /// Compare two UUIDs for equality.
    bool opEquals(const UUID other) const pure nothrow @safe @nogc
    {
        return bytes == other.bytes;
    }

    /// Compare two UUIDs for ordering.
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
}

/// UUID parsing result.
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

/// Parse UUID from canonical string format (8-4-4-4-12).
UuidResult parseUuid(string str) @trusted nothrow
{
    if (str.length == 0)
        return UuidResult.failure("Empty UUID string");

    auto result = proven_uuid_parse(
        cast(const(ubyte)*) str.ptr, str.length
    );

    if (provenFailed(result.status))
        return UuidResult.failure("Invalid UUID format");

    UUID uuid;
    uuid.bytes = result.uuid.bytes;
    return UuidResult.success(uuid);
}

/// Format UUID to canonical string (8-4-4-4-12, lowercase).
string formatUuid(const UUID uuid) @trusted nothrow
{
    ProvenUUID u;
    u.bytes = uuid.bytes;
    auto result = proven_uuid_to_string(u);
    return provenStringToD(result);
}

/// Generate a v4 (random) UUID.
UUID generateUuidV4() @trusted nothrow @nogc
{
    auto result = proven_uuid_v4();
    UUID uuid;
    if (provenFailed(result.status))
        return UUID.nil;
    uuid.bytes = result.uuid.bytes;
    return uuid;
}

/// Check if string is valid UUID format.
bool isValidUuid(string str) @trusted nothrow
{
    return parseUuid(str).ok;
}
