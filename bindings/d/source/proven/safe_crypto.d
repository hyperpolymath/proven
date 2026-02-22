// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe cryptographic operations.
 *
 * Thin FFI wrapper around libproven's SafeCrypto module. Constant-time
 * comparison and random byte generation are performed in formally verified
 * Idris 2 code via the Zig FFI bridge. This module only marshals data
 * to/from the C ABI.
 */
module proven.safe_crypto;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Constant-time byte array comparison to prevent timing attacks.
/// Returns false if lengths differ or on FFI error.
bool constantTimeEquals(const(ubyte)[] a, const(ubyte)[] b) @trusted nothrow @nogc
{
    auto result = proven_crypto_constant_time_eq(
        a.ptr, a.length, b.ptr, b.length
    );
    if (provenFailed(result.status))
        return false;
    return result.value;
}

/// Constant-time string comparison to prevent timing attacks.
bool constantTimeEqualsString(string a, string b) @trusted nothrow
{
    return constantTimeEquals(
        cast(const(ubyte)[]) a,
        cast(const(ubyte)[]) b
    );
}

/// Generate cryptographically secure random bytes.
/// Returns null on failure.
ubyte[] randomBytes(size_t count) @trusted nothrow
{
    ubyte[] buf = new ubyte[count];
    auto status = proven_crypto_random_bytes(buf.ptr, count);
    if (status != ProvenStatus.ok)
        return null;
    return buf;
}

/// Generate random bytes as lowercase hex string.
string randomHex(size_t byteCount) @trusted nothrow
{
    auto bytes = randomBytes(byteCount);
    if (bytes is null)
        return null;
    auto result = proven_hex_encode(bytes.ptr, bytes.length, false);
    return provenStringToD(result);
}
