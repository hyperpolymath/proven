// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe hexadecimal encoding and decoding operations.
 *
 * Thin FFI wrapper around libproven's SafeHex module. Hex encoding,
 * decoding, and validation are performed in formally verified Idris 2
 * code. This module only marshals data to/from the C ABI.
 */
module proven.safe_hex;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// Hex operation result type.
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

/// Encode bytes to lowercase hex string.
string encode(const(ubyte)[] bytes) @trusted nothrow
{
    if (bytes.length == 0)
        return "";
    auto result = proven_hex_encode(bytes.ptr, bytes.length, false);
    return provenStringToD(result);
}

/// Encode bytes to uppercase hex string.
string encodeUpper(const(ubyte)[] bytes) @trusted nothrow
{
    if (bytes.length == 0)
        return "";
    auto result = proven_hex_encode(bytes.ptr, bytes.length, true);
    return provenStringToD(result);
}

/// Decode hex string to bytes.
HexResult!(ubyte[]) decode(string hex) @trusted nothrow
{
    if (hex.length == 0)
        return HexResult!(ubyte[]).success([]);
    if (hex.length % 2 != 0)
        return HexResult!(ubyte[]).failure("Odd hex string length");

    auto result = proven_hex_decode(
        cast(const(ubyte)*) hex.ptr, hex.length
    );

    if (provenFailed(result.status) || result.data is null)
        return HexResult!(ubyte[]).failure("Invalid hexadecimal");

    // Copy data before freeing
    ubyte[] decoded = result.data[0 .. result.length].dup;

    // Free the C-allocated memory
    proven_hex_free(&result);

    return HexResult!(ubyte[]).success(decoded);
}
