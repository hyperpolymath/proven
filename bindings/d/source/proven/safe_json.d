// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Safe JSON parsing and validation operations.
 *
 * Thin FFI wrapper around libproven's SafeJson module. JSON syntax
 * validation and type detection are performed in formally verified
 * Idris 2 code. This module only marshals data to/from the C ABI.
 */
module proven.safe_json;

import proven.ffi;
import std.typecons : Nullable, nullable;

pragma(lib, "proven");

/// JSON value types (mirrors libproven's ProvenJsonType).
enum JsonType
{
    null_,
    boolean,
    number,
    string_,
    array,
    object,
}

/// Check if string is valid JSON.
bool isValidJson(string jsonString) @trusted nothrow @nogc
{
    if (jsonString.length == 0)
        return false;
    auto result = proven_json_is_valid(
        cast(const(ubyte)*) jsonString.ptr, jsonString.length
    );
    if (provenFailed(result.status))
        return false;
    return result.value;
}

/// Detect the type of a JSON value at root level.
Nullable!JsonType detectJsonType(string jsonString) @trusted nothrow @nogc
{
    if (jsonString.length == 0)
        return Nullable!JsonType.init;

    auto ffiType = proven_json_get_type(
        cast(const(ubyte)*) jsonString.ptr, jsonString.length
    );

    final switch (ffiType)
    {
    case ProvenJsonType.null_:
        return nullable(JsonType.null_);
    case ProvenJsonType.bool_:
        return nullable(JsonType.boolean);
    case ProvenJsonType.number:
        return nullable(JsonType.number);
    case ProvenJsonType.string_:
        return nullable(JsonType.string_);
    case ProvenJsonType.array:
        return nullable(JsonType.array);
    case ProvenJsonType.object:
        return nullable(JsonType.object);
    case ProvenJsonType.invalid:
        return Nullable!JsonType.init;
    }
}
