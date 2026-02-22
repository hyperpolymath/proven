// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_string.bal - Safe string operations via libproven FFI.
//
// Provides UTF-8 validation and injection-safe escaping for SQL, HTML,
// and JavaScript contexts. All operations delegate to Idris 2 verified code.
// No string logic is implemented in Ballerina.

// Check if a byte array is valid UTF-8.
//
// Returns true if valid UTF-8, false if invalid, or error on FFI failure.
public isolated function isValidUtf8(byte[] data) returns boolean|error {
    handle nativePtr = toNativeBytes(data);
    handle result = provenStringIsValidUtf8(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractBoolResultStatus(result);
    if succeeded(status) {
        return extractBoolResultValue(result);
    }
    return statusToError(status);
}

// Escape a string for safe use in SQL queries.
//
// Escapes single quotes by doubling them.
// Returns the escaped string, or error on failure.
public isolated function escapeSql(string input) returns string|error {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenStringEscapeSql(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractStringResultStatus(result);
    if succeeded(status) {
        return extractStringResultValue(result);
    }
    return statusToError(status);
}

// Escape a string for safe use in HTML content.
//
// Escapes <, >, &, ", and ' characters.
// Returns the escaped string, or error on failure.
public isolated function escapeHtml(string input) returns string|error {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenStringEscapeHtml(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractStringResultStatus(result);
    if succeeded(status) {
        return extractStringResultValue(result);
    }
    return statusToError(status);
}

// Escape a string for safe use in JavaScript string literals.
//
// Returns the escaped string, or error on failure.
public isolated function escapeJs(string input) returns string|error {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenStringEscapeJs(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractStringResultStatus(result);
    if succeeded(status) {
        return extractStringResultValue(result);
    }
    return statusToError(status);
}
