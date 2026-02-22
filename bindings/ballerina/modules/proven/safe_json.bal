// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_json.bal - Safe JSON validation via libproven FFI.
//
// Validates JSON strings and determines root value types without full parsing.
// All operations delegate to Idris 2 verified code.
// No JSON parsing logic is implemented in Ballerina.

// JSON value type constants.
public const JSON_TYPE_NULL = 0;
public const JSON_TYPE_BOOL = 1;
public const JSON_TYPE_NUMBER = 2;
public const JSON_TYPE_STRING = 3;
public const JSON_TYPE_ARRAY = 4;
public const JSON_TYPE_OBJECT = 5;
public const JSON_TYPE_INVALID = -1;

// Check if a string is valid JSON.
//
// Returns true if the input is syntactically valid JSON, false otherwise.
// Returns error on FFI failure.
public isolated function isValidJson(string input) returns boolean|error {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenJsonIsValid(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractBoolResultStatus(result);
    if succeeded(status) {
        return extractBoolResultValue(result);
    }
    return statusToError(status);
}

// Get the JSON value type at the root level.
//
// Returns one of the JSON_TYPE_* constants:
//   JSON_TYPE_NULL (0), JSON_TYPE_BOOL (1), JSON_TYPE_NUMBER (2),
//   JSON_TYPE_STRING (3), JSON_TYPE_ARRAY (4), JSON_TYPE_OBJECT (5),
//   JSON_TYPE_INVALID (-1).
public isolated function getJsonType(string input) returns int {
    byte[] data = input.toBytes();
    handle nativePtr = toNativeBytes(data);
    int result = provenJsonGetType(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    return result;
}

// Convert a JSON type code to a human-readable string.
public isolated function jsonTypeName(int typeCode) returns string {
    match typeCode {
        0 => { return "null"; }
        1 => { return "bool"; }
        2 => { return "number"; }
        3 => { return "string"; }
        4 => { return "array"; }
        5 => { return "object"; }
        _ => { return "invalid"; }
    }
}
