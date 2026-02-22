// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_path.bal - Safe filesystem path operations via libproven FFI.
//
// Prevents directory traversal attacks and sanitizes filenames.
// All operations delegate to Idris 2 verified code.
// No path logic is implemented in Ballerina.

// Check if a path contains directory traversal sequences (e.g., "../").
//
// Returns true if traversal is detected, false if the path is safe,
// or error on FFI failure.
public isolated function hasTraversal(string path) returns boolean|error {
    byte[] data = path.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenPathHasTraversal(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractBoolResultStatus(result);
    if succeeded(status) {
        return extractBoolResultValue(result);
    }
    return statusToError(status);
}

// Sanitize a filename by removing dangerous characters.
//
// Returns the sanitized filename as a string, or error on failure.
public isolated function sanitizeFilename(string filename) returns string|error {
    byte[] data = filename.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenPathSanitizeFilename(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractStringResultStatus(result);
    if succeeded(status) {
        return extractStringResultValue(result);
    }
    return statusToError(status);
}
