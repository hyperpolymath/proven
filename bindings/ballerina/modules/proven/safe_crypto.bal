// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_crypto.bal - Safe cryptographic primitives via libproven FFI.
//
// Provides constant-time comparison and cryptographic random byte generation.
// All operations delegate to Idris 2 verified code.
// No cryptographic logic is implemented in Ballerina.

// Constant-time byte comparison (timing-attack resistant).
//
// Returns true if both byte arrays are identical. The comparison takes the
// same amount of time regardless of where a mismatch occurs. Returns false
// if lengths differ. Returns error on FFI failure.
public isolated function constantTimeEq(byte[] a, byte[] b) returns boolean|error {
    handle nativePtrA = toNativeBytes(a);
    handle nativePtrB = toNativeBytes(b);
    handle result = provenCryptoConstantTimeEq(nativePtrA, a.length(), nativePtrB, b.length());
    freeNativeBytes(nativePtrA);
    freeNativeBytes(nativePtrB);
    int status = extractBoolResultStatus(result);
    if succeeded(status) {
        return extractBoolResultValue(result);
    }
    return statusToError(status);
}

// Fill a byte array with cryptographically secure random bytes.
//
// Uses the Idris 2 verified CSPRNG implementation.
// Returns the filled byte array, or error on failure.
public isolated function randomBytes(int len) returns byte[]|error {
    byte[] buf = [];
    // Pre-allocate the array with zeros.
    int i = 0;
    while i < len {
        buf.push(0);
        i = i + 1;
    }
    handle nativePtr = toNativeBytes(buf);
    int status = provenCryptoRandomBytes(nativePtr, len);
    if succeeded(status) {
        byte[] result = readNativeBytes(nativePtr, len);
        freeNativeBytes(nativePtr);
        return result;
    }
    freeNativeBytes(nativePtr);
    return statusToError(status);
}

// JNA helper: Read bytes from native memory into a Ballerina byte array.
isolated function readNativeBytes(handle ptr, int len) returns byte[] = @java:Method {
    name: "readNativeBytes",
    'class: "io.hyperpolymath.proven.ProvenNative"
} external;
