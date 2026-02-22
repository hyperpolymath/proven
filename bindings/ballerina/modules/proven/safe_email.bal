// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_email.bal - Safe email validation via libproven FFI.
//
// Validates email addresses according to RFC 5321/5322 simplified rules.
// All operations delegate to Idris 2 verified code.
// No validation logic is implemented in Ballerina.

// Validate an email address per RFC 5321 simplified rules.
//
// Returns true if the email address is valid, false if invalid,
// or error on FFI failure.
public isolated function isValidEmail(string email) returns boolean|error {
    byte[] data = email.toBytes();
    handle nativePtr = toNativeBytes(data);
    handle result = provenEmailIsValid(nativePtr, data.length());
    freeNativeBytes(nativePtr);
    int status = extractBoolResultStatus(result);
    if succeeded(status) {
        return extractBoolResultValue(result);
    }
    return statusToError(status);
}
