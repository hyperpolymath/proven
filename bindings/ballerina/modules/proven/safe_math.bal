// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// safe_math.bal - Safe arithmetic operations via libproven FFI.
//
// All operations delegate to the Idris 2 formally verified implementation
// through the Zig FFI layer. No arithmetic logic is implemented in Ballerina.

// Safe addition with overflow detection.
//
// Returns the sum, or an error if the result would exceed i64 range.
public isolated function safeAdd(int a, int b) returns int|error {
    handle result = provenMathAddChecked(a, b);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Safe subtraction with underflow detection.
//
// Returns the difference, or an error if the result would go below i64 minimum.
public isolated function safeSub(int a, int b) returns int|error {
    handle result = provenMathSubChecked(a, b);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Safe multiplication with overflow detection.
//
// Returns the product, or an error if the result would overflow i64.
public isolated function safeMul(int a, int b) returns int|error {
    handle result = provenMathMulChecked(a, b);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Safe division with zero-check.
//
// Returns the quotient, or an error if the denominator is 0 or if
// INT64_MIN / -1 would overflow.
public isolated function safeDiv(int numerator, int denominator) returns int|error {
    handle result = provenMathDiv(numerator, denominator);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Safe modulo with zero-check.
//
// Returns the remainder, or an error if the denominator is 0.
public isolated function safeMod(int numerator, int denominator) returns int|error {
    handle result = provenMathMod(numerator, denominator);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Safe absolute value.
//
// Returns the absolute value, or an error for i64 minimum (whose absolute
// value cannot be represented as a positive i64).
public isolated function safeAbs(int n) returns int|error {
    handle result = provenMathAbsSafe(n);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}

// Clamp value to range [lo, hi].
//
// Always succeeds. Returns lo if value < lo, hi if value > hi.
public isolated function safeClamp(int lo, int hi, int value) returns int {
    return provenMathClamp(lo, hi, value);
}

// Integer power with overflow checking.
//
// Returns base^exp, or an error if the result would overflow i64.
public isolated function safePow(int base, int exp) returns int|error {
    handle result = provenMathPowChecked(base, exp);
    int status = extractIntResultStatus(result);
    if succeeded(status) {
        return extractIntResultValue(result);
    }
    return statusToError(status);
}
