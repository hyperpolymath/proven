# Proven Ballerina Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Ballerina language bindings for **libproven**, a formally verified safety library.
All computation is performed in Idris 2 via the Zig FFI bridge. These bindings
call libproven through JNA (Java Native Access) on the JVM -- they do NOT
reimplement any logic.

## Architecture

```
Ballerina code  -->  JNA bridge (Java)  -->  libproven.so (C ABI)  -->  Zig FFI  -->  Idris 2 (verified)
```

Since Ballerina runs on the JVM, native C functions are accessed through JNA.
The Java bridge class `io.hyperpolymath.proven.ProvenNative` loads `libproven`
and exposes each C function as a static Java method annotated for Ballerina's
`@java:Method` interop.

## Modules

| Module | File | Description |
|--------|------|-------------|
| `lib_proven` | `modules/proven/lib_proven.bal` | JNA FFI declarations, types, status codes |
| `safe_math` | `modules/proven/safe_math.bal` | Overflow-safe arithmetic |
| `safe_string` | `modules/proven/safe_string.bal` | UTF-8 validation, SQL/HTML/JS escaping |
| `safe_path` | `modules/proven/safe_path.bal` | Directory traversal prevention |
| `safe_email` | `modules/proven/safe_email.bal` | RFC 5321 email validation |
| `safe_url` | `modules/proven/safe_url.bal` | RFC 3986 URL parsing |
| `safe_crypto` | `modules/proven/safe_crypto.bal` | Constant-time comparison, CSPRNG |
| `safe_json` | `modules/proven/safe_json.bal` | JSON validation and type detection |

## Usage

```ballerina
import hyperpolymath/proven;

public function main() returns error? {
    // Initialize the Proven runtime
    int initStatus = proven:provenInit();

    // Safe division -- returns error on division by zero
    int|error result = proven:safeDiv(100, 0);
    // result is error("division by zero")

    // Email validation
    boolean|error valid = proven:isValidEmail("user@example.com");
    // valid is true

    // Safe addition with overflow detection
    int|error sum = proven:safeAdd(9223372036854775800, 100);
    // sum is error("overflow") -- detected by Idris 2

    // JSON validation
    boolean|error isJson = proven:isValidJson("{\"key\": \"value\"}");
    // isJson is true

    // Cleanup
    proven:provenDeinit();
}
```

## Error Handling

All fallible operations return Ballerina's `T|error` union type. Use
`check` expressions or `is error` checks:

```ballerina
// Using check expression (propagates error)
int result = check proven:safeDiv(10, 3);

// Using explicit error check
int|error maybeResult = proven:safeDiv(10, 0);
if maybeResult is error {
    io:println("Error: " + maybeResult.message());
} else {
    io:println("Result: " + maybeResult.toString());
}
```

## JNA Bridge

The Java bridge class `io.hyperpolymath.proven.ProvenNative` is required at
runtime. It uses JNA to:

1. Load `libproven.so` (Linux) / `libproven.dylib` (macOS)
2. Map C struct types (`IntResult`, `BoolResult`, etc.) to Java classes
3. Expose extraction functions for reading struct fields from Ballerina

Place `jna-5.14.0.jar` in the `lib/` directory.

## Building

```bash
bal build
```

## Requirements

- Ballerina Swan Lake (2201.9.0+)
- JNA 5.14.0 JAR (`lib/jna-5.14.0.jar`)
- libproven shared library (`libproven.so` or `libproven.dylib`)
- Java bridge class (`io.hyperpolymath.proven.ProvenNative`)

## Package Configuration

See `Ballerina.toml`:
- Organization: `hyperpolymath`
- Package name: `proven`
- Version: `0.5.0`

## Status Codes

| Code | Constant | Meaning |
|------|----------|---------|
| 0 | `STATUS_OK` | Success |
| -1 | `STATUS_ERR_NULL_POINTER` | Null pointer passed |
| -2 | `STATUS_ERR_INVALID_ARGUMENT` | Invalid argument |
| -3 | `STATUS_ERR_OVERFLOW` | Integer overflow |
| -4 | `STATUS_ERR_UNDERFLOW` | Integer underflow |
| -5 | `STATUS_ERR_DIVISION_BY_ZERO` | Division by zero |
| -6 | `STATUS_ERR_PARSE_FAILURE` | Parse failure |
| -7 | `STATUS_ERR_VALIDATION_FAILED` | Validation failed |
| -8 | `STATUS_ERR_OUT_OF_BOUNDS` | Index out of bounds |
| -9 | `STATUS_ERR_ENCODING_ERROR` | Encoding error |
| -10 | `STATUS_ERR_ALLOCATION_FAILED` | Memory allocation failed |
| -99 | `STATUS_ERR_NOT_IMPLEMENTED` | Not implemented |
