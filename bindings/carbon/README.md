# Proven Carbon Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Carbon language bindings for **libproven**, a formally verified safety library.
All computation is performed in Idris 2 via the Zig FFI bridge. These bindings
are thin wrappers that call libproven through Carbon's native C interop -- they
do NOT reimplement any logic.

## Architecture

```
Carbon code  -->  libproven.so (C ABI)  -->  Zig FFI bridge  -->  Idris 2 (verified)
```

## Modules

| Module | File | Description |
|--------|------|-------------|
| `lib_proven` | `src/proven/lib_proven.carbon` | C import declarations, types, status codes |
| `SafeMath` | `src/proven/safe_math.carbon` | Overflow-safe arithmetic |
| `SafeString` | `src/proven/safe_string.carbon` | UTF-8 validation, SQL/HTML/JS escaping |
| `SafePath` | `src/proven/safe_path.carbon` | Directory traversal prevention |
| `SafeEmail` | `src/proven/safe_email.carbon` | RFC 5321 email validation |
| `SafeUrl` | `src/proven/safe_url.carbon` | RFC 3986 URL parsing |
| `SafeCrypto` | `src/proven/safe_crypto.carbon` | Constant-time comparison, CSPRNG |
| `SafeJson` | `src/proven/safe_json.carbon` | JSON validation and type detection |

## Usage

```carbon
import Proven library "lib_proven";
import Proven library "safe_math";

fn Main() -> i32 {
  proven_init();

  // Safe division -- returns None on division by zero
  var result: Optional(i64) = SafeMath.Div(100, 0);
  // result is None -- no crash, no undefined behavior

  var sum: Optional(i64) = SafeMath.Add(9223372036854775800, 100);
  // sum is None -- overflow detected by Idris 2

  proven_deinit();
  return 0;
}
```

## Error Handling

All fallible operations return `Optional(T)`. `None` indicates the operation
failed (overflow, invalid input, etc.). The underlying status code from
libproven is available through the raw `IntResult`, `BoolResult`,
`StringResult`, and `FloatResult` structures defined in `lib_proven.carbon`.

## Memory Management

String results from libproven (`StringResult`) contain a pointer that must be
freed using `proven_free_string()` (or `SafeString.FreeString()`) when no
longer needed.

## Building

Link against `libproven.so` (or `libproven.a` for static linking):

```bash
carbon build --link=proven src/proven/*.carbon
```

## Requirements

- Carbon toolchain (experimental)
- libproven shared library (`libproven.so`) or static library (`libproven.a`)
- libproven C headers (included in `bindings/c/include/`)

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
