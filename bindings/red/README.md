# Proven Red Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Red language bindings for **libproven**, a formally verified safety library.
All computation is performed in Idris 2 via the Zig FFI bridge. These bindings
are thin wrappers that call libproven through Red's `#import` directive -- they
do NOT reimplement any logic.

## Architecture

```
Red code  -->  #import libproven.so (cdecl)  -->  Zig FFI bridge  -->  Idris 2 (verified)
```

## Modules

| Module | File | Description |
|--------|------|-------------|
| `proven` | `proven.red` | C import declarations, status codes, helpers |
| `safe-math` | `safe-math.red` | Overflow-safe arithmetic |
| `safe-string` | `safe-string.red` | UTF-8 validation, SQL/HTML/JS escaping |
| `safe-path` | `safe-path.red` | Directory traversal prevention |
| `safe-email` | `safe-email.red` | RFC 5321 email validation |
| `safe-url` | `safe-url.red` | RFC 3986 URL parsing |
| `safe-crypto` | `safe-crypto.red` | Constant-time comparison, CSPRNG |
| `safe-json` | `safe-json.red` | JSON validation and type detection |

## Usage

```red
Red [Needs: 'proven]

#include %proven.red
#include %safe-math.red
#include %safe-email.red

proven_init

; Safe division -- returns none on division by zero
result: safe-math/div 100 0
; result is none -- no crash

; Email validation
valid?: safe-email/is-valid? "user@example.com"
; valid? is true

; Safe addition with overflow detection
sum: safe-math/add 9223372036854775800 100
; sum is none -- overflow detected by Idris 2

proven_deinit
```

## Error Handling

All fallible operations return `none` on failure. Use Red's `either` or
`if` to handle the result:

```red
result: safe-math/div 10 3
either result [
    print ["Result:" result]
][
    print "Division failed"
]
```

For detailed error information, use the raw FFI functions from `proven.red`
directly and check the `status` field against the `STATUS_*` constants.

## Memory Management

String results from libproven contain pointers that must be freed using
`proven_free_string`. The safe wrapper modules handle this automatically
by copying the string data before freeing.

## Building

Ensure `libproven.so` (Linux) or `libproven.dylib` (macOS) is in the
library search path:

```bash
export LD_LIBRARY_PATH=/path/to/libproven:$LD_LIBRARY_PATH
red your-script.red
```

## Requirements

- Red toolchain (0.6.5+)
- libproven shared library (`libproven.so` or `libproven.dylib`)

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
