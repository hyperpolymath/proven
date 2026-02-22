# Proven -- Ring Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Ring language FFI bindings for **libproven**, a formally verified safety library.
All computation is performed in the Idris 2 / Zig core; these bindings use
Ring's `LoadLib()` / `CallDLL()` interface to call the C ABI directly.

**No logic is reimplemented.** Every function calls `libproven` via DLL FFI.

## Prerequisites

- [Ring](https://ring-lang.net/) programming language
- `libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows)

## Usage

```ring
load "proven/safe_math.ring"
load "proven/safe_string.ring"

# Initialize the runtime
proven_init()

# Safe arithmetic (returns NULL on overflow)
result = safe_add(9223372036854775800, 100)
if result != NULL
    ? result
else
    ? "Overflow detected"
ok

# Email validation
load "proven/safe_email.ring"
? is_valid_email("user@example.com")   # 1 (true)
? is_valid_email("not-an-email")       # 0 (false)

# JSON validation
load "proven/safe_json.ring"
? json_is_valid('{"key": "value"}')    # 1 (true)

# Cleanup
proven_deinit()
proven_unload_library()
```

## Modules

| Module | Description |
|--------|-------------|
| `proven/lib_proven.ring` | Library loading, status codes, lifecycle functions |
| `proven/safe_math.ring` | Checked arithmetic (add, sub, mul, div, mod, abs, pow) |
| `proven/safe_string.ring` | String escaping (SQL, HTML, JavaScript) and UTF-8 validation |
| `proven/safe_path.ring` | Path traversal detection and filename sanitization |
| `proven/safe_email.ring` | Email address validation (RFC 5321) |
| `proven/safe_url.ring` | URL parsing and IPv4 validation |
| `proven/safe_crypto.ring` | Constant-time comparison, random bytes, hex encode/decode |
| `proven/safe_json.ring` | JSON validation and type detection |

## Error Handling

Functions return `NULL` on error:

- Math functions return `NULL` on overflow, underflow, or division by zero
- String functions return `NULL` on encoding or memory errors
- Boolean functions return `0` (false) on failure

## Architecture

```
Ring (LoadLib/CallDLL)  -->  C ABI (libproven.so)  -->  Zig FFI  -->  Idris 2 core
        ^                                                                   ^
   this package                                                    formally verified
```
