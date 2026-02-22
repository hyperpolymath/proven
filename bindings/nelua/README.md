# Proven -- Nelua Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Nelua FFI bindings for **libproven**, a formally verified safety library.
All computation is performed in the Idris 2 / Zig core; these bindings are
a thin declaration layer that marshals data across the C ABI boundary.

**No logic is reimplemented.** Every function calls `libproven` via Nelua's
native C interop.

## Prerequisites

- [Nelua](https://nelua.io/) compiler
- `libproven.so` (or `libproven.a`) built from the proven repository
- The `proven.h` C header on the include path

## Usage

```nelua
require 'proven.safe_math'
require 'proven.safe_string'

-- Initialize the runtime
proven_init()

-- Safe arithmetic (returns nil on overflow)
local result = safe_add(9223372036854775800_i64, 100_i64)
if result then
  print(result)
end

-- Safe string escaping (returns nil on error)
local escaped = escape_html("<script>alert('xss')</script>")
if escaped then
  print(escaped)
end

-- Cleanup
proven_deinit()
```

## Modules

| Module | Description |
|--------|-------------|
| `proven.lib_proven` | Raw C FFI declarations (status codes, types, functions) |
| `proven.safe_math` | Checked arithmetic (add, sub, mul, div, mod, abs, pow) |
| `proven.safe_string` | String escaping (SQL, HTML, JavaScript) and UTF-8 validation |
| `proven.safe_path` | Path traversal detection and filename sanitization |
| `proven.safe_email` | Email address validation (RFC 5321) |
| `proven.safe_url` | URL parsing into components |
| `proven.safe_crypto` | Constant-time comparison, random bytes, hex encode/decode |
| `proven.safe_json` | JSON validation and type detection |

## Error Handling

High-level wrappers return Nelua's `facultative(T)` (nilable) types:

- `nil` indicates an error (overflow, invalid input, etc.)
- A non-nil value indicates success

For finer-grained error information, use the raw FFI functions from
`proven.lib_proven` directly and inspect the `status` field.

## Building

```bash
nelua --cc=gcc -L/path/to/libproven your_program.nelua
```

Or ensure `libproven.so` is on `LD_LIBRARY_PATH`.

## Architecture

```
Nelua wrapper  -->  C ABI (libproven.so)  -->  Zig FFI bridge  -->  Idris 2 core
     ^                                                                    ^
  this package                                                   formally verified
```
