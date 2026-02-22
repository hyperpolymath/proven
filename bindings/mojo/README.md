<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven Mojo Bindings

Mojo FFI bindings for **libproven**, a formally verified safety library
implemented in Idris 2 with a Zig FFI bridge exposing a stable C ABI.

All computation is performed in the verified Idris 2 core. These bindings
are thin wrappers that marshal data via `external_call` (Mojo's native C
interop). No logic is reimplemented in Mojo.

## Architecture

```
Mojo code (this package)
    |  external_call (C FFI)
    v
libproven.so / libproven.a (Zig-compiled)
    |
    v
Idris 2 verified core (all computation)
```

## Modules

| Module | Description |
|--------|-------------|
| `lib_proven` | Raw C FFI declarations, result types, status codes |
| `safe_math` | Overflow-safe integer and floating-point arithmetic |
| `safe_string` | Encoding-safe text (SQL, HTML, JS escaping; hex encoding) |
| `safe_path` | Filesystem path traversal prevention |
| `safe_email` | RFC 5321 email address validation |
| `safe_url` | URL parsing, percent-encoding/decoding |
| `safe_network` | IPv4 address parsing and classification |
| `safe_crypto` | Timing-safe comparison, secure random bytes, CRC32 |
| `safe_json` | JSON validation and type detection |
| `safe_datetime` | ISO 8601 date/time parsing and formatting |

## Prerequisites

Build libproven from the repository root:

```bash
cd ffi/zig
zig build -Doptimize=ReleaseSafe
```

Ensure `libproven.so` (or `libproven.a`) is on the library search path.

## Usage

```mojo
from proven.src.lib_proven import proven_init, proven_deinit
from proven.src.safe_math import safe_add, safe_div
from proven.src.safe_string import escape_html
from proven.src.safe_email import is_valid_email

fn main():
    # Initialize the Proven runtime (required once)
    _ = proven_init()

    # Safe arithmetic -- returns None on overflow instead of crashing
    var sum = safe_add(Int64(9223372036854775800), Int64(100))
    if not sum:
        print("overflow detected")

    # Safe division -- returns None on divide-by-zero
    var quotient = safe_div(Int64(10), Int64(0))
    if not quotient:
        print("division by zero handled safely")

    # XSS prevention
    var escaped = escape_html("<script>alert(1)</script>")
    if escaped:
        print(escaped.value())

    # Email validation
    if is_valid_email("user@example.com"):
        print("valid email")

    # Cleanup
    proven_deinit()
```

## Error Handling

All fallible functions return `Optional[T]`:

- `Optional[Int64]` for integer operations
- `Optional[Float64]` for float operations
- `Optional[String]` for string operations
- `Bool` for validation checks (False on error as safe default)

For callers who need the specific error code, raw `IntResult`/`FloatResult`
access functions are available (e.g. `safe_add_result`, `safe_div_result`).

## Status Codes

| Code | Value | Meaning |
|------|-------|---------|
| `PROVEN_OK` | 0 | Success |
| `PROVEN_ERR_NULL_POINTER` | -1 | Null pointer passed |
| `PROVEN_ERR_INVALID_ARGUMENT` | -2 | Invalid argument |
| `PROVEN_ERR_OVERFLOW` | -3 | Integer overflow |
| `PROVEN_ERR_UNDERFLOW` | -4 | Integer underflow |
| `PROVEN_ERR_DIVISION_BY_ZERO` | -5 | Division by zero |
| `PROVEN_ERR_PARSE_FAILURE` | -6 | Parse error |
| `PROVEN_ERR_VALIDATION_FAILED` | -7 | Validation failed |
| `PROVEN_ERR_OUT_OF_BOUNDS` | -8 | Index out of bounds |
| `PROVEN_ERR_ENCODING_ERROR` | -9 | Encoding error |
| `PROVEN_ERR_ALLOCATION_FAILED` | -10 | Allocation failed |
| `PROVEN_ERR_NOT_IMPLEMENTED` | -99 | Not implemented |

## License

PMPL-1.0-or-later
