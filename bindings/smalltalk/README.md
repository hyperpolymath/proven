<!-- SPDX-License-Identifier: PMPL-1.0-or-later
     Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven Smalltalk (Pharo) FFI Bindings

Pharo Smalltalk bindings for **libproven**, a formally verified safety library
implemented in Idris 2 with a Zig FFI bridge exposing a stable C ABI.

All computation is performed by the verified Idris 2 core. These bindings are
thin FFI wrappers using Pharo's UnifiedFFI (UFFI) framework. **No logic is
reimplemented in Smalltalk.**

## Packages

| Package | Class | Description |
|---------|-------|-------------|
| `Proven-FFI` | `ProvenLibrary` | FFI library class with all function declarations |
| `Proven-FFI` | `ProvenIntResult` | FFI struct for integer results |
| `Proven-FFI` | `ProvenBoolResult` | FFI struct for boolean results |
| `Proven-FFI` | `ProvenStringResult` | FFI struct for string results (auto-frees C memory) |
| `Proven-FFI` | `ProvenFloatResult` | FFI struct for floating-point results |
| `Proven-SafeMath` | `ProvenSafeMath` | Safe arithmetic (add, sub, mul, div, mod, abs, clamp, pow) |
| `Proven-SafeString` | `ProvenSafeString` | String escaping (SQL, HTML, JS) and UTF-8 validation |
| `Proven-SafePath` | `ProvenSafePath` | Path traversal detection and filename sanitization |
| `Proven-SafeEmail` | `ProvenSafeEmail` | Email address validation (RFC 5321) |
| `Proven-SafeUrl` | `ProvenSafeUrl` | URL parsing into components |
| `Proven-SafeNetwork` | `ProvenSafeNetwork` | IPv4 parsing and classification (private/loopback) |
| `Proven-SafeCrypto` | `ProvenSafeCrypto` | Constant-time comparison, random bytes, hex encoding, CRC32 |
| `Proven-SafeJson` | `ProvenSafeJson` | JSON validation and type detection |
| `Proven-SafeDateTime` | `ProvenSafeDateTime` | ISO 8601 parsing, formatting, leap year, days-in-month |

## Prerequisites

- Pharo 12+ (with UnifiedFFI / UFFI support)
- `libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows) on the library search path

## Usage

```smalltalk
"Initialize the runtime (once per session)"
ProvenLibrary uniqueInstance provenInit.

"Safe arithmetic"
ProvenSafeMath add: 2 to: 3.                          "=> 5"
ProvenSafeMath divide: 10 by: 0.                       "=> nil (division by zero)"

"Email validation"
ProvenSafeEmail isValid: 'user@example.com'.           "=> true"
ProvenSafeEmail isValid: 'not-an-email'.               "=> false"

"HTML escaping"
ProvenSafeString escapeHtml: '<script>alert(1)</script>'.
  "=> '&lt;script&gt;alert(1)&lt;/script&gt;'"

"Path safety"
ProvenSafePath hasTraversal: '../../etc/passwd'.       "=> true"
ProvenSafePath sanitizeFilename: '../bad<file>.txt'.   "=> 'badfile.txt'"

"URL parsing"
ProvenSafeUrl parse: 'https://example.com:8080/path?q=1'.
  "=> Dictionary(#scheme -> 'https', #host -> 'example.com', ...)"

"IPv4 classification"
ProvenSafeNetwork isPrivate: '192.168.1.1'.            "=> true"
ProvenSafeNetwork isLoopback: '127.0.0.1'.             "=> true"

"Random bytes"
ProvenSafeCrypto randomBytes: 16.                      "=> ByteArray (16 bytes)"

"JSON validation"
ProvenSafeJson isValid: '{"key": "value"}'.            "=> true"
ProvenSafeJson isObject: '{"key": "value"}'.           "=> true"

"DateTime"
ProvenSafeDateTime isLeapYear: 2024.                   "=> true"
ProvenSafeDateTime daysInMonth: 2 year: 2024.          "=> 29"
ProvenSafeDateTime parse: '2026-02-22T10:30:00Z'.
  "=> Dictionary(#year -> 2026, #month -> 2, #day -> 22, ...)"

"Shut down when done"
ProvenLibrary uniqueInstance provenDeinit.
```

## Error Handling

All wrapper methods return `nil` on error (idiomatic Smalltalk). For detailed
error information, use the low-level FFI result structs directly:

```smalltalk
| result |
result := ProvenLibrary uniqueInstance provenMathDiv: 10 denominator: 0.
result isSuccess.          "=> false"
result status.             "=> -5"
result errorDescription.   "=> 'Division by zero'"
```

## File Format

Files use Tonel format for class definitions, compatible with Pharo's file-out
and Iceberg git integration.

## Architecture

```
Smalltalk code (this binding)
    |
    v  [UFFI / ffiCall:]
libproven.so  (Zig FFI bridge)
    |
    v  [C ABI calls]
Idris 2 RefC  (formally verified core)
```

## License

PMPL-1.0-or-later
