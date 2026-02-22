# proven-ffi: Self-hosted Idris 2 FFI binding for libproven

SPDX-License-Identifier: PMPL-1.0-or-later

Self-hosted Idris 2 binding that calls the precompiled `libproven` shared
library via `%foreign "C"` FFI declarations. This allows users to consume
proven as a shared library without needing the Idris 2 source tree.

## Architecture

```
Your Idris 2 code
       |
       v
  Proven.SafeMath / SafeString / SafePath / ...   (this package)
       |
       v
  Proven.FFI  (%foreign "C:symbol,libproven")
       |
       v
  libproven.so / libproven.a  (precompiled)
       |
       v
  Zig FFI bridge -> Idris 2 RefC verified core
```

All computation happens in the formally verified Idris 2 core. This
binding is a thin wrapper that marshals data across the C ABI boundary.

## Prerequisites

- Idris 2 (with RefC codegen)
- `libproven.so` (or `libproven.a`) installed in the library search path
- `proven.h` available for reference (not needed at compile time)

## Building

```sh
idris2 --build proven-ffi.ipkg
```

## Usage

```idris
import Proven

main : IO ()
main = do
  Right () <- provenInit
    | Left err => putStrLn ("Init failed: " ++ show err)

  -- Safe arithmetic (cannot overflow silently)
  Just sum <- safeAdd 9223372036854775800 10
    | Nothing => putStrLn "Addition overflowed"
  printLn sum

  -- Email validation
  Just valid <- isValidEmail "user@example.com"
    | Nothing => putStrLn "Validation error"
  putStrLn $ "Email valid: " ++ show valid

  -- String escaping (prevents XSS)
  Just escaped <- escapeHtml "<script>alert('xss')</script>"
    | Nothing => putStrLn "Escape error"
  putStrLn $ "Escaped: " ++ escaped

  -- Calendar operations (pure, no IO needed)
  putStrLn $ "2024 is leap year: " ++ show (isLeapYear 2024)
  putStrLn $ "Days in Feb 2024: " ++ show (daysInMonth 2024 2)

  provenDeinit
```

## Modules

| Module              | Description                                  |
|---------------------|----------------------------------------------|
| `Proven`            | Main re-export module, lifecycle management   |
| `Proven.FFI`        | Raw `%foreign` declarations and status codes  |
| `Proven.SafeMath`   | Overflow-safe integer arithmetic              |
| `Proven.SafeString` | UTF-8 validation, SQL/HTML/JS escaping        |
| `Proven.SafePath`   | Directory traversal detection, sanitisation   |
| `Proven.SafeEmail`  | RFC 5321 email validation                     |
| `Proven.SafeUrl`    | URL parsing and validation                    |
| `Proven.SafeNetwork`| IPv4 address parsing and classification       |
| `Proven.SafeCrypto` | Constant-time comparison, secure RNG, hex     |
| `Proven.SafeJson`   | JSON validation and type detection            |
| `Proven.SafeDateTime`| ISO 8601 parsing, calendar utilities          |

## Error handling

Functions that can fail return `Maybe a` (simple) or
`Either ProvenError a` (typed errors). The `ProvenError` type maps
directly to the C status codes from libproven.

Functions suffixed with `E` (e.g. `safeAddE`, `isValidEmailE`) return
`Either ProvenError a` for pattern matching on specific error conditions.

## Memory management

String results from libproven are automatically copied into managed
Idris Strings and the C buffers are freed. Users do not need to call
any free functions manually.

## Limitations

Some C functions return complex structs (URL components, IPv4 addresses,
DateTime). Full struct field extraction requires either Idris 2 Struct FFI
support or thin C accessor shims. The current version provides the raw
pointer interface for these cases.

## Licence

PMPL-1.0-or-later. Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath).
