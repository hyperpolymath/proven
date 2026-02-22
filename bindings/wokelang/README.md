<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - WokeLang Bindings

## Overview

WokeLang bindings for **libproven**, the Idris 2 formally verified safety library.

WokeLang is a social-awareness themed programming language where functions "enlighten"
and errors represent "ignorance". It uses `@import_truth("C", ...)` for FFI declarations
and `awareness<T, E>` as its core result type (Enlightened/Ignorant), making error
handling a journey toward computational enlightenment.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in WokeLang.** These bindings are thin FFI wrappers only.

## Architecture

```
WokeLang (.woke)
  |  @import_truth("C", ...)
  v
libproven.so (Zig FFI layer)
  |  Zig -> Idris2 RefC calls
  v
Idris 2 (dependent types, totality checking)
```

## Modules

| Module | Description |
|--------|-------------|
| `Proven` | Core types, error types, lifecycle, re-exports |
| `Proven.FFI` | Raw FFI declarations to libproven C ABI |
| `Proven.SafeMath` | Overflow-checked integer arithmetic |
| `Proven.SafeString` | String escaping, UTF-8 validation, path safety |
| `Proven.SafeEmail` | RFC 5321 email validation |
| `Proven.SafeUrl` | URL parsing and validation |
| `Proven.SafeCrypto` | Constant-time comparison, random bytes, hex, CRC32 |
| `Proven.SafeJson` | JSON validation, type detection, expression eval |

## Error Types

WokeLang's `awareness<T, E>` type provides exhaustive matching with awareness semantics:

```wokelang
conscious type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `awareness<T, ProvenError>`:

```wokelang
match safe_add(a, b) with
| Enlightened(result)  => celebrate_truth(result)
| Ignorant(Overflow)   => confront_excess()
| Ignorant(e)          => confront_other(e)
```

## Usage

```wokelang
import Proven

enlighten fn main() -> awareness<Unit, ProvenError> =
  init()?
  let sum = safe_add(100i64, 200i64)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Enlightened(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- WokeLang compiler with C FFI support

## License

PMPL-1.0-or-later
