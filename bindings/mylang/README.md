# Proven - MyLang Bindings

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

MyLang bindings for **libproven**, the Idris 2 formally verified safety library.

MyLang is a personal/customizable programming language with Rust-like FFI syntax
using `extern "C" fn` declarations. Its strong type system with `Result<T, E>`
and `Option<T>` types makes it well-suited for wrapping libproven's safety-critical
operations.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in MyLang.** These bindings are thin FFI wrappers only.

## Architecture

```
MyLang (.my)
  |  extern "C" fn ...
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

## Error Handling

MyLang uses a Rust-like `Result<T, E>` type for error handling:

```mylang
type ProvenError = enum {
  Overflow, Underflow, DivByZero,
  NullPointer, InvalidArgument,
  ParseFailure, ValidationFailed,
  OutOfBounds, EncodingError,
  AllocationFailed, NotImplemented,
  UnknownError(i32),
}
```

All operations return `Result<T, ProvenError>`:

```mylang
match safe_add(a, b) {
  Result::Ok(result) => use_value(result),
  Result::Err(ProvenError::Overflow) => handle_overflow(),
  Result::Err(e) => handle_other(e),
}
```

## Usage

```mylang
import Proven

fn main() -> Result<(), ProvenError> =
  init()?
  let sum = safe_add(100i64, 200i64)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Result::Ok(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- MyLang compiler with C FFI support

## License

PMPL-1.0-or-later
