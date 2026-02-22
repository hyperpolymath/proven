# Proven - Error-Lang Bindings

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Error-Lang bindings for **libproven**, the Idris 2 formally verified safety library.

Error-Lang is a formally-verified language specializing in error handling with dependent
types. It uses a ReScript compiler with Zig FFI and Idris2 ABI, making it naturally
suited for wrapping libproven's safety-critical operations.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in Error-Lang.** These bindings are thin FFI wrappers only.

## Architecture

```
Error-Lang (.err)
  |  @foreign("C", ...)
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

Error-Lang's dependent error types provide exhaustive matching:

```error-lang
error_type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `Proven<T, ProvenError>`:

```error-lang
match safe_add(a, b) with
| Ok(result) => use_value(result)
| Err(Overflow) => handle_overflow()
| Err(e) => handle_other(e)
```

## Usage

```error-lang
import Proven

fn main() -> Proven<Unit, ProvenError> =
  init()?
  let sum = safe_add(100i64, 200i64)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Ok(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- Error-Lang compiler with C FFI support

## License

PMPL-1.0-or-later
