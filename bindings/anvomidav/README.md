# Proven - Anvomidav Bindings

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Anvomidav bindings for **libproven**, the Idris 2 formally verified safety library.

Anvomidav is a language from the nextgen-languages project. It uses `@link("C", ...)`
for FFI binding, `proc` for function declarations, `define type` for type definitions,
and `result<T, E>` / `maybe<T>` for error handling.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in Anvomidav.** These bindings are thin FFI wrappers only.

## Architecture

```
Anvomidav (.anv)
  |  @link("C", ...)
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

Anvomidav uses `result<T, E>` with `ok(T)` and `err(E)` variants:

```anvomidav
define type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `result<T, ProvenError>`:

```anvomidav
match safe_add(a, b)
| ok(value) => use_value(value)
| err(Overflow) => handle_overflow()
| err(e) => handle_other(e)
end
```

## Usage

```anvomidav
import Proven

proc main() -> result<Unit, ProvenError> =
  init()?
  let sum = safe_add(100, 200)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  ok(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- Anvomidav compiler with C FFI support

## License

PMPL-1.0-or-later
