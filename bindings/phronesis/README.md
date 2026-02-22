# Proven - Phronesis Bindings

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Phronesis bindings for **libproven**, the Idris 2 formally verified safety library.

Phronesis (Greek: practical wisdom/prudence) is a language whose design draws from
Greek philosophical concepts. It uses `@sophia("C", ...)` for FFI (sophia = wisdom),
`gnosis fn` for function declarations (gnosis = knowledge), `logos type` for type
definitions (logos = word/reason), and `krisis<T, E>` for error handling
(krisis = judgment/decision).

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in Phronesis.** These bindings are thin FFI wrappers only.

## Architecture

```
Phronesis (.phr)
  |  @sophia("C", ...)
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

Phronesis uses `krisis<T, E>` (judgment) with `Aletheia(T)` (truth/success)
and `Hamartia(E)` (missing the mark/error):

```phronesis
logos type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `krisis<T, ProvenError>`:

```phronesis
match safe_add(a, b) with
| Aletheia(result) => use_value(result)
| Hamartia(Overflow) => handle_overflow()
| Hamartia(e) => handle_other(e)
```

## Usage

```phronesis
import Proven

gnosis fn main() -> krisis<Unit, ProvenError> =
  init()?
  let sum = safe_add(100, 200)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Aletheia(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- Phronesis compiler with C FFI support

## License

PMPL-1.0-or-later
