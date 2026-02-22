# Proven - 7Tentacles Educational Tool

SPDX-License-Identifier: PMPL-1.0-or-later
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

**7Tentacles is an educational tool, not a standalone programming language.** It is
designed to work alongside [MyLang](../../mylang/) as part of the MyLang language
group, providing interactive teaching demonstrations of safety concepts.

These files demonstrate how proven's safety primitives can be expressed using
7Tentacles' octopus-themed pedagogical notation. They are illustrative examples
for learning, not production-ready bindings.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented.** These files are thin FFI wrappers for educational demonstration only.

## Architecture

```
7Tentacles (.7t)
  |  @reach("C", ...)
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

7Tentacles uses `ink<T, E>` with `Catch(T)` and `Squirt(E)` variants:

```7tentacles
sucker type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `ink<T, ProvenError>`:

```7tentacles
match safe_add(a, b) with
| Catch(result) => use_value(result)
| Squirt(Overflow) => handle_overflow()
| Squirt(e) => handle_other(e)
```

## Usage

```7tentacles
import Proven

grasp fn main() -> ink<Unit, ProvenError> =
  init()?
  let sum = safe_add(100, 200)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Catch(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- 7Tentacles compiler with C FFI support

## License

PMPL-1.0-or-later
