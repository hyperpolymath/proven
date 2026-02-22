<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - Tangle Bindings

## Overview

Tangle bindings for **libproven**, the Idris 2 formally verified safety library.

Tangle is a graph-oriented/interconnected programming language where functions are
"woven" together via `@bridge("C", ...)` FFI declarations. It uses `strand<T, E>`
as its core result type (Connected/Severed), representing whether computation flows
through intact or is severed by an error.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in Tangle.** These bindings are thin FFI wrappers only.

## Architecture

```
Tangle (.tgl)
  |  @bridge("C", ...)
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

Tangle's `strand<T, E>` type provides exhaustive matching with graph semantics:

```tangle
node type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `strand<T, ProvenError>`:

```tangle
match safe_add(a, b) with
| Connected(result)  => weave_next(result)
| Severed(Overflow)  => handle_break()
| Severed(e)         => handle_other(e)
```

## Usage

```tangle
import Proven

weave fn main() -> strand<Unit, ProvenError> =
  init()?
  let sum = safe_add(100i64, 200i64)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Connected(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- Tangle compiler with C FFI support

## License

PMPL-1.0-or-later
