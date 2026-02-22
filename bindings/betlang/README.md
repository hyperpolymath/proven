<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - BetLang Bindings

## Overview

BetLang bindings for **libproven**, the Idris 2 formally verified safety library.

BetLang is a probabilistic programming language with betting/wagering metaphors.
It uses `@wager("C", ...)` for FFI declarations and `odds<T, E>` as its core
result type (Win/Lose), making it naturally suited for wrapping libproven's
safety-critical operations as probabilistic wagers.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in BetLang.** These bindings are thin FFI wrappers only.

## Architecture

```
BetLang (.bet)
  |  @wager("C", ...)
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

BetLang's `odds<T, E>` type provides exhaustive matching with wagering semantics:

```betlang
stake type ProvenError =
  | Overflow | Underflow | DivByZero
  | NullPointer | InvalidArgument
  | ParseFailure | ValidationFailed
  | OutOfBounds | EncodingError
  | AllocationFailed | NotImplemented
  | UnknownError(Int32)
```

All operations return `odds<T, ProvenError>`:

```betlang
match safe_add(a, b) with
| Win(result)     => handle_payout(result)
| Lose(Overflow)  => handle_bust()
| Lose(e)         => handle_other(e)
```

## Usage

```betlang
import Proven

bet fn main() -> odds<Unit, ProvenError> =
  init()?
  let sum = safe_add(100i64, 200i64)?
  let email_ok = is_valid_email("user@example.com")?
  deinit()
  Win(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- BetLang compiler with C FFI support

## License

PMPL-1.0-or-later
