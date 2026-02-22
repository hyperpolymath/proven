<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven -- Koka Bindings

Koka bindings for the **proven** formally verified safety library.

## Architecture

Koka has native C interop via `extern` declarations with `c inline`. These
bindings declare extern functions that call directly into libproven's C ABI,
which is implemented by the Zig FFI bridge over the Idris 2 verified core.

```
Koka code
  -> extern c inline (C FFI)
    -> libproven C ABI
      -> Zig FFI bridge
        -> Idris 2 verified implementation
```

**All computation is performed in Idris 2. No logic is reimplemented in Koka.**

## Modules

| Module | Functions | Description |
|--------|-----------|-------------|
| `proven/lib-proven` | Core | Lifecycle, status codes, result types |
| `proven/safe-math` | 8 | Checked arithmetic (add, sub, mul, div, mod, abs, clamp, pow) |
| `proven/safe-string` | 4 | UTF-8 validation, SQL/HTML/JS escaping |
| `proven/safe-path` | 3 | Directory traversal detection, filename sanitization |
| `proven/safe-email` | 2 | RFC 5321 email validation |
| `proven/safe-url` | 5 | URL parsing into components |
| `proven/safe-crypto` | 3 | Constant-time comparison, hex encoding |
| `proven/safe-json` | 4 | JSON validation and type detection |
| `proven/safe-datetime` | 3 | ISO 8601 parsing, leap year, days-in-month |

## Usage

```koka
import proven/lib-proven
import proven/safe-math
import proven/safe-email

fun main(): <ndet,console> ()
  // Initialize the runtime
  val status = init()
  match status
    Ok -> println("Proven initialized")
    _  -> println("Failed to initialize")

  // Safe math
  match safe-add(9223372036854775807.int64, 1.int64)
    Just(v) -> println("Sum: " ++ v.show)
    Nothing -> println("Overflow detected!")

  // Email validation
  match is-valid-email("user@example.com")
    Just(True)  -> println("Valid email")
    Just(False) -> println("Invalid email")
    Nothing     -> println("Validation error")

  // Cleanup
  deinit()
```

## Building

```bash
# Ensure libproven is built and available
cd ../../ffi/zig && zig build

# Build the Koka package
koka --cc-opts="-I../../bindings/c/include" \
     --cc-link="-L../../ffi/zig/zig-out/lib -lproven" \
     proven/lib-proven.kk
```

## Error Handling

All fallible operations return `maybe<t>`:

- `Just(value)` on success
- `Nothing` on any error (overflow, validation failure, etc.)

For operations that need detailed error information, use the `status-from-int32`
function to inspect the specific error code.

## Effect System

Functions that call into the C FFI use the `<ndet>` effect to indicate
non-deterministic behavior (C function calls). This integrates with Koka's
algebraic effect system for safe composition.

## License

PMPL-1.0-or-later
