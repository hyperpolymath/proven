<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven -- Flix Bindings

Flix bindings for the **proven** formally verified safety library.

## Architecture

Flix runs on the JVM, so these bindings use JNA (Java Native Access) to call
into the libproven shared library. JNA loads `libproven.so` at runtime and
invokes exported C ABI functions via dynamic function lookup.

```
Flix code
  -> JNA (com.sun.jna.Function)
    -> libproven.so (C ABI)
      -> Zig FFI bridge
        -> Idris 2 verified implementation
```

**All computation is performed in Idris 2. No logic is reimplemented in Flix.**

## Modules

| Module | Functions | Description |
|--------|-----------|-------------|
| `Proven.LibProven` | Core | Lifecycle, status codes, JNA helpers |
| `Proven.SafeMath` | 8 | Checked arithmetic (add, sub, mul, div, mod, abs, clamp, pow) |
| `Proven.SafeString` | 4 | UTF-8 validation, SQL/HTML/JS escaping |
| `Proven.SafePath` | 3 | Directory traversal detection, filename sanitization |
| `Proven.SafeEmail` | 2 | RFC 5321 email validation |
| `Proven.SafeUrl` | 4 | URL parsing into components |
| `Proven.SafeCrypto` | 3 | Constant-time comparison, hex encoding |
| `Proven.SafeJson` | 4 | JSON validation and type detection |

## Usage

```flix
use Proven.LibProven
use Proven.SafeMath
use Proven.SafeEmail
use Proven.SafeJson

def main(): Unit \ IO =
    // Initialize the runtime
    let status = LibProven.init();
    match status {
        case ProvenStatus.Ok => println("Proven initialized")
        case _               => println("Failed to initialize")
    };

    // Safe math
    match SafeMath.add(9223372036854775807i64, 1i64) {
        case Some(v) => println("Sum: ${v}")
        case None    => println("Overflow detected!")
    };

    // Email validation
    match SafeEmail.isValid("user@example.com") {
        case Some(true)  => println("Valid email")
        case Some(false) => println("Invalid email")
        case None        => println("Validation error")
    };

    // JSON type detection
    let jt = SafeJson.getType("{\"key\": 42}");
    println("JSON type: ${jt}");

    // Cleanup
    LibProven.deinit()
```

## Building

```bash
# Ensure libproven is built
cd ../../ffi/zig && zig build

# Run with Flix, providing JNA and library path
flix run --jar jna-5.14.0.jar \
         -Djna.library.path=../../ffi/zig/zig-out/lib
```

## Dependencies

- **JNA 5.14.0** -- Java Native Access for calling libproven's C ABI
  - Declared in `flix.toml` under `[mvn-dependencies]`
  - Automatically resolved by Flix's Maven integration

## Error Handling

All fallible operations return `Option[T]`:

- `Some(value)` on success
- `None` on any error (overflow, validation failure, etc.)

For detailed error information, the `Proven.LibProven` module exposes
`ProvenStatus` for mapping raw C status codes.

## Effect System

Functions that call into the native library use the `IO` effect, integrating
with Flix's algebraic effect system. This ensures that FFI calls are tracked
in the type signature.

## License

PMPL-1.0-or-later
