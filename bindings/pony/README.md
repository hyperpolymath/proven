<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - Pony Binding

FFI binding to **libproven**, a formally verified safety library implemented
in Idris 2 with a Zig FFI bridge exposing a stable C ABI.

**All computation is performed in the Idris 2 core.  This binding is a thin
wrapper; it does NOT reimplement any logic.**

Pony is an actor-model, capabilities-secure language with direct C FFI
support via the `@` calling convention.

## Requirements

- Pony compiler (ponyc) 0.55+
- `libproven.so` (Linux) or `libproven.dylib` (macOS) on the library path
- corral (Pony package manager) for dependency management

## Quick Start

```pony
use "proven"

actor Main
  new create(env: Env) =>
    // Initialize the runtime.
    Proven.init()

    // Safe arithmetic.
    match SafeMath.add(2, 3)
    | let v: I64 => env.out.print("Sum: " + v.string())
    | None => env.out.print("Overflow!")
    end

    match SafeMath.div(10, 0)
    | let v: I64 => env.out.print("Quotient: " + v.string())
    | None => env.out.print("Division by zero detected safely.")
    end

    // String safety.
    match SafeString.escape_html("<script>alert(1)</script>")
    | let s: String => env.out.print("Escaped: " + s)
    | None => env.out.print("Error")
    end

    // Email validation.
    match SafeEmail.is_valid("user@example.com")
    | let v: Bool => env.out.print("Valid email: " + v.string())
    | None => env.out.print("Error")
    end

    // Path traversal detection.
    match SafePath.has_traversal("../../etc/passwd")
    | let v: Bool =>
      if v then env.out.print("Traversal detected!") end
    | None => env.out.print("Error")
    end

    // JSON validation.
    match SafeJson.is_valid("{\"key\": 42}")
    | let v: Bool => env.out.print("Valid JSON: " + v.string())
    | None => env.out.print("Error")
    end

    // URL encoding.
    match SafeUrl.encode("hello world")
    | let s: String => env.out.print("Encoded: " + s)
    | None => env.out.print("Error")
    end

    // Cryptographic random bytes.
    match SafeCrypto.random_bytes(16)
    | let buf: Array[U8] iso =>
      env.out.print("Generated 16 random bytes")
    | None => env.out.print("Error")
    end

    // Clean up.
    Proven.deinit()
```

## Building

```bash
# Using corral.
corral fetch
corral run -- ponyc -p /path/to/libproven .

# Or directly with ponyc.
ponyc -p /path/to/libproven .
```

## Error Handling

All fallible operations return Pony union types `(T | None)`.  Use
`match` expressions to handle both cases:

```pony
match SafeMath.div(10, 0)
| let v: I64 => env.out.print("Result: " + v.string())
| None => env.out.print("Division by zero handled safely.")
end
```

This is idiomatic Pony and aligns with the language's capabilities-secure
philosophy of making failure explicit.

## Modules

| Primitive | Description |
|-----------|-------------|
| `Proven` | Lifecycle management (init/deinit) |
| `SafeMath` | Overflow-safe integer arithmetic |
| `SafeString` | XSS/SQL/JS escaping, UTF-8 validation |
| `SafePath` | Directory traversal prevention |
| `SafeEmail` | RFC 5321 email validation |
| `SafeUrl` | RFC 3986 URL encoding/decoding |
| `SafeCrypto` | Constant-time comparison, random bytes, hex encoding |
| `SafeJson` | JSON validation and type detection |

## Architecture

```
Pony actor  -->  @proven_*[T](args)  -->  libproven.so
                                              |
                                         Zig FFI bridge
                                              |
                                         Idris 2 core
                                     (formally verified)
```

## License

PMPL-1.0-or-later
