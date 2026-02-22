<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven Ephapax-Affine Bindings

Ephapax-Affine bindings for the proven library. Ephapax is a linear types
language; the "affine" variant uses affine types where values can be consumed
at most once. This binding uses Rust's ownership system to enforce affine
semantics at compile time.

All computation is performed in formally verified Idris 2 code through the
Zig FFI bridge. This crate contains only FFI marshaling and affine type
wrappers. No algorithms are reimplemented.

## Architecture

```
Ephapax-Affine (Rust, affine types via ownership)
  | extern "C" FFI calls
  v
libproven C ABI (proven.h)
  | C ABI
  v
Zig FFI bridge
  | Zig -> Idris2 RefC calls
  v
Verified Idris 2 core (dependent types, totality checking)
```

## Files

| File                  | Purpose                                     |
|-----------------------|---------------------------------------------|
| `src/ffi.rs`          | Raw FFI declarations (C ABI)                |
| `src/lib.rs`          | Core types, error handling, lifecycle       |
| `src/safe_math.rs`    | Checked integer arithmetic (`AffineInt`)    |
| `src/safe_string.rs`  | String escaping/sanitization (`AffineString`) |
| `src/safe_email.rs`   | Email validation (`ValidatedEmail`)         |
| `src/safe_url.rs`     | URL/path/IPv4 validation (`ValidatedUrl`)   |
| `src/safe_crypto.rs`  | Crypto: random, hash, hex (`AffineBytes`)   |
| `src/safe_json.rs`    | JSON validation (`ValidatedJson`)           |

## Affine Type Wrappers

Each module provides affine wrapper types that enforce single-use semantics:

| Type              | Module         | Contains        | Consumed via     |
|-------------------|----------------|-----------------|------------------|
| `AffineInt`       | `safe_math`    | `i64`           | `.consume()`     |
| `AffineString`    | `safe_string`  | `String`        | `.consume()`     |
| `AffineBytes`     | `safe_crypto`  | `Vec<u8>`       | `.consume()`     |
| `ValidatedEmail`  | `safe_email`   | `String`        | `.consume()`     |
| `ValidatedUrl`    | `safe_url`     | `String`        | `.consume()`     |
| `ValidatedJson`   | `safe_json`    | `String`        | `.consume()`     |

Once `.consume()` is called, the wrapper is moved and cannot be reused (Rust
ownership enforces this at compile time).

## Usage

```rust
use ephapax_proven_affine::{init, safe_math, safe_email, safe_json};

fn main() {
    // Initialize the proven runtime.
    init().expect("proven init failed");

    // Checked addition -- result is affine (consumed once).
    let sum = safe_math::safe_add(100, 200).expect("overflow");
    let value = sum.consume(); // value = 300, sum is now consumed

    // Email validation -- validated email is affine.
    let email = safe_email::validate_email("alice@example.com")
        .expect("invalid email");
    let addr = email.consume(); // addr = "alice@example.com", email is consumed

    // JSON validation -- validated JSON is affine.
    let doc = safe_json::validate_json("{\"key\": 42}")
        .expect("invalid JSON");
    assert_eq!(doc.json_type(), safe_json::JsonType::Object);
    let raw = doc.consume(); // raw = "{\"key\": 42}", doc is consumed
}
```

## Build

```bash
cargo build
```

Requires `libproven.so` to be available at link time. Set `LD_LIBRARY_PATH`
or use `RUSTFLAGS="-L /path/to/libproven"`.

## License

SPDX-License-Identifier: PMPL-1.0-or-later
