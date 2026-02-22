<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-affinescript

AffineScript bindings for the **Proven** formally verified safety library.

## Overview

These bindings provide thin, affine-typed wrappers around the `libproven` C ABI.
All computation is performed in Idris 2 (formally verified with dependent types)
and exposed through the Zig FFI layer. **No logic is reimplemented here** --
these files only marshal data across the FFI boundary and enforce AffineScript's
at-most-once consumption guarantee on every result.

## Architecture

```
AffineScript (.afs)  -->  Zig FFI (libproven.so)  -->  Idris 2 (verified)
   affine types            C ABI bridge                 dependent types
```

AffineScript compiles via OCaml and targets Zig, so the FFI uses `@extern("C", ...)`
declarations that map directly to the C ABI symbols exported by libproven.

## Modules

| Module | File | Description |
|--------|------|-------------|
| `Proven` | `src/proven.afs` | Main entry point, lifecycle, error types |
| `Proven.FFI` | `src/ffi.afs` | Raw extern "C" declarations |
| `Proven.SafeMath` | `src/safe_math.afs` | Arithmetic without overflow/underflow |
| `Proven.SafeString` | `src/safe_string.afs` | UTF-8 validation and escaping |
| `Proven.SafePath` | `src/safe_path.afs` | Path traversal prevention |
| `Proven.SafeEmail` | `src/safe_email.afs` | Email validation |
| `Proven.SafeUrl` | `src/safe_url.afs` | URL parsing |
| `Proven.SafeCrypto` | `src/safe_crypto.afs` | Cryptographic primitives |
| `Proven.SafeJson` | `src/safe_json.afs` | JSON validation |

## Usage

```affinescript
import Proven
import Proven.SafeMath
import Proven.SafeEmail

fn main() -> () / IO {
  // Initialise the runtime
  match Proven.init() {
    Err(e) => println("Init failed: " ++ e.message),
    Ok(()) => {
      // Safe arithmetic -- overflow returns Err, not a crash
      match safe_add(9223372036854775800, 100) {
        Err(e) => println("Overflow: " ++ e.message),
        Ok(sum) => println("Sum: " ++ intToString(sum))
      }

      // Email validation -- no regex backtracking
      match is_valid(ref "user@example.com") {
        Err(e) => println("Error: " ++ e.message),
        Ok(valid) => println("Valid: " ++ boolToString(valid))
      }

      Proven.deinit()
    }
  }
}
```

## Affine Type Semantics

Every FFI result is wrapped in an affine type (`own`), meaning it can be
consumed **at most once**. This prevents:

- Double-free of FFI-allocated strings
- Use-after-free on marshalled pointers
- Aliasing of mutable FFI state

```affinescript
// Each result must be consumed exactly once:
let result = safe_add(1, 2);  // result is affine
match result {                  // consumed here -- cannot use result again
  Ok(n) => println(intToString(n)),
  Err(e) => println(e.message)
}
// result is no longer available -- compiler enforces this
```

## Building

Requires `libproven.so` (or `.dylib` / `.dll`) to be available at link time.

```bash
# Build the AffineScript compiler first
cd /path/to/affinescript && dune build

# Compile the bindings (once the AffineScript compiler supports codegen)
affinescript build src/proven.afs --link-lib=proven
```

## License

PMPL-1.0-or-later
