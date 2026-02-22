<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-eclexia

Eclexia bindings for the **Proven** formally verified safety library.

## Overview

These bindings provide thin, resource-typed wrappers around the `libproven` C ABI.
All computation is performed in Idris 2 (formally verified with dependent types)
and exposed through the Zig FFI layer. **No logic is reimplemented here** --
these files only marshal data across the FFI boundary and leverage Eclexia's
economics-as-code paradigm for resource-aware safety.

## Architecture

```
Eclexia (.ecl)  -->  Rust interop  -->  Zig FFI (libproven.so)  -->  Idris 2 (verified)
  resource types     extern "C"         C ABI bridge                  dependent types
```

Eclexia has a Rust-based compiler ecosystem, so the FFI uses `extern "C"` declarations
that map directly to the C ABI symbols exported by libproven. Eclexia's resource
budgets (`@requires: energy < NJ`) track computational cost of FFI operations.

## Modules

| Module | File | Description |
|--------|------|-------------|
| `lib` | `src/lib.ecl` | Main entry point, lifecycle, error types |
| `ffi` | `src/ffi.ecl` | Raw `extern "C"` declarations |
| `safe_math` | `src/safe_math.ecl` | Arithmetic without overflow/underflow |
| `safe_string` | `src/safe_string.ecl` | UTF-8 validation and escaping |
| `safe_currency` | `src/safe_currency.ecl` | Currency operations with resource semantics |
| `safe_crypto` | `src/safe_crypto.ecl` | Cryptographic primitives |
| `safe_json` | `src/safe_json.ecl` | JSON validation |

## Usage

```eclexia
// SPDX-License-Identifier: PMPL-1.0-or-later

fn main() {
    // Initialise the runtime
    match init() {
        Err(e) => println("Init failed:", e.message),
        Ok(()) => {
            // Safe arithmetic -- overflow returns Err, not a crash
            match safe_add(9223372036854775800, 100) {
                Err(e) => println("Overflow:", e.message),
                Ok(sum) => println("Sum:", sum)
            }

            // Currency operations with resource tracking
            let price = SafeAmount {
                minor_units: 1999,
                currency_code: "USD",
                decimal_places: 2
            }
            let qty_price = mul_amount(price, 3)
            match qty_price {
                Ok(total) => println("Total:", format_currency(total)),
                Err(e) => println("Error:", e.message)
            }

            deinit()
        }
    }
}
```

## Resource Budgets

Eclexia's `@requires` annotations declare computational budgets on FFI calls:

```eclexia
// Currency parsing has bounded computational cost
def parse_currency(input: String) -> Result[SafeAmount, CurrencyError]
    @requires: energy < 10J
{
    // ... FFI call ...
}
```

The runtime tracks energy and carbon budgets across all calls, ensuring that
proven operations stay within declared resource envelopes.

## Building

Requires `libproven.so` (or `.dylib` / `.dll`) to be available at link time.

```bash
# Build the Eclexia compiler
cd /path/to/eclexia && cargo build --release

# Build the bindings
eclexia build src/lib.ecl --link-lib=proven
```

## License

PMPL-1.0-or-later
