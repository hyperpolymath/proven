<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-sml

Standard ML bindings for **libproven**, the formally verified safety library.

All computation is performed in the verified Idris 2 core (via the Zig FFI bridge
and C ABI). This package provides data marshaling only -- **no logic is
reimplemented in SML**.

## Modules

| Structure     | C Module         | Description                                  |
|---------------|------------------|----------------------------------------------|
| `SafeMath`    | SafeMath         | Checked integer arithmetic (add, sub, mul, div, mod, abs, pow) |
| `SafeString`  | SafeString       | UTF-8 validation, SQL/HTML/JS escaping       |
| `SafePath`    | SafePath         | Directory traversal detection, filename sanitization |
| `SafeEmail`   | SafeEmail        | RFC 5321 email validation                    |
| `SafeUrl`     | SafeUrl + SafeHttp | URL parsing, percent-encoding/decoding     |
| `SafeNetwork` | SafeNetwork      | IPv4 parsing, private/loopback classification |
| `SafeCrypto`  | SafeCrypto + SafeHex + SafeChecksum | Constant-time comparison, random bytes, hex encode/decode, CRC32 |
| `SafeJson`    | SafeJson         | JSON validation and type detection           |
| `SafeDatetime`| SafeDateTime     | ISO 8601 parsing/formatting, leap year, days-in-month |
| `SafeFloat`   | SafeFloat + SafeCalculator + SafeAngle + SafeProbability + SafeUnit + SafeMl | Float division, sqrt, ln, expression evaluator, angles, probabilities, unit conversions, ML activations |
| `Proven`      | (all)            | Top-level re-export with `Proven.Math`, `Proven.Str`, etc. |

## Building

### MLton (recommended)

```sh
mlton -link-opt "-lproven -L/path/to/libproven" proven.mlb
```

Requires `libproven.so` (or `libproven.a`) on the linker search path.

### SML/NJ

SML/NJ does not support MLton's `_import` pragma. The `.cm` file is provided
for type-checking the wrapper layer. For runtime FFI under SML/NJ, provide
an NLFFI-based `lib_proven.sml` that loads `libproven.so` via `DynLink`.

## Usage

```sml
val () = ignore (Proven.init ())

(* Safe addition with overflow detection *)
val SOME sum = Proven.Math.add (100, 200)

(* Email validation *)
val SOME true = Proven.Email.isValid "user@example.com"

(* Safe division -- returns NONE on divide-by-zero *)
val NONE = Proven.Math.div' (42, 0)

(* HTML escaping *)
val SOME escaped = Proven.Str.escapeHtml "<script>alert('xss')</script>"

(* JSON validation *)
val SOME true = Proven.Json.isValid "{\"key\": \"value\"}"

val () = Proven.deinit ()
```

All functions return `SOME value` on success and `NONE` on error.

## Error Handling

The `ProvenStatus` structure exposes the underlying C error codes for
diagnostic purposes:

```sml
ProvenStatus.OK                   (*  0 *)
ProvenStatus.ERR_OVERFLOW         (* -3 *)
ProvenStatus.ERR_DIVISION_BY_ZERO (* -5 *)
(* etc. *)
```

## Requirements

- **MLton** (>= 20210117) for C FFI support
- **libproven** shared or static library (built from the proven repo)
- 64-bit platform (struct layout assumes LP64 ABI)

## License

PMPL-1.0-or-later
