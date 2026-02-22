# Proven - Lean 4 FFI Bindings

Lean 4 bindings for [libproven](https://github.com/hyperpolymath/proven), a
formally verified safety library. The core logic is implemented in Idris 2
with dependent types and totality checking, exposed via a Zig FFI bridge as a
stable C ABI.

**No logic is reimplemented in Lean.** Every operation delegates to the
verified Idris 2 core through `@[extern]` C FFI declarations.

## Modules

| Module               | Description                                      |
|----------------------|--------------------------------------------------|
| `Proven.FFI`         | Raw `@[extern]` C FFI declarations               |
| `Proven.SafeMath`    | Checked integer arithmetic (add, sub, mul, div)   |
| `Proven.SafeString`  | UTF-8 validation, SQL/HTML/JS escaping            |
| `Proven.SafePath`    | Directory traversal detection, filename sanitize  |
| `Proven.SafeEmail`   | RFC 5321 email validation                         |
| `Proven.SafeUrl`     | URL parsing into components                       |
| `Proven.SafeNetwork` | IPv4 parsing and classification                   |
| `Proven.SafeCrypto`  | Constant-time eq, random bytes, hex, CRC32        |
| `Proven.SafeJson`    | JSON validation and type detection                |
| `Proven.SafeDateTime`| ISO 8601 parsing, formatting, calendar utilities  |
| `Proven.SafeFloat`   | Safe float division, sqrt, ln                     |

## Prerequisites

- Lean 4 (v4.16.0 or compatible, see `lean-toolchain`)
- `libproven` shared or static library installed and linkable
- C compiler for the FFI shim (`ffi_shim.c`)

Set `LIBRARY_PATH` and `LD_LIBRARY_PATH` if libproven is not in a standard
location.

## Build

```bash
lake build
```

## Usage

```lean
import Proven

def main : IO Unit := do
  -- Initialize runtime (required once)
  Proven.init

  -- Safe arithmetic
  match (<- Proven.SafeMath.add 100 200) with
  | .ok sum   => IO.println s!"100 + 200 = {sum}"
  | .error e  => IO.println s!"Error: {e}"

  -- Email validation
  let valid <- Proven.SafeEmail.validate "user@example.com"
  IO.println s!"Email valid: {valid}"

  -- Clean up
  Proven.deinit
```

## Architecture

```
Lean 4 code (this package)
    |
    | @[extern] FFI calls
    v
ffi_shim.c (ByteArray/String marshaling)
    |
    | C function calls
    v
libproven.so (Zig FFI bridge)
    |
    | Zig -> C ABI -> RefC
    v
Idris 2 (formally verified core)
```

## Error handling

All fallible operations return `Except E T` where `E` carries a
`ProvenStatus` code. Convenience `?`-suffixed variants return
`Option T` instead.

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
<jonathan.jewell@open.ac.uk>
