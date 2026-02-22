# Proven -- Seed7 Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Seed7 FFI bindings for **libproven**, a formally verified safety library.
All computation is performed in the Idris 2 / Zig core; these bindings use
Seed7's `external` keyword with C linkage to call the ABI directly.

**No logic is reimplemented.** Every function calls `libproven` via Seed7's
external C interface.

## Prerequisites

- [Seed7](http://seed7.sourceforge.net/) compiler
- `libproven.so` (or `libproven.a`) built from the proven repository
- The `proven.h` C header (for the Seed7 compiler's C backend)

## Usage

```seed7
$ include "seed7_05.s7i";
  include "lib/proven.s7i";
  include "lib/safe_math.s7i";
  include "lib/safe_email.s7i";
  include "lib/safe_json.s7i";

const proc: main is func
  begin
    # Initialize the runtime
    block
      ignore(proven_init);
    exception
      catch RANGE_ERROR:
        writeln("Failed to initialize proven");
    end block;

    # Safe arithmetic
    block
      var IntResult: result is safeAdd(2147483647, 1);
      if succeeded(result) then
        writeln("Result: " <& result.value);
      else
        writeln("Overflow detected");
      end if;
    end block;

    # Email validation
    if isValidEmail("user@example.com") then
      writeln("Email is valid");
    end if;

    # JSON validation
    if jsonIsValid("{\"key\": \"value\"}") then
      writeln("JSON is valid");
    end if;

    # Cleanup
    proven_deinit;
  end func;
```

## Modules

| Module | Description |
|--------|-------------|
| `lib/proven.s7i` | Status codes, result types, lifecycle, version, memory management |
| `lib/safe_math.s7i` | Checked arithmetic (add, sub, mul, div, mod, abs, pow, clamp) |
| `lib/safe_string.s7i` | String escaping (SQL, HTML, JavaScript) and UTF-8 validation |
| `lib/safe_email.s7i` | Email address validation (RFC 5321) |
| `lib/safe_url.s7i` | URL validation and IPv4 address validation |
| `lib/safe_crypto.s7i` | Constant-time comparison, random bytes, hex encode/decode |
| `lib/safe_json.s7i` | JSON validation and type detection |

## Error Handling

Functions return result structs (`IntResult`, `StringResult`, `FloatResult`,
`BoolResult`) with a `status` field:

- `status = 0` indicates success
- `status < 0` indicates an error (see `ProvenStatus` enum in `proven.s7i`)

For string results, EMPTY (`""`) is returned on error. Use the `succeeded()`
helper to check status:

```seed7
var IntResult: res is safeAdd(a, b);
if succeeded(res) then
  writeln(res.value);
else
  writeln("Error: status = " <& res.status);
end if;
```

## Architecture

```
Seed7 (external C)  -->  C ABI (libproven.so)  -->  Zig FFI  -->  Idris 2 core
        ^                                                               ^
   this package                                                formally verified
```

## Compilation

Seed7 compiles to C, so linking against libproven is straightforward:

```bash
s7c -l proven your_program.sd7
```

Or with explicit library path:

```bash
s7c -L /path/to/libproven -l proven your_program.sd7
```
