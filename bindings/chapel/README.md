<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - Chapel Binding

FFI binding to **libproven**, a formally verified safety library implemented
in Idris 2 with a Zig FFI bridge exposing a stable C ABI.

**All computation is performed in the Idris 2 core.  This binding is a thin
wrapper; it does NOT reimplement any logic.**

Chapel is an HPC parallel computing language with native C interop, making
it natural to call libproven via `extern` declarations.

## Requirements

- Chapel 1.31+ (tested with Chapel 2.x)
- `libproven.so` (Linux), `libproven.dylib` (macOS) on library path
- `proven.h` header from `bindings/c/include/`

## Quick Start

```chapel
use Proven;

// Initialize the runtime.
provenInit();

// Safe arithmetic.
var sum = safeAdd(2: int(64), 3: int(64));       // some(5)
var bad = safeDiv(10: int(64), 0: int(64));      // none

// String safety.
var html = escapeHtml("<script>alert(1)</script>");  // some("&lt;script&gt;...")

// Email validation.
var ok = SafeEmail.isValid("user@example.com");  // some(true)

// Path traversal detection.
var trav = hasTraversal("../../etc/passwd");      // some(true)

// JSON validation.
var valid = SafeJson.isValid("{\"key\": 42}");   // some(true)

// URL encoding.
var enc = SafeUrl.encode("hello world");         // some("hello%20world")

// Cryptographic random bytes.
var rng = randomBytes(32);                       // some([...32 bytes...])

// Date/time.
var leap = isLeapYear(2024: int(32));            // true

// Clean up.
provenDeinit();
```

## Building

```bash
# Ensure libproven is installed and the header is findable.
export CHPL_INCLUDE_PATH=/path/to/proven/bindings/c/include
export CHPL_LIB_PATH=/path/to/libproven

# Compile with Mason.
mason build

# Or compile directly.
chpl -o myapp myapp.chpl -M src/ -lproven
```

## Error Handling

All fallible operations return Chapel optional types (`T?`).  Use pattern
matching to handle errors:

```chapel
var result = safeDiv(10: int(64), 0: int(64));
if result == none {
  writeln("Division by zero detected safely.");
} else {
  writeln("Result: ", result!);
}
```

## Modules

| Module | Description |
|--------|-------------|
| `LibProven` | Low-level extern declarations and helper functions |
| `SafeMath` | Overflow-safe integer arithmetic |
| `SafeString` | XSS/SQL/JS escaping, UTF-8 validation |
| `SafePath` | Directory traversal prevention |
| `SafeEmail` | RFC 5321 email validation |
| `SafeUrl` | RFC 3986 URL encoding/decoding |
| `SafeCrypto` | Constant-time comparison, random bytes, hex encoding |
| `SafeJson` | JSON validation and type detection |
| `SafeDateTime` | ISO 8601 parsing/formatting, leap year, days-in-month |
| `Proven` | Umbrella module re-exporting everything |

## Architecture

```
Chapel program  -->  extern proc (C interop)  -->  libproven.so
                                                       |
                                                  Zig FFI bridge
                                                       |
                                                  Idris 2 core
                                              (formally verified)
```

## License

PMPL-1.0-or-later
