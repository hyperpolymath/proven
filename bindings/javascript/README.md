# Proven JavaScript Bindings (FFI)

Thin FFI wrapper for the formally verified **libproven** safety library.
All computation delegates to Idris 2 (dependent types + totality checking)
via the Zig FFI bridge. This binding loads `libproven.so`/`libproven.dylib`
through `Deno.dlopen`.

## Architecture

```
JavaScript (this package)
    |
    | Deno.dlopen / FFI
    v
libproven.so (Zig C ABI)
    |
    | extern fn calls
    v
Idris 2 RefC (formally verified)
```

**No logic is reimplemented in JavaScript.** Every function call goes
through the FFI to Idris 2 compiled code.

## Requirements

- **Deno >= 1.40** (for `Deno.dlopen` FFI support)
- **libproven** shared library built from `ffi/zig/`
- Run with `--allow-ffi` and `--unstable-ffi` flags

## Building libproven

```bash
cd ../../ffi/zig
zig build -Doptimize=ReleaseSafe
# Output: zig-out/lib/libproven.so (or .dylib on macOS)
```

## Usage

```javascript
import { SafeMath, SafeString, SafeAngle, isAvailable } from '@hyperpolymath/proven';

// Check if FFI is available
if (!isAvailable()) {
  console.error('libproven not loaded');
  Deno.exit(1);
}

// Safe math (calls Idris via FFI)
const sum = SafeMath.add(Number.MAX_SAFE_INTEGER - 1, 1);
console.log(sum); // { ok: true, value: 9007199254740991 }

const overflow = SafeMath.add(Number.MAX_SAFE_INTEGER, 1);
console.log(overflow); // { ok: false, error: 'Integer overflow' }

// Safe string escaping (calls Idris via FFI)
const escaped = SafeString.escapeHtml("<script>alert('xss')</script>");
console.log(escaped); // { ok: true, value: '&lt;script&gt;...' }

// Angle conversion (calls Idris via FFI)
const rad = SafeAngle.degToRad(180);
console.log(rad); // 3.141592653589793
```

## Custom library path

Set `PROVEN_LIB_PATH` environment variable:

```bash
PROVEN_LIB_PATH=/opt/proven/lib/libproven.so deno run --allow-ffi --unstable-ffi app.ts
```

## Subpath imports

Each module is available as a subpath import:

```javascript
import { SafeMath } from '@hyperpolymath/proven/math';
import { SafeUUID } from '@hyperpolymath/proven/uuid';
import { BloomFilter } from '@hyperpolymath/proven/bloom';
```

## Modules (39)

### Core (11)
SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork,
SafeCrypto, SafeUUID, SafeCurrency (Money), SafePhone, SafeHex

### Data (7)
SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor, SafeAngle, SafeUnit

### Data Structures (5)
BoundedBuffer, BoundedQueue, BloomFilter, LruCache, DirectedGraph

### Resilience (4)
TokenBucket, CircuitBreaker, Retry, MonotonicCounter

### State (2)
StateMachine, SafeCalculator

### Algorithm (4)
SafeGeo, SafeProbability, SafeChecksum, Matrix (Tensor)

### Security (2)
SafePassword, SafeML

### HTTP (3)
SafeHeader, SafeCookie, SafeContentType

## License

PMPL-1.0-or-later
