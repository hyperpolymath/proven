<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven C Bindings

Header-only C wrapper providing type-safe access to **libproven**, a formally
verified safety library implemented in Idris 2 with a Zig FFI bridge exposing
a stable C ABI.

## Quick Start

```c
#include <proven.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    proven_init();

    /* Safe division -- no crash on divide by zero */
    ProvenIntResult r = proven_math_div(10, 0);
    if (PROVEN_FAILED(r)) {
        printf("Handled safely: status=%d\n", r.status);
    }

    /* XSS prevention */
    const char* input = "<script>alert(1)</script>";
    ProvenStringResult html = proven_string_escape_html(
        (const uint8_t*)input, strlen(input)
    );
    if (PROVEN_SUCCEEDED(html)) {
        printf("Safe: %s\n", html.value);
        proven_free_string(html.value);
    }

    proven_deinit();
    return 0;
}
```

## Files

| File | Purpose |
|------|---------|
| `include/proven.h` | Complete header with all 103+ function declarations |
| `include/proven_types.h` | Lightweight types-only header (subset of proven.h) |
| `proven.pc.in` | pkg-config template |
| `examples/example.c` | Usage demonstration |

## Installation

### 1. Build the Zig library

```bash
cd ffi/zig
zig build -Doptimize=ReleaseSafe
```

### 2. Copy header and library

```bash
# Header
sudo cp bindings/c/include/proven.h /usr/local/include/

# Library (choose one)
sudo cp ffi/zig/zig-out/lib/libproven.so /usr/local/lib/   # Dynamic
sudo cp ffi/zig/zig-out/lib/libproven.a  /usr/local/lib/   # Static
```

### 3. Install pkg-config file (optional)

```bash
sed 's|@PREFIX@|/usr/local|g' bindings/c/proven.pc.in \
    > /usr/local/lib/pkgconfig/proven.pc
```

### 4. Update library cache (Linux)

```bash
sudo ldconfig
```

## Compilation

```bash
# With pkg-config (recommended)
gcc -o myapp myapp.c $(pkg-config --cflags --libs proven)

# Dynamic linking
gcc -o myapp myapp.c -lproven

# Static linking
gcc -o myapp myapp.c -lproven -static

# Explicit paths
gcc -o myapp myapp.c \
    -I/path/to/proven/bindings/c/include \
    -L/path/to/proven/ffi/zig/zig-out/lib \
    -lproven
```

## API Overview (103+ functions across 41 modules)

| Module | Functions | Description |
|--------|-----------|-------------|
| **Lifecycle** | `init`, `deinit`, `is_initialized` | Runtime management |
| **Version** | `ffi_abi_version`, `version_major/minor/patch`, `module_count` | Version info |
| **SafeMath** | `div`, `mod`, `add/sub/mul_checked`, `abs_safe`, `clamp`, `pow_checked` | Overflow-safe arithmetic |
| **SafeString** | `is_valid_utf8`, `escape_sql/html/js` | Encoding-safe text |
| **SafePath** | `has_traversal`, `sanitize_filename` | Path traversal prevention |
| **SafeCrypto** | `constant_time_eq`, `random_bytes` | Timing-safe crypto primitives |
| **SafeUrl** | `url_parse`, `url_free` | URL component extraction |
| **SafeEmail** | `email_is_valid` | RFC 5321 email validation |
| **SafeNetwork** | `parse_ipv4`, `ipv4_is_private/loopback` | IP address classification |
| **SafeHeader** | `has_crlf`, `is_valid_name`, `is_dangerous`, `render`, `build_csp/hsts` | HTTP header safety |
| **SafeCookie** | `has_injection`, `validate_name/value`, `get_prefix`, `build_set_cookie/delete` | Cookie injection prevention |
| **SafeContentType** | `parse`, `free`, `can_sniff_dangerous`, `render`, `is_json/xml` | MIME type handling |
| **SafeUUID** | `v4`, `to_string`, `parse`, `is_nil`, `version` | UUID generation/parsing |
| **SafeJson** | `is_valid`, `get_type` | JSON validation |
| **SafeDateTime** | `parse`, `format_iso8601`, `is_leap_year`, `days_in_month` | ISO 8601 date/time |
| **SafeFloat** | `div`, `is_finite`, `is_nan`, `sqrt`, `ln` | Safe floating-point |
| **SafePassword** | `validate`, `is_common` | Password strength checking |
| **SafeHex** | `encode`, `decode`, `free` | Hex encoding/decoding |
| **SafeCurrency** | `parse`, `format` | ISO 4217 monetary values |
| **SafePhone** | `parse`, `format_e164` | E.164 phone numbers |
| **SafeVersion** | `parse`, `compare`, `free` | Semantic versioning |
| **SafeGeo** | `validate`, `distance`, `in_bounds` | Geographic coordinates |
| **SafeChecksum** | `crc32`, `verify_crc32` | CRC verification |
| **SafeProbability** | `create`, `and`, `or_exclusive`, `not` | Probability arithmetic |
| **SafeCalculator** | `eval` | Expression evaluation |
| **SafeBuffer** | `create`, `append`, `get`, `free` | Bounded buffers |
| **SafeRateLimiter** | `create`, `try_acquire`, `free` | Token bucket rate limiting |
| **SafeCircuitBreaker** | `create`, `allow`, `success`, `failure`, `state`, `free` | Fault tolerance |
| **SafeRetry** | `delay`, `should_retry` | Exponential backoff |
| **SafeMonotonic** | `create`, `next`, `free` | Monotonic sequences |
| **SafeStateMachine** | `create`, `allow`, `transition`, `state`, `free` | State transitions |
| **SafeTensor** | `create`, `set`, `get`, `matmul`, `free` | 2D tensor operations |
| **SafeMl** | `softmax`, `sigmoid`, `relu`, `leaky_relu`, `clamp` | ML activation functions |
| **SafeLru** | `create`, `get`, `put`, `free` | LRU cache |
| **SafeGraph** | `create`, `add_edge`, `has_edge`, `free` | Directed graph |
| **SafeRegistry** | `parse`, `to_string`, `has_registry` | OCI image references |
| **SafeDigest** | `parse`, `verify`, `to_string` | Cryptographic digests |
| **SafeHttp** | `url_encode`, `url_decode`, `parse_www_authenticate` | HTTP encoding |
| **SafeColor** | `parse_hex`, `rgb_to_hsl`, `to_hex` | Color conversions |
| **SafeAngle** | `deg_to_rad`, `rad_to_deg`, `normalize_degrees/radians` | Angle math |
| **SafeUnit** | `convert_length`, `convert_temp` | Physical unit conversions |
| **SafeQueue** | `create`, `push`, `pop`, `size`, `free` | Bounded FIFO queue |
| **SafeBloom** | `create`, `add`, `contains`, `free` | Bloom filter |
| **Callbacks** | `register`, `unregister`, `fire`, `count`, `clear_all` | Event system |

## Memory Management

| Result Type | How to Free |
|-------------|-------------|
| `ProvenStringResult` | `proven_free_string(result.value)` |
| `ProvenUrlResult` | `proven_url_free(&result.components)` |
| `ProvenContentTypeResult` | `proven_content_type_free(&result)` |
| `ProvenHexDecodeResult` | `proven_hex_free(&result)` |
| `ProvenVersionResult` | `proven_version_free(&result.version)` |
| `ProvenBoundedBuffer*` | `proven_buffer_free(ptr)` |
| `ProvenBoundedQueue*` | `proven_queue_free(ptr)` |
| `ProvenBloomFilter*` | `proven_bloom_free(ptr)` |
| `ProvenLRUCache*` | `proven_lru_free(ptr)` |
| `ProvenGraph*` | `proven_graph_free(ptr)` |
| `ProvenRateLimiter*` | `proven_rate_limiter_free(ptr)` |
| `ProvenCircuitBreaker*` | `proven_circuit_breaker_free(ptr)` |
| `ProvenMonotonicCounter*` | `proven_monotonic_free(ptr)` |
| `ProvenStateMachine*` | `proven_state_machine_free(ptr)` |
| `ProvenTensor2D*` | `proven_tensor_free(ptr)` |
| Integer/bool/float results | No freeing needed |

## Error Handling

All functions return a status code. Use the convenience macros:

```c
if (PROVEN_SUCCEEDED(result)) {
    /* Use result.value */
} else {
    /* Handle error based on result.status */
    switch (result.status) {
    case PROVEN_ERR_DIVISION_BY_ZERO: /* ... */ break;
    case PROVEN_ERR_OVERFLOW:         /* ... */ break;
    /* etc. */
    }
}
```

## Status Codes

| Code | Value | Meaning |
|------|-------|---------|
| `PROVEN_OK` | 0 | Success |
| `PROVEN_ERR_NULL_POINTER` | -1 | Null pointer passed |
| `PROVEN_ERR_INVALID_ARGUMENT` | -2 | Invalid argument |
| `PROVEN_ERR_OVERFLOW` | -3 | Integer overflow |
| `PROVEN_ERR_UNDERFLOW` | -4 | Integer underflow |
| `PROVEN_ERR_DIVISION_BY_ZERO` | -5 | Division by zero |
| `PROVEN_ERR_PARSE_FAILURE` | -6 | Parse error |
| `PROVEN_ERR_VALIDATION_FAILED` | -7 | Validation failed |
| `PROVEN_ERR_OUT_OF_BOUNDS` | -8 | Index out of bounds |
| `PROVEN_ERR_ENCODING_ERROR` | -9 | UTF-8 encoding error |
| `PROVEN_ERR_ALLOCATION_FAILED` | -10 | Memory allocation failed |
| `PROVEN_ERR_NOT_IMPLEMENTED` | -99 | Not yet implemented |

## Thread Safety

The Proven library is thread-safe. Multiple threads can call functions
concurrently after `proven_init()` has been called once. The callback
system uses internal mutex protection.

## Architecture

```
 Your C code
      |
      v
 proven.h (this header -- declarations only, no logic)
      |
      v
 libproven.so / libproven.a (Zig-compiled shared/static library)
      |
      v
 Zig FFI bridge (ffi/zig/src/main.zig -- marshals C ABI calls)
      |
      v
 Idris 2 verified core (src/Proven/*.idr -- all computation here)
```

All computation happens in formally verified Idris 2 code. This C header
is a pure declaration layer -- it does NOT reimplement any logic.

## License

PMPL-1.0-or-later
