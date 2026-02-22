# proven-kt

Kotlin JNA bindings for [libproven](https://github.com/hyperpolymath/proven) -- a formally verified safety library (Idris 2 + Zig FFI).

All computation is performed by the native `libproven` shared library. This binding contains **only** JNA-based data marshaling -- no logic is reimplemented on the JVM.

## Prerequisites

`libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows) must be available on the JNA library path at runtime.

## Installation

Add to your `build.gradle.kts`:

```kotlin
dependencies {
    implementation("io.github.hyperpolymath:proven:0.9.0")
}
```

## Usage

```kotlin
import io.github.hyperpolymath.proven.*

// Initialise the native runtime
Proven.init()

// Safe math (returns null on overflow / division by zero)
val sum = SafeMath.add(Long.MAX_VALUE, 1L)   // null (overflow)
val div = SafeMath.div(10L, 0L)              // null (division by zero)
val ok  = SafeMath.add(2L, 3L)              // 5L

// Safe string escaping (XSS prevention)
val escaped = SafeString.escapeHtml("<script>alert('xss')</script>")
// "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

// Email validation
SafeEmail.isValid("user@example.com")  // true
SafeEmail.isValid("invalid")           // false

// Path safety
SafePath.hasTraversal("../../../etc/passwd")  // true

// URL parsing
val url = SafeUrl.parse("https://example.com:8080/path?q=1#frag")
// ParsedUrl(scheme=https, host=example.com, port=8080, path=/path, query=q=1, fragment=frag)

// Safe expression evaluation
SafeCalculator.eval("(2 + 3) * 4")  // 20.0

// Data structures (auto-closeable, backed by native memory)
SafeQueue.create(100)?.use { queue ->
    queue.push(42L)
    val value = queue.pop()  // 42L
}

// Cleanup
Proven.deinit()
```

## Modules (38)

All 103 exported FFI functions from libproven are wrapped, covering:

- **SafeMath**: Overflow-safe arithmetic (div, mod, add, sub, mul, pow, abs, clamp)
- **SafeString**: UTF-8 validation, SQL/HTML/JS escaping
- **SafePath**: Traversal detection, filename sanitisation
- **SafeEmail**: RFC 5321 email validation
- **SafeUrl**: URL parsing into components
- **SafeNetwork**: IPv4 parsing, private/loopback classification
- **SafeCrypto**: Constant-time comparison, secure random bytes
- **SafeJson**: JSON validation and type detection
- **SafeDatetime**: ISO 8601 parsing/formatting, leap year, days-in-month
- **SafeFloat**: Safe division, sqrt, ln, finiteness checks
- **SafeVersion**: Semantic version parsing and comparison
- **SafeColor**: Hex color parsing, RGB-to-HSL conversion
- **SafeAngle**: Degree/radian conversion and normalisation
- **SafeUnit**: Length and temperature unit conversion
- **SafeBuffer**: Bounded buffer with append
- **SafeQueue**: Bounded FIFO queue
- **SafeBloom**: Bloom filter
- **SafeLRU**: LRU cache
- **SafeGraph**: Directed graph with adjacency matrix
- **SafeRateLimiter**: Token bucket rate limiting
- **SafeCircuitBreaker**: Circuit breaker pattern
- **SafeRetry**: Exponential backoff configuration
- **SafeMonotonic**: Monotonically increasing counter
- **SafeStateMachine**: State machine with explicit transitions
- **SafeCalculator**: Safe arithmetic expression evaluation

## Architecture

```
Kotlin JNA binding (this package)
    |
    | JNA FFI calls
    v
libproven.so (Zig C ABI bridge)
    |
    | Calls compiled Idris 2 RefC output
    v
Idris 2 formally verified core (THE TRUTH)
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
