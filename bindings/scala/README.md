# proven-scala

Scala JNA bindings for [libproven](https://github.com/hyperpolymath/proven) -- a formally verified safety library (Idris 2 + Zig FFI).

All computation is performed by the native `libproven` shared library. This binding contains **only** JNA-based data marshaling -- no logic is reimplemented on the JVM.

## Prerequisites

`libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows) must be available on the JNA library path at runtime.

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.hyperpolymath" %% "proven" % "0.9.0"
```

## Usage

```scala
import io.github.hyperpolymath.proven.*

// Initialise the native runtime
Proven.init()

// Safe math (returns Option -- None on overflow / division by zero)
SafeMath.add(Long.MaxValue, 1L)   // None (overflow)
SafeMath.div(10L, 0L)             // None (division by zero)
SafeMath.add(2L, 3L)              // Some(5L)

// For comprehension
for
  a <- SafeMath.add(1L, 2L)
  b <- SafeMath.mul(a, 3L)
yield b  // Some(9L)

// Safe string escaping (XSS prevention)
SafeString.escapeHtml("<script>alert('xss')</script>")
// Some("&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;")

// Email validation
SafeEmail.isValid("user@example.com")  // true
SafeEmail.isValid("invalid")           // false

// Path safety
SafePath.hasTraversal("../../../etc/passwd")  // true

// URL parsing
SafeUrl.parse("https://example.com:8080/path?q=1#frag")
// Some(ParsedUrl(https, example.com, Some(8080), /path, Some(q=1), Some(frag)))

// Pattern matching
SafeUrl.parse(url) match
  case Some(ParsedUrl(scheme, host, port, path, query, fragment)) =>
    println(s"Host: $host")
  case None =>
    println("Invalid URL")

// Safe expression evaluation
SafeCalculator.eval("(2 + 3) * 4")  // Some(20.0)

// Data structures (AutoCloseable, backed by native memory)
SafeQueue.create(100).foreach { queue =>
  try
    queue.push(42L)
    val value = queue.pop()  // Some(42L)
  finally queue.close()
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
Scala JNA binding (this package)
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
