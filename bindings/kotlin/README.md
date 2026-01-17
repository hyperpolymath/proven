# proven-kt

Kotlin bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations for math, crypto, parsing, and validation.

## Installation

Add to your `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.hyperpolymath:proven:0.4.0")
}
```

Or in Maven `pom.xml`:

```xml
<dependency>
    <groupId>com.hyperpolymath</groupId>
    <artifactId>proven</artifactId>
    <version>0.4.0</version>
</dependency>
```

## Modules (38)

### Core Modules (11)
- **SafeMath**: Overflow detection, safe division, bounded integers
- **SafeString**: UTF-8 validation, injection-safe escaping (HTML, SQL, JS, URL, shell)
- **SafePath**: Path traversal prevention
- **SafeEmail**: RFC 5321/5322 validation
- **SafeUrl**: RFC 3986 compliant parsing
- **SafeNetwork**: IPv4/IPv6 parsing, CIDR notation
- **SafeCrypto**: Secure hashing (SHA-3, BLAKE3)
- **SafeUuid**: UUID generation and validation
- **SafeCurrency**: Currency handling with precision
- **SafePhone**: Phone number parsing and validation
- **SafeHex**: Hexadecimal encoding/decoding

### Data Modules (7)
- **SafeJson**: Exception-free parsing with validation
- **SafeDatetime**: ISO 8601 parsing, date validation
- **SafeFloat**: NaN/Infinity prevention
- **SafeVersion**: Semantic versioning
- **SafeColor**: RGB/HSL with WCAG contrast
- **SafeAngle**: Angle conversions and trigonometry
- **SafeUnit**: Unit conversions (length, mass, temperature, etc.)

### Data Structure Modules (5)
- **SafeBuffer**: Bounded, ring, and growable buffers
- **SafeQueue**: Priority queues and deques
- **SafeBloom**: Bloom filters
- **SafeLRU**: LRU caches with TTL
- **SafeGraph**: Directed and undirected graphs with algorithms

### Resilience Modules (4)
- **SafeRateLimiter**: Token bucket, sliding window, leaky bucket
- **SafeCircuitBreaker**: Circuit breaker pattern
- **SafeRetry**: Retry strategies with backoff
- **SafeMonotonic**: Monotonic counters and timestamps

### State Modules (2)
- **SafeStateMachine**: State machine builder
- **SafeCalculator**: Expression parser and calculator

### Algorithm Modules (4)
- **SafeGeo**: Geographic calculations (haversine, bearing)
- **SafeProbability**: Probability distributions
- **SafeChecksum**: CRC-32, Adler-32, FNV, Luhn
- **SafeTensor**: Tensor operations

### Security Modules (2)
- **SafePassword**: Password validation and generation
- **SafeML**: Machine learning utilities

### HTTP Modules (3)
- **SafeHeader**: HTTP header validation
- **SafeCookie**: HTTP cookie handling
- **SafeContentType**: MIME type handling

## Usage

```kotlin
import com.hyperpolymath.proven.*

// Safe math operations
val sum = SafeMath.addChecked(Long.MAX_VALUE, 1)
// Returns Result.failure (overflow detected)

val div = SafeMath.divChecked(10, 0)
// Returns Result.failure (division by zero)

// Safe string escaping
val escaped = SafeString.escapeHtml("<script>alert('xss')</script>")
// Returns "&lt;script&gt;..."

// Email validation
SafeEmail.isValid("user@example.com")  // true
SafeEmail.isValid("invalid")           // false

// JSON parsing
val json = SafeJson.parse("""{"name": "Alice", "age": 30}""")
val name = json.get("name")?.asString  // "Alice"

// Rate limiting
val limiter = TokenBucket(capacity = 100, refillRate = 10.0)
if (limiter.tryAcquire()) {
    // Request allowed
}

// State machines
val machine = StateMachineBuilder<String, String>()
    .initialState("idle")
    .state("idle") {
        on("start") { transitionTo("running") }
    }
    .state("running") {
        on("stop") { transitionTo("idle") }
    }
    .build()
```

## Features

- **Thread-safe**: Uses `ReentrantLock` for concurrent access
- **Kotlin-idiomatic**: Value classes, sealed classes, extension functions
- **Type-safe**: Leverages Kotlin's null safety and Result types
- **Zero dependencies**: Pure Kotlin implementation

## License

AGPL-3.0-or-later
