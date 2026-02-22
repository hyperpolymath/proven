# Proven-Swift

Swift bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations for math, crypto, parsing, and validation.

## Installation

### Swift Package Manager

Add to your `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/hyperpolymath/proven.git", from: "0.4.0")
]
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

```swift
import Proven

// Safe math operations
let sum = SafeMath.addChecked(Int.max, 1)
// Returns nil (overflow detected)

let div = SafeMath.divChecked(10, 0)
// Returns nil (division by zero)

// Safe string escaping
let escaped = SafeString.escapeHtml("<script>alert('xss')</script>")
// Returns "&lt;script&gt;..."

// Email validation
SafeEmail.isValid("user@example.com")  // true
SafeEmail.isValid("invalid")           // false

// JSON parsing
if let json = SafeJson.parse(#"{"name": "Alice", "age": 30}"#) {
    let name = json["name"]?.asString  // "Alice"
}

// Rate limiting
let limiter = TokenBucket(capacity: 100, refillRate: 10.0)
if limiter.tryAcquire() {
    // Request allowed
}

// State machines
let machine = StateMachineBuilder<String, String>()
    .initialState("idle")
    .state("idle") { state in
        state.on("start") { _ in .transition("running") }
    }
    .state("running") { state in
        state.on("stop") { _ in .transition("idle") }
    }
    .build()

// Geographic calculations
let nyc = Coordinate(latitude: 40.7128, longitude: -74.0060)
let la = Coordinate(latitude: 34.0522, longitude: -118.2437)
let distance = SafeGeo.haversine(from: nyc, to: la)  // ~3935 km
```

## Features

- **Thread-safe**: Uses `NSLock` for concurrent access
- **Swift-idiomatic**: Optionals, Result types, protocol-oriented design
- **Type-safe**: Leverages Swift's strong type system
- **Zero dependencies**: Pure Swift implementation

## License

PMPL-1.0-or-later
