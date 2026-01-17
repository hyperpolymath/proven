# proven-rs

Rust bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations for math, crypto, parsing, and validation.

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
proven = "0.4"
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

```rust
use proven::{SafeMath, SafeString, SafeEmail, Result};

// Safe math operations
let sum = SafeMath::add(i64::MAX, 1);
assert!(sum.is_err()); // Overflow detected

let div = SafeMath::div(10, 0);
assert!(div.is_err()); // Division by zero caught

// Safe string escaping
let escaped = SafeString::escape_html("<script>alert('xss')</script>");
assert!(!escaped.contains('<'));

let sql = SafeString::escape_sql("O'Brien");
assert_eq!(sql, "O''Brien");

// Email validation
assert!(SafeEmail::is_valid("user@example.com"));
assert!(!SafeEmail::is_valid("invalid"));

// Rate limiting
let limiter = SafeRateLimiter::token_bucket(100, 10.0);
assert!(limiter.acquire().is_ok());

// State machines
let machine = SafeStateMachine::builder()
    .state("idle")
    .state("running")
    .transition("idle", "start", "running")
    .build();
```

## License

AGPL-3.0-or-later
