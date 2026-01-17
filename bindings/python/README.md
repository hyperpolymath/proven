# proven-py

Python bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations for math, crypto, parsing, and validation.

## Installation

```bash
pip install proven
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

```python
from proven import SafeMath, SafeString, SafeEmail

# Safe math operations
result = SafeMath.add(1, 2)
if result.is_ok():
    print(result.unwrap())  # 3

# Overflow detection
overflow = SafeMath.add(2**63 - 1, 1)
if overflow.is_err():
    print("Overflow detected!")

# Safe string escaping
escaped = SafeString.escape_html("<script>alert('xss')</script>")
print(escaped)  # &lt;script&gt;...

# Email validation
if SafeEmail.is_valid("user@example.com"):
    print("Valid email")

# Rate limiting
from proven import TokenBucket
limiter = TokenBucket(capacity=100, refill_rate=10.0)
if limiter.try_acquire():
    # Request allowed
    pass

# State machines
from proven import StateMachineBuilder
machine = (StateMachineBuilder()
    .initial_state("idle")
    .add_transition("idle", "start", "running")
    .add_transition("running", "stop", "idle")
    .build())

# Geographic calculations
from proven import SafeGeo, Coordinate
nyc = Coordinate(40.7128, -74.0060)
la = Coordinate(34.0522, -118.2437)
distance = SafeGeo.haversine(nyc, la)  # ~3935 km
```

## License

AGPL-3.0-or-later
