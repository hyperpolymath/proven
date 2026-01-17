# Proven JavaScript Bindings

Safe, formally verified library for JavaScript - 38 modules of operations that cannot crash.

## Installation

```bash
npm install @proven/javascript
# or
deno add @proven/javascript
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

```javascript
import {
  SafeMath,
  SafeString,
  SafeEmail,
  SafeUUID,
  Money,
  CurrencyCode,
  TokenBucket,
  StateMachineBuilder,
} from '@proven/javascript';

// Safe math operations
const sum = SafeMath.addChecked(Number.MAX_SAFE_INTEGER, 1);
if (sum.isErr()) {
  console.log('Overflow detected!');
}

// Safe string escaping
const escaped = SafeString.escapeHtml("<script>alert('xss')</script>");
console.log(escaped);  // &lt;script&gt;...

// UUID operations
const uuidResult = SafeUUID.parse('550e8400-e29b-41d4-a716-446655440000');
if (uuidResult.ok) {
  console.log(uuidResult.value.toString());
  console.log(uuidResult.value.getVersion());  // 'v4'
}

// Money operations (no floating-point errors)
const moneyResult = Money.fromMajorUnits(10.50, CurrencyCode.USD);
if (moneyResult.ok) {
  const doubled = moneyResult.value.multiply(2);
  console.log(doubled.value.format());  // '$21.00'
}

// Rate limiting
const limiter = new TokenBucket(100, 10.0);
if (limiter.tryAcquire()) {
  // Request allowed
}

// State machines
const machine = new StateMachineBuilder()
  .initialState('idle')
  .addTransition('idle', 'start', 'running')
  .addTransition('running', 'stop', 'idle')
  .build();
```

## License

AGPL-3.0-or-later
