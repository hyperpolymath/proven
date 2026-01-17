# Proven TypeScript Bindings

TypeScript bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations with full type safety.

## Installation

```bash
npm install @proven/typescript
# or
deno add @proven/typescript
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

```typescript
import {
  SafeMath,
  SafeString,
  SafeEmail,
  Result,
  TokenBucket,
  StateMachineBuilder,
  Coordinate,
  SafeGeo,
} from '@proven/typescript';

// Safe math operations with full type inference
const sum: Result<number, OverflowError> = SafeMath.addChecked(Number.MAX_SAFE_INTEGER, 1);
if (sum.isErr()) {
  console.log('Overflow detected!');
}

// Safe string escaping
const escaped: string = SafeString.escapeHtml("<script>alert('xss')</script>");

// Email validation
const isValid: boolean = SafeEmail.isValid("user@example.com");

// Rate limiting
const limiter = new TokenBucket(100, 10.0);
if (limiter.tryAcquire()) {
  // Request allowed
}

// Type-safe state machines
const machine = new StateMachineBuilder<string, string>()
  .initialState('idle')
  .addTransition('idle', 'start', 'running')
  .addTransition('running', 'stop', 'idle')
  .build();

const currentState: string = machine.currentState;

// Geographic calculations
const nyc: Coordinate = { latitude: 40.7128, longitude: -74.0060 };
const la: Coordinate = { latitude: 34.0522, longitude: -118.2437 };
const distance: number = SafeGeo.haversine(nyc, la);
```

## Type Definitions

Full TypeScript definitions are included for all 38 modules:

```typescript
// Result type for safe error handling
type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

// Example type definitions
interface Coordinate {
  latitude: number;  // -90 to 90
  longitude: number; // -180 to 180
}

interface Money {
  amount: bigint;
  currency: CurrencyCode;
  format(): string;
  add(other: Money): Result<Money, CurrencyMismatch>;
}
```

## See Also

- `../javascript/` - JavaScript bindings (no types)
- `../rescript/` - ReScript bindings (compiles to JS)
- `../deno/` - Deno-specific binding

## License

AGPL-3.0-or-later
