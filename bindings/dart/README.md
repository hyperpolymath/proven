# Proven for Dart

Dart FFI bindings to **libproven** -- a formally verified safety library.

All computation is performed in Idris 2 with dependent types and totality
checking. This package is a **thin wrapper** that marshals arguments across
the C ABI boundary via `dart:ffi`. No logic is reimplemented in Dart.

## Prerequisites

The `libproven` shared library must be available on the library load path:

- Linux: `libproven.so`
- macOS: `libproven.dylib`
- Windows: `proven.dll`

Build from the `proven` repo root:

```bash
cd ffi/zig && zig build -Doptimize=ReleaseSafe
```

## Installation

Add to your `pubspec.yaml`:

```yaml
dependencies:
  proven:
    git:
      url: https://github.com/hyperpolymath/proven.git
      path: bindings/dart
```

## Usage

```dart
import 'package:proven/proven.dart';

void main() {
  // Safe math with overflow checking (returns null on overflow)
  final sum = SafeMath.add(1000000000000, 2000000000000);
  if (sum != null) {
    print('Sum: $sum');
  } else {
    print('Overflow!');
  }

  // XSS prevention (returns null on FFI error)
  final safeHtml = SafeString.escapeHtml('<script>alert("xss")</script>');
  print(safeHtml); // &lt;script&gt;...

  // Path traversal detection
  final hasTrav = SafePath.hasTraversal('../../../etc/passwd');
  if (hasTrav == true) {
    print('Dangerous path detected!');
  }

  // Email validation
  if (SafeEmail.isValid('user@example.com') == true) {
    print('Valid email');
  }

  // IP classification
  final classification = SafeNetwork.classifyIPv4('192.168.1.1');
  print(classification); // IpClassification.private_

  // Secure random bytes
  final bytes = SafeCrypto.randomBytes(32);
  print('Random: $bytes');

  // UUID generation
  final uuid = SafeUuid.v4();
  print('UUID: $uuid');
}
```

## API Design

All methods return **nullable types** instead of throwing exceptions:

- `int?` for integer operations (null = overflow/error)
- `double?` for float operations (null = NaN/Infinity/error)
- `String?` for string operations (null = FFI error)
- `bool?` for validation operations (null = FFI error)
- Custom value types for complex results (null = parse error)

This ensures callers must handle error cases explicitly.

## Modules

| Module | Description |
|--------|-------------|
| `SafeMath` | Overflow-checked integer arithmetic |
| `SafeString` | HTML/SQL/JS escaping, UTF-8 validation |
| `SafeFloat` | NaN/Infinity-safe floating-point operations |
| `SafePath` | Directory traversal detection, filename sanitization |
| `SafeEmail` | Email address validation |
| `SafeNetwork` | IPv4 parsing and classification |
| `SafeCrypto` | Constant-time comparison, secure random bytes |
| `SafeHex` | Hexadecimal encoding/decoding |
| `SafeUuid` | UUID v4 generation and validation (RFC 4122) |
| `SafeJson` | JSON validation |
| `SafeColor` | Color parsing (hex) and RGB/HSL conversion |
| `SafeAngle` | Angle unit conversion and normalization |
| `SafeUnit` | Physical unit conversions (length, temperature) |
| `SafeCurrency` | Currency parsing (ISO 4217) |
| `SafePhone` | Phone number parsing (E.164) |
| `SafeDateTime` | ISO 8601 parsing, leap year, days-in-month |
| `SafeVersion` | Semantic version parsing (SemVer 2.0.0) |
| `SafeUrl` | URL encoding and decoding |

## Architecture

```
Dart app
  |
  v
lib/src/safe_*.dart   (thin wrappers, nullable return types)
  |
  v
lib/src/ffi.dart      (dart:ffi bindings, struct definitions)
  |
  v
libproven.so          (Zig FFI layer)
  |
  v
Idris 2 RefC          (formally verified core with dependent types)
```

## License

PMPL-1.0-or-later
