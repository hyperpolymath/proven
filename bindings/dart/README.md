# Proven for Dart

Safe, validated operations library for Dart and Flutter applications.

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
  // Safe math with overflow checking
  final result = SafeMath.add(1000000000000, 2000000000000);
  if (result.isOk) {
    print('Sum: ${result.value}');
  } else {
    print('Overflow!');
  }

  // XSS prevention
  final userInput = '<script>alert("xss")</script>';
  final safeHtml = SafeString.escapeHtml(userInput);
  print(safeHtml); // &lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;

  // Path safety
  if (SafePath.hasTraversal('../../../etc/passwd')) {
    print('Dangerous path detected!');
  }

  // Email validation
  final email = 'user@example.com';
  if (SafeEmail.isValid(email)) {
    print('Valid email');
    print('Domain: ${SafeEmail.getDomain(email)}');
  }

  // IP classification
  final ip = IPv4Address.parse('192.168.1.1');
  if (ip != null) {
    print('Is private: ${ip.isPrivate}'); // true
    print('Classification: ${ip.classification}'); // private
  }

  // Secure tokens
  final token = SafeCrypto.generateToken();
  print('CSRF token: $token');

  // Password hashing
  final hash = SafeCrypto.sha256Hash('password');
  print('Hash: $hash');
}
```

## Modules

### SafeMath

Overflow-checked arithmetic operations:

```dart
// All operations return CheckedInt
final sum = SafeMath.add(a, b);
final diff = SafeMath.sub(a, b);
final prod = SafeMath.mul(a, b);
final quot = SafeMath.div(a, b);

// Check result
if (sum.isOk) {
  print(sum.value);
}

// Or use unwrap (throws on overflow)
try {
  final value = SafeMath.mul(bigNumber, anotherBig).unwrap();
} on OverflowException {
  print('Overflow!');
}

// Safe default
final value = SafeMath.div(x, y).unwrapOr(0);
```

### SafeString

XSS prevention and string sanitization:

```dart
// HTML escaping
final safe = SafeString.escapeHtml('<script>');

// SQL escaping (prefer parameterized queries!)
final safeSql = SafeString.escapeSql("O'Brien");

// JavaScript escaping
final safeJs = SafeString.escapeJs(userInput);

// URL encoding
final encoded = SafeString.urlEncode('hello world');

// String sanitization
final clean = SafeString.sanitize(input, allowed: r'a-zA-Z0-9');
```

### SafePath

Directory traversal protection:

```dart
// Check for traversal
if (SafePath.hasTraversal(userPath)) {
  throw SecurityException('Path traversal detected');
}

// Sanitize filename
final safe = SafePath.sanitizeFilename(userFilename);

// Safe path joining
final result = SafePath.join('/base', ['subdir', 'file.txt']);
if (result.isOk) {
  print(result.path);
}

// Resolve within base directory
final resolved = SafePath.resolveWithin('/var/www', userPath);
if (resolved.isOk) {
  // Safe to use
}
```

### SafeEmail

Email validation:

```dart
// Simple validation
if (SafeEmail.isValid(email)) {
  // Valid
}

// Parse with error details
final result = SafeEmail.parse(email);
if (result.isOk) {
  print('Local: ${result.localPart}');
  print('Domain: ${result.domain}');
} else {
  print('Error: ${result.error}');
}

// Check for disposable emails
if (SafeEmail.isDisposable(email)) {
  print('Please use a non-disposable email');
}

// Normalize
final normalized = SafeEmail.normalize('User@EXAMPLE.COM');
// -> User@example.com
```

### SafeNetwork

IP address validation and classification:

```dart
// Parse IPv4
final ip = IPv4Address.parse('192.168.1.1');
if (ip != null) {
  print('Loopback: ${ip.isLoopback}');
  print('Private: ${ip.isPrivate}');
  print('Public: ${ip.isPublic}');
  print('Classification: ${ip.classification}');
}

// Validate port
if (SafeNetwork.isValidPort(8080)) {
  // OK
}

// SSRF protection
if (SafeNetwork.isPrivateUrl(url)) {
  throw SecurityException('Cannot access private URLs');
}
```

### SafeCrypto

Cryptographic operations:

```dart
// Secure random generation
final bytes = SafeCrypto.randomBytes(32);
final hex = SafeCrypto.randomHex(16);
final token = SafeCrypto.generateToken();

// Hashing
final hash = SafeCrypto.sha256Hash('data');
final hash512 = SafeCrypto.sha512Hash('data');

// HMAC
final mac = SafeCrypto.hmacSha256(key, message);
if (SafeCrypto.verifyHmacSha256(key, message, expectedMac)) {
  // Valid
}

// Constant-time comparison (timing attack prevention)
if (SafeCrypto.constantTimeEquals(actual, expected)) {
  // Match
}

// Key derivation
final derived = SafeCrypto.pbkdf2(password, salt, iterations: 100000);
```

## Flutter Support

This library works with Flutter for mobile, web, and desktop applications. Note that `SafePath` filesystem operations require `dart:io` and won't work on Flutter web.

## License

PMPL-1.0
