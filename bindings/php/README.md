# Proven PHP

Code that cannot crash - safe arithmetic, string escaping, path validation, and more.

## Installation

```bash
composer require hyperpolymath/proven
```

## Usage

```php
<?php
use Proven\SafeMath;
use Proven\SafeString;
use Proven\SafePath;
use Proven\SafeEmail;
use Proven\SafeNetwork;
use Proven\SafeCrypto;

// Safe division - returns null instead of crashing
$result = SafeMath::div(10, 0);  // null, not an exception

// Overflow-checked arithmetic
$sum = SafeMath::addChecked(PHP_INT_MAX, 1);  // null (would overflow)

// XSS prevention
$safe = SafeString::escapeHtml('<script>alert(1)</script>');
// &lt;script&gt;alert(1)&lt;/script&gt;

// SQL injection prevention (prefer parameterized queries!)
$escaped = SafeString::escapeSql("O'Brien");  // O''Brien

// Path traversal prevention
if (SafePath::hasTraversal($userInput)) {
    die('Nice try');
}
$safePath = SafePath::safeJoin('/uploads', $filename);

// Email validation
if (SafeEmail::isValid($email)) {
    $domain = SafeEmail::getDomain($email);
}

// IP classification
if (SafeNetwork::isPrivate($ip)) {
    // Internal network
}

// Constant-time comparison (timing attack prevention)
if (SafeCrypto::constantTimeCompare($token, $expected)) {
    // Valid
}

// Secure password hashing
$hash = SafeCrypto::hashPassword($password);
if (SafeCrypto::verifyPassword($input, $hash)) {
    // Authenticated
}
```

## Modules

- **SafeMath**: Overflow/underflow-safe arithmetic
- **SafeString**: HTML, SQL, JS, URL escaping and UTF-8 validation
- **SafePath**: Directory traversal prevention and filename sanitization
- **SafeEmail**: Email validation, parsing, and normalization
- **SafeNetwork**: IPv4/IPv6 validation and classification
- **SafeCrypto**: Constant-time comparison, secure random, password hashing

## Requirements

- PHP 8.1+
- mbstring extension

## License

PMPL-1.0
