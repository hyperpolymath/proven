# proven-rs

Rust bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations for math, crypto, parsing, and validation.

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
proven = "0.1"
```

## Features

- **Safe Math**: Overflow detection, safe division, bounded integers
- **Safe Strings**: UTF-8 validation, injection-safe escaping (HTML, SQL, JS, URL, shell)
- **Safe JSON**: Exception-free parsing with validation
- **Safe URLs**: RFC 3986 compliant parsing
- **Safe Email**: RFC 5321/5322 validation
- **Safe Paths**: Traversal prevention
- **Safe Crypto**: Secure hashing stubs (use proper crypto libraries in production)
- **Safe Passwords**: Policy validation, strength analysis
- **Safe DateTime**: ISO 8601 parsing, date validation
- **Safe Network**: IPv4/IPv6 parsing, CIDR notation

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
```

## License

AGPL-3.0-or-later
