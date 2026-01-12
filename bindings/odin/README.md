# Proven for Odin

Safe, validated operations library for Odin applications.

## Installation

Copy the `proven` directory to your project or add as a collection:

```bash
# In your project
cp -r proven /path/to/your/project/
```

## Usage

```odin
package main

import "proven"
import "core:fmt"

main :: proc() {
    // Safe math with overflow checking
    result, ok := proven.safe_add(1000000000000, 2000000000000)
    if ok {
        fmt.printf("Sum: %d\n", result)
    } else {
        fmt.println("Overflow!")
    }

    // XSS prevention
    safe_html := proven.escape_html("<script>alert('xss')</script>")
    // "&lt;script&gt;alert('xss')&lt;/script&gt;"

    // Path safety
    if proven.has_traversal("../../../etc/passwd") {
        panic("Dangerous path!")
    }

    // Email validation
    email_result := proven.parse_email("user@example.com")
    if email_result.ok {
        fmt.printf("Local: %s, Domain: %s\n", email_result.local_part, email_result.domain)
    } else {
        fmt.printf("Invalid: %s\n", email_result.error)
    }

    // IP classification
    ip, ip_ok := proven.parse_ipv4("192.168.1.1")
    if ip_ok {
        fmt.printf("Private: %v\n", proven.is_private_ip(ip))
        fmt.printf("Classification: %v\n", proven.classify_ip(ip))
    }

    // Secure tokens
    token := proven.generate_token_default()
    fmt.printf("CSRF token: %s\n", token)
}
```

## Modules

### SafeMath

Overflow-checked arithmetic with multiple return values:

```odin
import "proven"

// All operations return (result, ok)
result, ok := proven.safe_add(a, b)
result, ok := proven.safe_sub(a, b)
result, ok := proven.safe_mul(a, b)
result, ok := proven.safe_div(a, b)

// Check for overflow
if result, ok := proven.safe_mul(big_number, another_big); ok {
    fmt.println(result)
} else {
    fmt.println("Overflow!")
}

// Slice operations
proven.safe_sum([]i64{1, 2, 3, 4, 5})   // (15, true)
proven.safe_product([]i64{2, 3, 4})      // (24, true)
```

### SafeString

XSS prevention and string sanitization:

```odin
import "proven"

proven.escape_html("<script>")       // "&lt;script&gt;"
proven.escape_sql("O'Brien")         // "O''Brien"
proven.escape_js(user_input)
proven.url_encode("hello world")     // "hello%20world"
proven.sanitize_default(input)       // Only alphanumeric + _-
proven.slugify("Hello World!")       // "hello-world"
```

### SafePath

Directory traversal protection:

```odin
import "proven"

// Check for traversal
if proven.has_traversal(user_path) {
    panic("Path traversal detected")
}

// Sanitize filename
proven.sanitize_filename("../../../etc/passwd")  // "etc_passwd"

// Safe path joining
result := proven.path_join("/base", "subdir", "file.txt")
if result.ok {
    fmt.println(result.path)
} else {
    fmt.printf("Error: %s\n", result.error)
}
```

### SafeEmail

Email validation with result structs:

```odin
import "proven"

// Simple validation
if proven.is_valid_email(email) {
    // Valid format
}

// Parse with result checking
result := proven.parse_email(email)
if result.ok {
    fmt.printf("Local: %s, Domain: %s\n", result.local_part, result.domain)
} else {
    fmt.printf("Error: %s\n", result.error)
}

// Check for disposable emails
proven.is_disposable_email("user@mailinator.com")  // true

// Normalize
normalized, ok := proven.normalize_email("User@EXAMPLE.COM")
// "User@example.com"
```

### SafeNetwork

IP address validation and classification:

```odin
import "proven"

// Parse IPv4
ip, ok := proven.parse_ipv4("192.168.1.1")
if ok {
    fmt.printf("Loopback: %v\n", proven.is_loopback(ip))
    fmt.printf("Private: %v\n", proven.is_private_ip(ip))

    #partial switch proven.classify_ip(ip) {
    case .Private:
        fmt.println("Private network")
    case .Loopback:
        fmt.println("Localhost")
    case .Public:
        fmt.println("Public IP")
    }
}

// Validate port
proven.is_valid_port(8080)  // true

// SSRF protection
if proven.is_private_url(url) {
    panic("Cannot access private URLs")
}
```

### SafeCrypto

Cryptographic operations:

```odin
import "proven"

// Secure random generation
proven.random_bytes(32)
proven.random_hex(16)
proven.generate_token_default()

// Hashing
proven.sha256_hash("data")
proven.sha512_hash("data")

// Constant-time comparison (timing attack prevention)
proven.constant_time_equals_string(actual, expected)

// Password generation
proven.generate_password_default()
```

## License

PMPL-1.0
