# Proven C++ Bindings

Modern C++ wrapper for the Proven library (Zig implementation).

## Features

- RAII memory management (no manual `free()` calls)
- `std::optional` returns instead of error codes
- `std::string_view` for zero-copy string passing
- Exception-based error handling (optional)
- Header-only - just include and use

## Installation

1. Build the Zig library:
```bash
cd ffi/zig
zig build
```

2. Add include paths:
```bash
-I/path/to/proven/bindings/cpp/include
-I/path/to/proven/bindings/c/include
```

3. Link against libproven:
```bash
-lproven
```

## Usage

```cpp
#include <proven/proven.hpp>
#include <iostream>

int main() {
    // RAII runtime - automatically initializes and cleans up
    proven::Runtime runtime;

    // Safe math with optional returns
    auto result = proven::SafeMath::div(10, 0);
    if (!result) {
        std::cout << "Division by zero safely handled\n";
    }

    // With default value
    int64_t val = proven::SafeMath::div_or(42, 10, 0);  // returns 42

    // Overflow detection
    auto sum = proven::SafeMath::add_checked(INT64_MAX, 1);
    if (!sum) {
        std::cout << "Overflow detected\n";
    }

    // String escaping
    auto html = proven::SafeString::escape_html("<script>evil</script>");
    if (html) {
        std::cout << "Safe HTML: " << *html << "\n";
    }

    // Path traversal detection
    if (proven::SafePath::has_traversal("../../../etc/passwd")) {
        std::cout << "Attack detected!\n";
    }

    // Email validation
    if (proven::SafeEmail::is_valid("user@example.com")) {
        std::cout << "Valid email\n";
    }

    // IP address parsing and classification
    auto ip = proven::SafeNetwork::parse_ipv4("192.168.1.1");
    if (ip && ip->is_private()) {
        std::cout << "Private IP: " << ip->to_string() << "\n";
    }

    // Cryptographic operations
    auto bytes = proven::SafeCrypto::random_bytes(32);
    if (bytes) {
        // Use random bytes...
    }

    // Constant-time comparison (timing-safe)
    if (proven::SafeCrypto::constant_time_compare(secret, input)) {
        // Tokens match
    }

    // Version info
    auto version = proven::Version::get();
    std::cout << "Proven v" << version.to_string() << "\n";

    return 0;
}  // Runtime automatically cleaned up
```

## Compilation

```bash
# C++17 or later required
g++ -std=c++17 -o myapp myapp.cpp \
    -I/path/to/proven/bindings/cpp/include \
    -I/path/to/proven/bindings/c/include \
    -L/path/to/proven/ffi/zig/zig-out/lib \
    -lproven

# Or with pkg-config (if configured)
g++ -std=c++17 -o myapp myapp.cpp $(pkg-config --cflags --libs proven)
```

## API Overview

### Runtime Management

```cpp
proven::Runtime runtime;           // RAII initialization
proven::Runtime::is_initialized(); // Check status
proven::Runtime::abi_version();    // Get ABI version
```

### SafeMath

```cpp
proven::SafeMath::div(a, b)         // -> std::optional<int64_t>
proven::SafeMath::div_or(def, a, b) // -> int64_t
proven::SafeMath::mod(a, b)         // -> std::optional<int64_t>
proven::SafeMath::add_checked(a, b) // -> std::optional<int64_t>
proven::SafeMath::sub_checked(a, b) // -> std::optional<int64_t>
proven::SafeMath::mul_checked(a, b) // -> std::optional<int64_t>
proven::SafeMath::abs_safe(n)       // -> std::optional<int64_t>
proven::SafeMath::clamp(lo, hi, v)  // -> int64_t
proven::SafeMath::pow_checked(b, e) // -> std::optional<int64_t>
```

### SafeString

```cpp
proven::SafeString::is_valid_utf8(data)  // -> bool
proven::SafeString::escape_sql(str)      // -> std::optional<std::string>
proven::SafeString::escape_html(str)     // -> std::optional<std::string>
proven::SafeString::escape_js(str)       // -> std::optional<std::string>
```

### SafePath

```cpp
proven::SafePath::has_traversal(path)      // -> bool
proven::SafePath::is_safe(path)            // -> bool
proven::SafePath::sanitize_filename(name)  // -> std::optional<std::string>
```

### SafeEmail

```cpp
proven::SafeEmail::is_valid(email)  // -> bool
```

### SafeNetwork

```cpp
proven::SafeNetwork::parse_ipv4(str)    // -> std::optional<IPv4Address>
proven::SafeNetwork::is_valid_ipv4(str) // -> bool

// IPv4Address methods
ip[0], ip[1], ip[2], ip[3]  // Access octets
ip.is_private()              // RFC 1918 check
ip.is_loopback()             // 127.0.0.0/8 check
ip.to_string()               // "192.168.1.1"
```

### SafeCrypto

```cpp
proven::SafeCrypto::constant_time_compare(a, b)  // -> bool
proven::SafeCrypto::random_bytes(count)          // -> std::optional<std::string>
proven::SafeCrypto::random_bytes_into(buf, len)  // -> bool
```

## Exception Handling

The `proven::Error` exception is thrown only during runtime initialization
if it fails. All other operations return `std::optional` or `bool`.

```cpp
try {
    proven::Runtime runtime;
    // ...
} catch (const proven::Error& e) {
    std::cerr << "Proven error: " << e.what() << "\n";
    std::cerr << "Status: " << static_cast<int>(e.status()) << "\n";
}
```

## Requirements

- C++17 or later
- Proven library (libproven.so or libproven.a)

## License

PMPL-1.0
