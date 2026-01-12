# Proven C Bindings

C header for the Proven library (implemented in Zig).

## Installation

1. Build the Zig library:
```bash
cd ffi/zig
zig build
```

2. Copy header and library:
```bash
# Header
cp bindings/c/include/proven.h /usr/local/include/

# Library (choose one)
cp ffi/zig/zig-out/lib/libproven.so /usr/local/lib/  # Dynamic
cp ffi/zig/zig-out/lib/libproven.a /usr/local/lib/   # Static
```

3. Update library cache (Linux):
```bash
sudo ldconfig
```

## Usage

```c
#include <proven.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    // Initialize runtime
    if (proven_init() != PROVEN_OK) {
        fprintf(stderr, "Failed to initialize Proven\n");
        return 1;
    }

    // Safe division - no crash on divide by zero
    ProvenIntResult div_result = proven_math_div(10, 0);
    if (PROVEN_FAILED(div_result)) {
        printf("Division by zero handled: status=%d\n", div_result.status);
    }

    // Overflow-checked arithmetic
    ProvenIntResult add_result = proven_math_add_checked(INT64_MAX, 1);
    if (add_result.status == PROVEN_ERR_OVERFLOW) {
        printf("Overflow detected\n");
    }

    // HTML escaping for XSS prevention
    const char* unsafe = "<script>alert(1)</script>";
    ProvenStringResult html = proven_string_escape_html(
        (const uint8_t*)unsafe, strlen(unsafe)
    );
    if (PROVEN_SUCCEEDED(html)) {
        printf("Safe HTML: %s\n", html.value);
        proven_free_string(html.value);  // Don't forget to free!
    }

    // Path traversal detection
    const char* path = "../../../etc/passwd";
    ProvenBoolResult traversal = proven_path_has_traversal(
        (const uint8_t*)path, strlen(path)
    );
    if (traversal.value) {
        printf("Path traversal attack detected!\n");
    }

    // Email validation
    const char* email = "user@example.com";
    ProvenBoolResult valid = proven_email_is_valid(
        (const uint8_t*)email, strlen(email)
    );
    printf("Email valid: %s\n", valid.value ? "yes" : "no");

    // IP classification
    const char* ip = "192.168.1.1";
    ProvenIPv4Result ip_result = proven_network_parse_ipv4(
        (const uint8_t*)ip, strlen(ip)
    );
    if (PROVEN_SUCCEEDED(ip_result)) {
        if (proven_network_ipv4_is_private(ip_result.address)) {
            printf("Private IP address\n");
        }
    }

    // Constant-time comparison (timing attack prevention)
    const char* secret = "correct_token";
    const char* input = "user_input";
    ProvenBoolResult eq = proven_crypto_constant_time_eq(
        (const uint8_t*)secret, strlen(secret),
        (const uint8_t*)input, strlen(input)
    );
    // Result in eq.value - comparison is timing-safe

    // Cleanup
    proven_deinit();
    return 0;
}
```

## Compilation

```bash
# Dynamic linking
gcc -o myapp myapp.c -lproven

# Static linking
gcc -o myapp myapp.c -lproven -static

# With explicit paths
gcc -o myapp myapp.c -I/path/to/proven/bindings/c/include \
    -L/path/to/proven/ffi/zig/zig-out/lib -lproven
```

## Memory Management

- String results (`ProvenStringResult`) must be freed with `proven_free_string()`
- URL results must be freed with `proven_url_free()`
- Integer and boolean results don't require freeing

## Thread Safety

The Proven library is thread-safe. Multiple threads can call functions
concurrently after `proven_init()` is called once.

## Error Handling

All functions return a status code. Check with:

```c
if (PROVEN_SUCCEEDED(result)) {
    // Use result.value
} else {
    // Handle error based on result.status
}
```

## Status Codes

| Code | Meaning |
|------|---------|
| `PROVEN_OK` | Success |
| `PROVEN_ERR_NULL_POINTER` | Null pointer passed |
| `PROVEN_ERR_INVALID_ARGUMENT` | Invalid argument |
| `PROVEN_ERR_OVERFLOW` | Integer overflow |
| `PROVEN_ERR_UNDERFLOW` | Integer underflow |
| `PROVEN_ERR_DIVISION_BY_ZERO` | Division by zero |
| `PROVEN_ERR_PARSE_FAILURE` | Parse error |
| `PROVEN_ERR_VALIDATION_FAILED` | Validation failed |
| `PROVEN_ERR_OUT_OF_BOUNDS` | Index out of bounds |
| `PROVEN_ERR_ENCODING_ERROR` | UTF-8 encoding error |
| `PROVEN_ERR_ALLOCATION_FAILED` | Memory allocation failed |

## License

PMPL-1.0
