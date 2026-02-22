<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven VQL Bindings

VQL (Velociraptor Query Language) functions backed by libproven for digital
forensics and incident response. All computation is performed in formally
verified Idris 2 code through the Zig FFI bridge; the Go plugin uses CGo
to call the libproven C ABI.

## Architecture

```
VQL query (Velociraptor)
  |
  v
Go VQL plugin (proven_plugin.go)
  |
  v
CGo (C gateway)
  |
  v
libproven C ABI (proven.h)
  |
  v
Zig FFI bridge
  |
  v
Verified Idris 2 core (dependent types, totality checking)
```

## Files

| File                    | Purpose                                          |
|-------------------------|--------------------------------------------------|
| `proven_functions.vql`  | VQL function aliases for proven plugin functions  |
| `proven_plugin.go`      | Go plugin calling libproven via CGo               |
| `proven_queries.vql`    | Example proven-backed forensic queries            |

## Functions

### SafeMath

| VQL Function                    | libproven Call                   | Return     |
|---------------------------------|----------------------------------|------------|
| `proven_math_add(a, b)`        | `proven_math_add_checked(a, b)` | `int64`    |
| `proven_math_sub(a, b)`        | `proven_math_sub_checked(a, b)` | `int64`    |
| `proven_math_mul(a, b)`        | `proven_math_mul_checked(a, b)` | `int64`    |
| `proven_math_div(a, b)`        | `proven_math_div(a, b)`         | `int64`    |
| `proven_math_mod(a, b)`        | `proven_math_mod(a, b)`         | `int64`    |
| `proven_math_abs(n)`           | `proven_math_abs_safe(n)`       | `int64`    |
| `proven_math_clamp(v, lo, hi)` | `proven_math_clamp(lo, hi, v)`  | `int64`    |
| `proven_math_pow(base, exp)`   | `proven_math_pow_checked(b, e)` | `int64`    |

### Validation

| VQL Function                   | libproven Call                      | Return |
|--------------------------------|-------------------------------------|--------|
| `proven_validate_email(input)` | `proven_email_is_valid(ptr, len)`   | `bool` |
| `proven_validate_url(input)`   | `proven_url_parse(ptr, len)`        | `bool` |
| `proven_validate_json(input)`  | `proven_json_is_valid(ptr, len)`    | `bool` |
| `proven_validate_utf8(input)`  | `proven_string_is_valid_utf8(p, l)` | `bool` |

### String Operations

| VQL Function                        | libproven Call                      | Return   |
|-------------------------------------|-------------------------------------|----------|
| `proven_string_escape_html(input)`  | `proven_string_escape_html(p, l)`   | `string` |
| `proven_string_escape_sql(input)`   | `proven_string_escape_sql(p, l)`    | `string` |
| `proven_string_escape_js(input)`    | `proven_string_escape_js(p, l)`     | `string` |

### Crypto

| VQL Function                               | libproven Call                          | Return   |
|--------------------------------------------|-----------------------------------------|----------|
| `proven_crypto_hex_encode(input)`          | `proven_hex_encode(ptr, len, false)`    | `string` |
| `proven_crypto_crc32(input)`               | `proven_checksum_crc32(ptr, len)`       | `int64`  |
| `proven_crypto_verify_crc32(input, expected)` | `proven_checksum_verify_crc32(p,l,e)` | `bool`   |
| `proven_crypto_ct_eq(a, b)`               | `proven_crypto_constant_time_eq(a,l,b,l)` | `bool` |

## Build

```bash
CGO_LDFLAGS="-L/path/to/libproven -lproven" \
    go build -buildmode=plugin -o proven_plugin.so proven_plugin.go
```

## Deployment

1. Build the Go plugin with CGo linking to libproven
2. Place `proven_plugin.so` in Velociraptor's plugin directory
3. Ensure `libproven.so` is in the library search path (`LD_LIBRARY_PATH`)
4. Restart Velociraptor server to load the plugin
5. Use proven functions in VQL queries

## Use Cases

- **Safe arithmetic**: Prevent overflow in file size calculations, timestamp math
- **Evidence validation**: Validate emails, URLs, JSON found in forensic artifacts
- **String sanitization**: Escape evidence strings for safe display/storage
- **Integrity verification**: CRC32 checksums, constant-time hash comparison
- **Encoding**: Hex-encode binary evidence for display

## License

SPDX-License-Identifier: PMPL-1.0-or-later
