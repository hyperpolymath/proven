<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven Orc Bindings

[Orc](https://orc.csres.utexas.edu/) language bindings for libproven. All
computation is performed in formally verified Idris 2 code through the Zig FFI
bridge; these bindings use Orc's `import site` mechanism to call JNA (Java
Native Access) which maps to the libproven C ABI.

## Architecture

```
Orc (.orc) concurrent orchestration
  | import site -> Java class (JNA)
  v
JNA (com.sun.jna)
  | JNA -> C ABI call
  v
libproven C ABI (proven.h)
  | C ABI
  v
Zig FFI bridge
  | Zig -> Idris2 RefC calls
  v
Verified Idris 2 core (dependent types, totality checking)
```

## Files

| File                    | Purpose                                        |
|-------------------------|------------------------------------------------|
| `src/ffi.orc`           | Raw FFI declarations via JNA bridge            |
| `src/proven.orc`        | Main module: lifecycle, error handling, helpers |
| `src/safe_math.orc`     | Checked integer arithmetic                     |
| `src/safe_string.orc`   | String escaping (SQL, HTML, JS)                |
| `src/safe_email.orc`    | Email validation (RFC 5321/5322)               |
| `src/safe_url.orc`      | URL parsing and validation                     |
| `src/safe_crypto.orc`   | Crypto: constant-time eq, random, hex, CRC32   |
| `src/safe_json.orc`     | JSON validation and type detection             |

## Functions

### SafeMath

| Function               | libproven Call                   | Description                  |
|------------------------|----------------------------------|------------------------------|
| `safe_add(a, b)`       | `proven_math_add_checked(a, b)` | Checked addition             |
| `safe_sub(a, b)`       | `proven_math_sub_checked(a, b)` | Checked subtraction          |
| `safe_mul(a, b)`       | `proven_math_mul_checked(a, b)` | Checked multiplication       |
| `safe_div(a, b)`       | `proven_math_div(a, b)`         | Safe division                |
| `safe_mod(a, b)`       | `proven_math_mod(a, b)`         | Safe modulo                  |
| `safe_abs(n)`          | `proven_math_abs_safe(n)`       | Safe absolute value          |
| `clamp(v, lo, hi)`     | `proven_math_clamp(lo, hi, v)`  | Clamp to range               |
| `safe_pow(base, exp)`  | `proven_math_pow_checked(b, e)` | Checked exponentiation       |

### SafeString

| Function            | libproven Call                      | Description              |
|---------------------|-------------------------------------|--------------------------|
| `is_valid_utf8(s)`  | `proven_string_is_valid_utf8(p, l)` | UTF-8 validation         |
| `escape_sql(s)`     | `proven_string_escape_sql(p, l)`    | SQL injection prevention |
| `escape_html(s)`    | `proven_string_escape_html(p, l)`   | XSS prevention           |
| `escape_js(s)`      | `proven_string_escape_js(p, l)`     | JS string escaping       |

### Validation

| Function             | libproven Call                    | Description            |
|----------------------|-----------------------------------|------------------------|
| `is_valid_email(s)`  | `proven_email_is_valid(p, l)`     | RFC 5321 email check   |
| `is_valid_url(s)`    | `proven_url_parse(p, l)`          | RFC 3986 URL check     |
| `is_valid_json(s)`   | `proven_json_is_valid(p, l)`      | JSON document check    |

### Crypto

| Function                      | libproven Call                           | Description             |
|-------------------------------|------------------------------------------|-------------------------|
| `constant_time_eq(a, b)`     | `proven_crypto_constant_time_eq(a,l,b,l)` | Timing-safe compare    |
| `random_bytes(len)`          | `proven_crypto_random_bytes(buf, len)`   | Secure random bytes     |
| `hex_encode(s)`              | `proven_hex_encode(p, l, false)`         | Hex encoding            |
| `hex_decode(s)`              | `proven_hex_decode(p, l)`                | Hex decoding            |
| `crc32(s)`                   | `proven_checksum_crc32(p, l)`            | CRC32 checksum          |
| `verify_crc32(s, expected)`  | `proven_checksum_verify_crc32(p, l, e)`  | CRC32 verification      |

## Prerequisites

1. Orc runtime (https://orc.csres.utexas.edu/)
2. JNA (`com.sun.jna`) on the classpath
3. `libproven.so` (or `libproven.dylib` / `proven.dll`) in the JNA library path
4. The `proven.jna` Java package providing the JNA interface declarations

## Usage

```orc
include "src/proven.orc"
include "src/safe_math.orc"
include "src/safe_email.orc"

-- Initialize the proven runtime
init()

-- Checked arithmetic
>safe_add(1000000000000, 2000000000000)>
  Println(result)

-- Concurrent email validation
val emails = ["alice@example.com", "invalid@@", "bob@test.org"]
validate_emails(emails) >(email, valid)>
  Println(email + ": " + valid)
```

## Concurrency

Orc's key strength is concurrent orchestration. All proven functions can be
composed using Orc's concurrency operators:

- `|` (parallel): Run multiple proven calls concurrently
- `>x>` (sequential): Chain results through proven calls
- Pruning: Race multiple approaches, take the first result

```orc
-- Compute add and mul concurrently
(safe_add(a, b) | safe_mul(a, b)) >result>
  Println(result)
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later
