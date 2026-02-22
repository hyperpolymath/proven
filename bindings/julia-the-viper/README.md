<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven Julia the Viper Bindings

Julia the Viper (JTV) language bindings for libproven. JTV is a snake-themed
Julia variant that separates programs into Control-plane (Turing-complete,
side-effectful) and Data-plane (addition-only, total). All computation is
performed in formally verified Idris 2 code through the Zig FFI bridge.

## Architecture

```
Julia the Viper (.jtv)
  | extern "C" FFI calls via Rust backend
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

| File                  | Purpose                                     |
|-----------------------|---------------------------------------------|
| `src/ffi.jtv`         | Raw FFI declarations (C ABI extern)         |
| `src/proven.jtv`      | Main module: lifecycle, error types, helpers |
| `src/safe_math.jtv`   | Checked integer arithmetic                  |
| `src/safe_string.jtv`  | String escaping (SQL, HTML, JS), UTF-8      |
| `src/safe_email.jtv`  | Email validation (RFC 5321/5322)            |
| `src/safe_url.jtv`    | URL parsing, path safety, filename sanitization |
| `src/safe_crypto.jtv`  | Crypto: constant-time eq, random, hex, CRC32 |
| `src/safe_json.jtv`   | JSON validation and type detection          |

## Data-plane vs Control-plane

JTV separates code into two planes:

- **Data-plane**: Addition-only, total functions. Cannot express control flow.
  Code injection is grammatically impossible. Pure validation (email, JSON,
  UTF-8) and safe_add live here.

- **Control-plane**: Turing-complete, side-effectful. FFI calls that allocate
  memory (string escaping, URL parsing, random bytes) live here. Extended
  arithmetic (subtraction, multiplication, division) is also Control-plane.

| Module         | Data-plane                | Control-plane                     |
|----------------|---------------------------|-----------------------------------|
| `safe_math`    | `safe_add`, `clamp`       | `safe_sub`, `safe_mul`, `safe_div`, etc. |
| `safe_string`  | `is_valid_utf8`           | `escape_sql`, `escape_html`, `escape_js` |
| `safe_email`   | `is_valid_email`          | `validate_email_str`              |
| `safe_url`     | `is_valid_url`            | `url_parse`, `url_free`, `sanitize_filename` |
| `safe_crypto`  | --                        | All (side-effectful)              |
| `safe_json`    | `is_valid_json`, `json_type` | `validate_json_str`            |

## Functions

### SafeMath

| Function                 | libproven Call                   | Plane    |
|--------------------------|----------------------------------|----------|
| `safe_add(a, b)`        | `proven_math_add_checked(a, b)` | Data     |
| `safe_sub(a, b)`        | `proven_math_sub_checked(a, b)` | Control  |
| `safe_mul(a, b)`        | `proven_math_mul_checked(a, b)` | Control  |
| `safe_div(a, b)`        | `proven_math_div(a, b)`         | Control  |
| `safe_mod(a, b)`        | `proven_math_mod(a, b)`         | Control  |
| `safe_abs(n)`           | `proven_math_abs_safe(n)`       | Control  |
| `clamp(v, lo, hi)`      | `proven_math_clamp(lo, hi, v)`  | Data     |
| `safe_pow(base, exp)`   | `proven_math_pow_checked(b, e)` | Control  |

### SafeString

| Function                   | libproven Call                      | Plane   |
|----------------------------|-------------------------------------|---------|
| `is_valid_utf8(ptr, len)`  | `proven_string_is_valid_utf8(p, l)` | Data    |
| `escape_sql(ptr, len)`     | `proven_string_escape_sql(p, l)`    | Control |
| `escape_html(ptr, len)`    | `proven_string_escape_html(p, l)`   | Control |
| `escape_js(ptr, len)`      | `proven_string_escape_js(p, l)`     | Control |

### Validation

| Function                        | libproven Call                    | Plane |
|---------------------------------|-----------------------------------|-------|
| `is_valid_email(ptr, len)`      | `proven_email_is_valid(p, l)`    | Data  |
| `is_valid_url(ptr, len)`        | `proven_url_parse(p, l)`         | Data  |
| `is_valid_json(ptr, len)`       | `proven_json_is_valid(p, l)`     | Data  |
| `has_path_traversal(ptr, len)`  | `proven_path_has_traversal(p, l)` | Data  |

### Crypto

| Function                                | libproven Call                          | Plane   |
|-----------------------------------------|-----------------------------------------|---------|
| `constant_time_eq(a, al, b, bl)`       | `proven_crypto_constant_time_eq(a,l,b,l)` | Control |
| `random_bytes(buf, len)`               | `proven_crypto_random_bytes(buf, len)`  | Control |
| `hex_encode(ptr, len)`                 | `proven_hex_encode(p, l, false)`        | Control |
| `hex_decode(ptr, len)`                 | `proven_hex_decode(p, l)`               | Control |
| `crc32(ptr, len)`                      | `proven_checksum_crc32(p, l)`           | Control |
| `verify_crc32(ptr, len, expected)`     | `proven_checksum_verify_crc32(p, l, e)` | Control |

## Usage

```jtv
import Proven (init, deinit)
import Proven.SafeMath (safe_add, safe_mul)
import Proven.SafeEmail (is_valid_email)
import Proven.SafeJson (is_valid_json, json_type_name)

-- Initialize the proven runtime
let status = init()
match status:
    Ok(_) -> proceed()
    Err(e) -> panic(e.message)

-- Data-plane: checked addition
let sum = safe_add(1000000000, 2000000000)
match sum:
    Ok(v) -> print("Sum: " + str(v))
    Err(e) -> print("Overflow: " + e.message)

-- Control-plane: email validation
let valid = is_valid_email("alice@example.com".ptr(), 17)
match valid:
    Ok(true) -> print("Valid email")
    Ok(false) -> print("Invalid email")
    Err(e) -> print("FFI error: " + e.message)

-- Shutdown
deinit()
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later
