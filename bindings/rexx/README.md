# Proven for REXX

Safe, validated operations library for REXX applications on mainframes and distributed platforms.

## Overview

This binding provides access to the formally verified Proven safety library from REXX.
Since REXX cannot perform direct C FFI, all operations invoke the `proven-cli` binary
via system commands. Every function delegates to Idris 2 verified code through the
Zig FFI layer -- no logic is reimplemented in REXX.

## Prerequisites

- `proven-cli` must be available on `PATH` (built from `ffi/zig/`)
- Alternatively, set the `PROVEN_CLI` environment variable to the full path

## Compatibility

| Platform       | Interpreter     | Notes                           |
|----------------|----------------|---------------------------------|
| z/OS           | TSO/E REXX     | Uses ADDRESS SYSTEM             |
| Linux/Unix     | Regina REXX 3+ | Standard ADDRESS SYSTEM         |
| Linux/Unix     | ooRexx 4+      | Object-oriented extensions      |
| OS/2           | Classic REXX   | Legacy IBM platform             |

## Modules

| File              | Module      | Description                          |
|-------------------|-------------|--------------------------------------|
| `proven.rexx`     | Master      | All wrapper functions in one file    |
| `safe_math.rexx`  | SafeMath    | Overflow-checked arithmetic          |
| `safe_string.rexx`| SafeString  | XSS prevention, sanitization         |
| `safe_email.rexx` | SafeEmail   | RFC 5321 email validation            |
| `safe_url.rexx`   | SafeUrl     | URL parsing and validation           |
| `safe_path.rexx`  | SafePath    | Directory traversal prevention       |
| `safe_crypto.rexx`| SafeCrypto  | SHA-256, hex encode/decode, tokens   |
| `safe_json.rexx`  | SafeJson    | JSON validation and type checking    |

## Usage

### Loading a module

```rexx
/* Load all functions from the master module */
call proven.rexx

/* Or load individual modules */
call safe_math.rexx
call safe_email.rexx
```

### Safe arithmetic

```rexx
call safe_math.rexx

result = safe_add(2147483647, 1)
if result \= '' then
  say 'Sum:' result
else
  say 'Overflow detected'

result = safe_div(100, 0)
if result \= '' then
  say 'Quotient:' result
else
  say 'Division by zero prevented'
```

### Email validation

```rexx
call safe_email.rexx

addr = 'user@example.com'
if validate_email(addr) \= '' then
  say addr 'is valid'
else
  say addr 'is invalid'
```

### String sanitization

```rexx
call safe_string.rexx

user_input = '<script>alert("xss")</script>'
safe = escape_html(user_input)
say 'Safe HTML:' safe
```

### Path traversal detection

```rexx
call safe_path.rexx

path = '../../../etc/passwd'
if validate_path(path) = 'true' then
  say 'WARNING: directory traversal detected in:' path
```

### JSON validation

```rexx
call safe_json.rexx

json = '{"name": "test", "value": 42}'
if validate_json(json) \= '' then do
  say 'Valid JSON'
  say 'Type:' json_type(json)
end
```

### Cryptographic operations

```rexx
call safe_crypto.rexx

hash = hash_sha256('hello world')
say 'SHA-256:' hash

token = random_hex(16)
say 'Random token:' token

encoded = hex_encode('Hello')
say 'Hex:' encoded
decoded = hex_decode(encoded)
say 'Decoded:' decoded
```

### URL validation

```rexx
call safe_url.rexx

url = 'https://example.com/path?query=value'
result = validate_url(url)
if result \= '' then
  say 'Valid URL:' result
```

## Error Handling

All functions return an empty string (`''`) on error. This is idiomatic REXX
and allows simple conditional checks:

```rexx
result = safe_div(10, 0)
if result = '' then
  say 'Operation failed'
else
  say 'Result:' result
```

## Architecture

```
REXX Program
    |
    v
proven.rexx (thin wrapper)
    |
    v
proven-cli (binary)
    |
    v
Zig FFI layer
    |
    v
Idris 2 verified implementation
```

All computation happens in formally verified Idris 2 code. The REXX layer
is purely a command-line interface adapter with no logic of its own.

## Function Reference

### SafeMath

| Function           | Arguments      | Returns           |
|--------------------|----------------|-------------------|
| `safe_add`         | `a, b`         | Sum or `''`       |
| `safe_sub`         | `a, b`         | Difference or `''`|
| `safe_mul`         | `a, b`         | Product or `''`   |
| `safe_div`         | `a, b`         | Quotient or `''`  |
| `safe_mod`         | `a, b`         | Remainder or `''` |
| `safe_abs`         | `n`            | Absolute or `''`  |
| `safe_clamp`       | `lo, hi, val`  | Clamped or `''`   |
| `safe_pow`         | `base, exp`    | Power or `''`     |

### SafeString

| Function           | Arguments | Returns              |
|--------------------|-----------|----------------------|
| `sanitize_string`  | `input`   | Sanitized or `''`    |
| `escape_html`      | `input`   | Escaped or `''`      |
| `escape_sql`       | `input`   | Escaped or `''`      |
| `escape_js`        | `input`   | Escaped or `''`      |
| `is_valid_utf8`    | `input`   | `'true'`/`'false'`   |

### SafeEmail

| Function            | Arguments | Returns              |
|---------------------|-----------|----------------------|
| `validate_email`    | `addr`    | Result or `''`       |
| `parse_email`       | `addr`    | Parsed or `''`       |
| `normalize_email`   | `addr`    | Normalized or `''`   |

### SafePath

| Function            | Arguments     | Returns              |
|---------------------|---------------|----------------------|
| `validate_path`     | `path`        | `'true'`/`'false'`   |
| `sanitize_filename` | `name`        | Sanitized or `''`    |
| `safe_join`         | `base, child` | Joined or `''`       |

### SafeCrypto

| Function            | Arguments   | Returns              |
|---------------------|-------------|----------------------|
| `hash_sha256`       | `data`      | Hex hash or `''`     |
| `constant_time_eq`  | `a, b`      | `'true'`/`'false'`   |
| `random_hex`        | `nbytes`    | Hex string or `''`   |
| `hex_encode`        | `data`      | Hex string or `''`   |
| `hex_decode`        | `hexstr`    | Decoded or `''`      |

### SafeJson

| Function            | Arguments | Returns              |
|---------------------|-----------|----------------------|
| `validate_json`     | `str`     | `'true'`/`'false'`   |
| `json_type`         | `str`     | Type name or `''`    |

### SafeUrl

| Function            | Arguments | Returns              |
|---------------------|-----------|----------------------|
| `validate_url`      | `url`     | Parsed or `''`       |
| `is_https`          | `url`     | `'true'`/`'false'`   |

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
