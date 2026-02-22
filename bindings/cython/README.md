<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-cython

Cython bindings for **libproven** -- code that cannot crash.

All computation is performed by the formally verified Idris 2 core via the
Zig FFI bridge. These bindings compile to native C extensions that call
`libproven` directly and **never** reimplement any logic.

## Architecture

```
Cython code (this binding)
  -> direct C function calls (cimport)
    -> libproven.so (Zig FFI bridge)
      -> Idris 2 formally verified core
```

Unlike the Python ctypes binding, Cython compiles to native C extensions
with zero Python overhead in the FFI call path. The `nogil` directive
releases the GIL during C calls for thread safety.

## Requirements

- Python 3.9+
- Cython >= 3.0
- `libproven.so` / `libproven.dylib` (built from `ffi/zig/`)
- `proven.h` header (from `bindings/c/include/`)

## Build

```bash
# Build in-place for development
python setup.py build_ext --inplace

# Or install as editable
pip install Cython>=3.0
pip install -e .
```

## Usage

```python
from proven.safe_math import safe_add, safe_div, safe_mul, clamp

print(safe_add(5, 3))    # => 8
print(safe_div(10, 0))   # => None  (division by zero handled safely)
print(safe_mul(6, 7))    # => 42
print(clamp(0, 100, 150))  # => 100

from proven.safe_string import escape_html, is_valid_utf8

print(escape_html("<script>alert('xss')</script>"))
# => "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

from proven.safe_email import is_valid_email

print(is_valid_email("user@example.com"))  # => True
print(is_valid_email("not-an-email"))      # => False

from proven.safe_json import is_valid_json, json_type, JSON_OBJECT

print(is_valid_json('{"key": 1}'))  # => True
print(json_type('{"key": 1}') == JSON_OBJECT)  # => True

from proven.safe_datetime import parse_datetime, is_leap_year

dt = parse_datetime("2026-01-15T10:30:00Z")
print(dt["year"], dt["month"])  # => 2026 1
print(is_leap_year(2024))  # => True
```

## Modules

| Module | Description |
|--------|-------------|
| `proven` | Init/deinit, version info, status constants |
| `proven.safe_math` | Overflow/underflow-safe arithmetic |
| `proven.safe_string` | UTF-8 validation, SQL/HTML/JS escaping |
| `proven.safe_path` | Directory traversal prevention |
| `proven.safe_email` | RFC 5321 email validation |
| `proven.safe_url` | URL parsing into components |
| `proven.safe_crypto` | Constant-time comparison, secure random bytes |
| `proven.safe_json` | JSON validation and type detection |
| `proven.safe_datetime` | ISO 8601 date/time parsing and formatting |

## Performance

Cython extensions compile to native C code, so FFI overhead is minimal
compared to Python's ctypes or cffi. The `nogil` directive releases the
GIL during C calls, enabling true multi-threaded parallelism.

## License

PMPL-1.0-or-later
