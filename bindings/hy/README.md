<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-hy

Hy (Lisp-on-Python) bindings for **libproven** -- code that cannot crash.

All computation is performed by the formally verified Idris 2 core via the
Zig FFI bridge. These bindings are a thin ctypes wrapper and **never**
reimplement any logic.

## Requirements

- Python 3.9+
- [Hy](https://hylang.org/) >= 1.0
- `libproven.so` / `libproven.dylib` (built from `ffi/zig/`)

## Installation

```bash
pip install hy
pip install -e .
```

## Usage

```hy
(import proven.safe-math [safe-add safe-div safe-mul])

(print (safe-add 5 3))    ; => 8
(print (safe-div 10 0))   ; => None  (division by zero handled safely)
(print (safe-mul 6 7))    ; => 42

(import proven.safe-string [escape-html is-valid-utf8])

(print (escape-html "<script>alert('xss')</script>"))
; => "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

(import proven.safe-email [is-valid-email])

(print (is-valid-email "user@example.com"))  ; => True
(print (is-valid-email "not-an-email"))      ; => False

(import proven.safe-json [is-valid-json json-type JSON-OBJECT])

(print (is-valid-json "{\"key\": 1}"))  ; => True
(print (= (json-type "{\"key\": 1}") JSON-OBJECT))  ; => True
```

## Modules

| Module | Description |
|--------|-------------|
| `proven.ffi` | ctypes FFI declarations and library loading |
| `proven.safe_math` | Overflow/underflow-safe arithmetic |
| `proven.safe_string` | UTF-8 validation, SQL/HTML/JS escaping |
| `proven.safe_path` | Directory traversal prevention |
| `proven.safe_email` | RFC 5321 email validation |
| `proven.safe_url` | URL parsing into components |
| `proven.safe_crypto` | Constant-time comparison, secure random bytes |
| `proven.safe_json` | JSON validation and type detection |

## Architecture

```
Hy code (this binding)
  -> ctypes FFI calls
    -> libproven.so (Zig FFI bridge)
      -> Idris 2 formally verified core
```

## License

PMPL-1.0-or-later
