# proven-fennel - Fennel Bindings for libproven

SPDX-License-Identifier: PMPL-1.0-or-later

Fennel (Lisp on Lua) bindings for **libproven**, a formally verified safety
library. All computation is performed in Idris 2 via the Zig FFI layer.
These bindings are thin wrappers using LuaJIT FFI; they do NOT reimplement
any logic.

## Requirements

- LuaJIT 2.1+ (for FFI support)
- Fennel compiler (any version)
- libproven shared library (`libproven.so` / `libproven.dylib`)

## Installation

1. Build libproven from the repository root.
2. Ensure `libproven.so` is in your library path (`LD_LIBRARY_PATH`).
3. Add the `proven/` directory to your Fennel/Lua module path.

## Usage

```fennel
(local proven (require :proven))

;; Initialize the runtime
(proven.init)

;; Safe arithmetic
(proven.SafeMath.add_checked 1000000000000 2000000000000) ;=> 3000000000000
(proven.SafeMath.div 10 0)                                 ;=> nil

;; String escaping
(proven.SafeString.escape_html "<script>alert('xss')</script>")
;=> "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

;; Email validation
(proven.SafeEmail.is_valid "user@example.com") ;=> true
(proven.SafeEmail.is_valid "not-an-email")     ;=> nil

;; JSON validation
(proven.SafeJson.is_valid "{\"key\": \"value\"}") ;=> true
(proven.SafeJson.get_type "[1,2,3]")               ;=> "array"

;; Path safety
(proven.SafePath.has_traversal "../../etc/passwd")     ;=> true
(proven.SafePath.sanitize_filename "my<file>.txt")     ;=> "myfile.txt"

;; Cryptographic operations
(proven.SafeCrypto.constant_time_eq "secret" "secret") ;=> true
(proven.SafeCrypto.hex_encode "hello")                  ;=> "68656c6c6f"

;; URL operations
(let [url (proven.SafeUrl.parse "https://example.com:8080/path?q=1")]
  (print url.scheme)  ;=> "https"
  (print url.host)    ;=> "example.com"
  (print url.port))   ;=> 8080

;; Shut down
(proven.deinit)
```

## Modules

| File                   | Description                        |
|------------------------|------------------------------------|
| `proven/init.fnl`      | Main module, loads all submodules   |
| `proven/ffi.fnl`       | LuaJIT FFI declarations            |
| `proven/safe-math.fnl` | Safe integer and float arithmetic   |
| `proven/safe-string.fnl` | UTF-8 validation, SQL/HTML/JS escape |
| `proven/safe-path.fnl` | Traversal detection, sanitization   |
| `proven/safe-email.fnl` | RFC 5321 email validation          |
| `proven/safe-url.fnl`  | URL parsing, encoding/decoding     |
| `proven/safe-crypto.fnl` | Constant-time compare, random, hex |
| `proven/safe-json.fnl` | JSON validation, type detection    |

## Error Handling

All functions return `nil` on error. This matches the Lua/Fennel convention
for safe failure.

## Architecture

```
proven/safe-*.fnl  -->  proven/ffi.fnl  -->  LuaJIT FFI  -->  libproven.so
  (Fennel)              (ffi.cdef)          (ffi.load)       (Idris2 + Zig)
```

## Author

Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## License

PMPL-1.0-or-later
