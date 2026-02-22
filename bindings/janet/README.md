# proven-janet - Janet Bindings for libproven

SPDX-License-Identifier: PMPL-1.0-or-later

Janet bindings for **libproven**, a formally verified safety library.
All computation is performed in Idris 2 via the Zig FFI layer. These
bindings are thin wrappers using Janet's `ffi/` module; they do NOT
reimplement any logic.

## Requirements

- Janet 1.31+ (for `ffi/native` and `ffi/defbind` support)
- libproven shared library (`libproven.so` / `libproven.dylib`)

## Installation

1. Build libproven from the repository root.
2. Ensure `libproven.so` is in your library path (`LD_LIBRARY_PATH`).
3. Install with `jpm`:

```sh
cd bindings/janet
jpm deps
jpm install
```

Or add to your `project.janet` dependencies:

```janet
(declare-project
  :dependencies [{:url "https://github.com/hyperpolymath/proven"
                  :tag "v0.9.0"}])
```

## Usage

```janet
(import proven)

# Initialize the runtime
(proven/init)

# Safe arithmetic
(proven/safe-math/add 1000000000000 2000000000000) # => 3000000000000
(proven/safe-math/div 10 0)                         # => nil

# String escaping
(proven/safe-string/escape-html "<script>alert('xss')</script>")
# => "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

# Email validation
(proven/safe-email/valid? "user@example.com") # => true
(proven/safe-email/valid? "not-an-email")     # => nil

# JSON validation
(proven/safe-json/valid? `{"key": "value"}`) # => true
(proven/safe-json/get-type "[1,2,3]")        # => :array

# Path safety
(proven/safe-path/has-traversal? "../../etc/passwd") # => true
(proven/safe-path/sanitize-filename "my<file>.txt")  # => "myfile.txt"

# Cryptographic operations
(proven/safe-crypto/constant-time-eq "secret" "secret") # => true
(proven/safe-crypto/hex-encode "hello")                  # => "68656c6c6f"

# URL operations
(proven/safe-url/url-encode "hello world") # => "hello%20world"

# Shut down
(proven/deinit)
```

## Modules

| File                       | Description                        |
|----------------------------|------------------------------------|
| `proven/init.janet`        | Main module, imports all submodules |
| `proven/ffi.janet`         | FFI declarations (ffi/native)      |
| `proven/safe-math.janet`   | Safe integer and float arithmetic   |
| `proven/safe-string.janet` | UTF-8 validation, SQL/HTML/JS escape|
| `proven/safe-path.janet`   | Traversal detection, sanitization   |
| `proven/safe-email.janet`  | RFC 5321 email validation           |
| `proven/safe-url.janet`    | URL parsing, encoding/decoding      |
| `proven/safe-crypto.janet` | Constant-time compare, random, hex  |
| `proven/safe-json.janet`   | JSON validation, type detection     |
| `project.janet`            | Package configuration               |

## Error Handling

All functions return `nil` on error. This matches Janet conventions for
safe failure: callers can use `(when ...)` or `(if-let ...)` to handle
the nil case.

## Architecture

```
proven/safe-*.janet  -->  proven/ffi.janet  -->  ffi/native  -->  libproven.so
  (Janet)                 (ffi/defbind)         (C ABI)         (Idris2 + Zig)
```

## Author

Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## License

PMPL-1.0-or-later
