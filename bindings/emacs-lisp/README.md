# proven.el - Emacs Lisp Bindings for libproven

SPDX-License-Identifier: PMPL-1.0-or-later

Emacs Lisp bindings for **libproven**, a formally verified safety library.
All computation is performed in Idris 2 via the Zig FFI layer. These bindings
are thin wrappers that call libproven via Emacs dynamic modules; they do NOT
reimplement any logic.

## Requirements

- Emacs 28+ compiled with dynamic module support (`--with-modules`)
- libproven shared library (`libproven.so` / `libproven.dylib`)
- Compiled dynamic module (`proven-module.so` / `proven-module.dylib`)

## Installation

1. Build libproven from the repository root.
2. Compile the Emacs dynamic module (see `proven-module.c` in the build output).
3. Place `proven-module.so` alongside `proven.el` or in `~/.emacs.d/proven/`.
4. Add the binding directory to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/proven/bindings/emacs-lisp/")
(require 'proven)
```

## Usage

```elisp
;; Initialize the runtime
(proven-init)

;; Safe arithmetic (returns nil instead of crashing)
(proven-safe-math-add 1000000000000 2000000000000) ;=> 3000000000000
(proven-safe-math-div 10 0)                         ;=> nil

;; String escaping
(proven-safe-string-escape-html "<script>alert('xss')</script>")
;=> "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

;; Email validation
(proven-safe-email-valid-p "user@example.com")   ;=> t
(proven-safe-email-valid-p "not-an-email")       ;=> nil

;; JSON validation
(proven-safe-json-valid-p "{\"key\": \"value\"}") ;=> t
(proven-safe-json-get-type "[1,2,3]")             ;=> array

;; Path safety
(proven-safe-path-has-traversal-p "../../etc/passwd") ;=> t
(proven-safe-path-sanitize-filename "my<file>.txt")   ;=> "myfile.txt"

;; Cryptographic operations
(proven-safe-crypto-constant-time-eq "secret" "secret") ;=> t
(proven-safe-crypto-hex-encode "hello")                  ;=> "68656c6c6f"

;; Shut down when done
(proven-deinit)
```

## Modules

| File                    | Description                        |
|-------------------------|------------------------------------|
| `proven.el`             | Main package, loads all submodules  |
| `proven-ffi.el`         | FFI declarations, module loading    |
| `proven-safe-math.el`   | Safe integer and float arithmetic   |
| `proven-safe-string.el` | UTF-8 validation, SQL/HTML/JS escape|
| `proven-safe-path.el`   | Traversal detection, sanitization   |
| `proven-safe-email.el`  | RFC 5321 email validation           |
| `proven-safe-url.el`    | URL parsing, encoding/decoding      |
| `proven-safe-crypto.el` | Constant-time compare, random, hex  |
| `proven-safe-json.el`   | JSON validation, type detection     |

## Error Handling

All functions return `nil` on error. This is intentional: proven is about
total safety, and `nil` propagation is the idiomatic Emacs Lisp pattern for
safe failure. No signals are raised.

## Architecture

```
proven-safe-*.el  -->  proven-ffi.el  -->  proven-module.so  -->  libproven.so
  (Emacs Lisp)        (module-load)       (Emacs C module)      (Idris2 + Zig)
```

## Author

Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## License

PMPL-1.0-or-later
