<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven-cljs

ClojureScript bindings for **libproven** -- code that cannot crash.

All computation is performed by the formally verified Idris 2 core via the
Zig FFI bridge. These bindings use JS interop to call the JavaScript/Deno
binding layer (transitive FFI) and **never** reimplement any logic.

## Architecture

```
ClojureScript (this binding)
  -> JS interop
    -> JavaScript/Deno binding
      -> libproven.so (Zig FFI bridge)
        -> Idris 2 formally verified core
```

## Requirements

- Java 11+ (for ClojureScript compilation)
- [shadow-cljs](https://shadow-cljs.github.io/) 2.28+
- Node.js 18+ (runtime target)
- `libproven.so` / `libproven.dylib` (built from `ffi/zig/`)
- The proven JS binding (from `bindings/deno/`)

## Setup

```bash
# Install shadow-cljs
npm install -g shadow-cljs

# Install deps
npm install

# Build
shadow-cljs compile lib

# Run tests
shadow-cljs compile test && node dist/test.js
```

## Usage

```clojure
(ns my-app.core
  (:require [proven.safe-math :as math]
            [proven.safe-string :as string]
            [proven.safe-email :as email]))

;; Safe arithmetic
(math/safe-add 5 3)    ;=> 8
(math/safe-div 10 0)   ;=> nil (division by zero handled safely)
(math/safe-mul 6 7)    ;=> 42

;; HTML escaping (XSS prevention)
(string/escape-html "<script>alert('xss')</script>")
;=> "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

;; Email validation
(email/valid-email? "user@example.com")  ;=> true
(email/valid-email? "not-an-email")      ;=> false

;; JSON validation
(require '[proven.safe-json :as json])
(json/valid-json? "{\"key\": 1}")  ;=> true
(json/json-type "42")              ;=> 2 (JSON-NUMBER)
```

## Modules

| Namespace | Description |
|-----------|-------------|
| `proven.ffi` | JS interop bridge and status codes |
| `proven.core` | Main entry point and re-exports |
| `proven.safe-math` | Overflow/underflow-safe arithmetic |
| `proven.safe-string` | UTF-8 validation, SQL/HTML/JS escaping |
| `proven.safe-path` | Directory traversal prevention |
| `proven.safe-email` | RFC 5321 email validation |
| `proven.safe-url` | URL parsing into components |
| `proven.safe-crypto` | Constant-time comparison, secure random |
| `proven.safe-json` | JSON validation and type detection |

## Error Handling

All functions return `nil` on error instead of throwing exceptions.
This is consistent with Clojure's nil-punning idiom:

```clojure
;; Use when-let for safe chaining
(when-let [sum (math/safe-add a b)]
  (println "Sum is" sum))

;; Use or for defaults
(or (math/safe-div 10 0) 0)  ;=> 0
```

## License

PMPL-1.0-or-later
