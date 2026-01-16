# proven - Guile Scheme Bindings

> Code that cannot crash. Mathematically proven safe operations for Guile Scheme.

## Installation

Add the `proven/` directory to your Guile load path:

```bash
export GUILE_LOAD_PATH="/path/to/proven/bindings/guile:$GUILE_LOAD_PATH"
```

Or in Guile:

```scheme
(add-to-load-path "/path/to/proven/bindings/guile")
```

## Usage

### Import the full library

```scheme
(use-modules (proven))
```

### Or import specific modules

```scheme
(use-modules (proven safe-math))
(use-modules (proven safe-string))
(use-modules (proven safe-path))
```

## Modules

### SafeMath - Overflow-checked Arithmetic

All operations return an alist with `'value` and `'ok` keys.

```scheme
(use-modules (proven safe-math))

;; Safe division
(safe-div 10 2)    ; => ((value . 5) (ok . #t))
(safe-div 10 0)    ; => ((value . 0) (ok . #f))

;; Safe addition
(safe-add 5 3)     ; => ((value . 8) (ok . #t))

;; Result accessors
(define result (safe-div 10 2))
(result-value result)  ; => 5
(result-ok? result)    ; => #t

;; Clamp and range checks
(clamp 15 0 10)    ; => 10
(in-range? 5 0 10) ; => #t
```

### SafeString - XSS and Injection Prevention

```scheme
(use-modules (proven safe-string))

;; Escape HTML
(escape-html "<script>alert('xss')</script>")
; => "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

;; Escape SQL
(escape-sql "'; DROP TABLE users;--")
; => "''; DROP TABLE users;--"

;; Escape JavaScript
(escape-js "alert(\"hello\")")
; => "alert(\\\"hello\\\")"

;; Sanitize to safe characters
(sanitize "hello<script>world")
; => "helloscriptworld"

;; URL-safe slug
(slugify "Hello World!")
; => "hello-world"
```

### SafePath - Path Traversal Prevention

```scheme
(use-modules (proven safe-path))

;; Validate paths
(path-validate "../../../etc/passwd")
; => ((path . "") (ok . #f) (error . "Path traversal detected"))

(path-validate "user-file.txt")
; => ((path . "user-file.txt") (ok . #t) (error . ""))

;; Safe path joining
(path-join-safe "/var/uploads" "../../../etc/passwd")
; => ((path . "") (ok . #f) (error . "Path traversal detected"))

(path-join-safe "/var/uploads" "document.pdf")
; => ((path . "/var/uploads/document.pdf") (ok . #t) (error . ""))

;; Path utilities
(path-extension "file.tar.gz")  ; => "gz"
(path-basename "/path/to/file") ; => "file"
(path-dirname "/path/to/file")  ; => "/path/to"
```

### SafeEmail - RFC 5321 Email Validation

```scheme
(use-modules (proven safe-email))

;; Validate email
(email-valid? "user@example.com")  ; => #t
(email-valid? "not-an-email")      ; => #f

;; Parse email
(email-parse "User@Example.COM")
; => ((local . "User") (domain . "Example.COM") (ok . #t) (error . ""))

;; Normalize (lowercase domain)
(email-normalize "User@Example.COM")
; => "User@example.com"

;; Extract parts
(email-get-domain "user@example.com")  ; => "example.com"
(email-get-local "user@example.com")   ; => "user"
```

### SafeNetwork - Network Primitive Validation

```scheme
(use-modules (proven safe-network))

;; IPv4 validation
(ipv4-valid? "192.168.1.1")  ; => #t (match object)
(ipv4-valid? "999.999.999")  ; => #f

;; Port validation
(port-valid? 8080)   ; => #t
(port-valid? 70000)  ; => #f

;; Hostname validation
(hostname-valid? "example.com")  ; => #t

;; CIDR parsing
(cidr-parse "192.168.1.0/24")
; => ((ip . "192.168.1.0") (prefix . 24) (ok . #t))

;; Private/loopback detection
(ip-private? "192.168.1.1")  ; => #t
(ip-loopback? "127.0.0.1")   ; => #t
```

### SafeUuid - RFC 4122 UUID Validation

```scheme
(use-modules (proven safe-uuid))

;; Validate UUID
(uuid-valid? "550e8400-e29b-41d4-a716-446655440000")  ; => #t
(uuid-valid-v4? "550e8400-e29b-41d4-a716-446655440000")  ; => #t

;; Get version
(uuid-version "550e8400-e29b-41d4-a716-446655440000")  ; => 4

;; Normalize
(uuid-normalize "550E8400-E29B-41D4-A716-446655440000")
; => "550e8400-e29b-41d4-a716-446655440000"

;; Parse into components
(uuid-parse "550e8400-e29b-41d4-a716-446655440000")

;; Convert to/from hex
(uuid-to-hex "550e8400-e29b-41d4-a716-446655440000")
; => "550e8400e29b41d4a716446655440000"

(uuid-from-hex "550e8400e29b41d4a716446655440000")
; => ((uuid . "550e8400-e29b-41d4-a716-446655440000") (ok . #t))
```

### SafeHex - Hexadecimal Validation

```scheme
(use-modules (proven safe-hex))

;; Validate hex
(hex-valid? "deadbeef")       ; => #t
(hex-valid-bytes? "deadbeef") ; => #t (even length)
(hex-valid-bytes? "deadbee")  ; => #f (odd length)

;; Hash type detection
(hex-is-sha256? "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
; => #t

;; Case-insensitive comparison
(hex-equals? "DEADBEEF" "deadbeef")  ; => #t

;; Pad to length
(hex-pad-left 8 "ff")  ; => "000000ff"
```

## Use with STATE.scm / META.scm / ECOSYSTEM.scm

The proven library is ideal for validating data in your Guile state files:

```scheme
;; In your STATE.scm
(use-modules (proven safe-path)
             (proven safe-string))

(define (validate-state state)
  (let* ((repo-path (assoc-ref state 'repo-path))
         (path-result (path-validate repo-path)))
    (if (not (path-result-ok? path-result))
        (error "Invalid repository path" (path-result-error path-result))
        state)))
```

## License

PMPL-1.0 (Palimpsest-MPL-1.0)
