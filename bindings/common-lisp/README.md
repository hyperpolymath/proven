# Proven - Common Lisp Bindings

Safe, validated operations library for Common Lisp.

## Installation

### Using Quicklisp (when available)

```lisp
(ql:quickload :proven)
```

### Manual Installation

1. Clone or copy to your ASDF source registry (e.g., `~/common-lisp/proven/`)
2. Load with ASDF:

```lisp
(asdf:load-system :proven)
```

## Usage

```lisp
(use-package :proven)

;; Or use qualified names
(proven:safe-add 1 2)
```

## Modules

### SafeMath - Overflow-Checked Arithmetic

```lisp
;; Safe addition with overflow checking
(let ((result (safe-add 100 200)))
  (when (safe-result-ok-p result)
    (format t "Sum: ~A~%" (safe-result-value result))))

;; Safe multiplication
(let ((result (safe-mul 1000000 1000000)))
  (if (safe-result-ok-p result)
      (format t "Product: ~A~%" (safe-result-value result))
      (format t "Overflow!~%")))

;; Safe division (handles zero)
(let ((result (safe-div 100 0)))
  (unless (safe-result-ok-p result)
    (format t "Division by zero prevented~%")))

;; Clamp values to range
(clamp 150 0 100)  ; => 100

;; Check if value in range
(in-range-p 50 0 100)  ; => T
```

### SafeString - XSS Prevention

```lisp
;; HTML escaping
(escape-html "<script>alert('xss')</script>")
; => "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"

;; SQL escaping (single quotes)
(escape-sql "O'Reilly")  ; => "O''Reilly"

;; JavaScript escaping
(escape-js "alert(\"hello\")")  ; => "alert(\\\"hello\\\")"

;; Sanitize to alphanumeric
(sanitize-default "user@123!")  ; => "user123"

;; URL encoding
(url-encode "hello world")  ; => "hello%20world"

;; Create URL-safe slugs
(slugify "Hello World!")  ; => "hello-world"
```

### SafePath - Directory Traversal Prevention

```lisp
;; Check for traversal attempts
(has-traversal-p "../etc/passwd")     ; => T
(has-traversal-p "documents/file.txt") ; => NIL

;; Sanitize filenames
(sanitize-filename "file<name>.txt")  ; => "file_name_.txt"

;; Safe path joining
(let ((result (safe-path-join "/var/www" "uploads/file.txt")))
  (when (path-result-ok-p result)
    (format t "Safe path: ~A~%" (path-result-path result))))

;; Traversal attempt blocked
(let ((result (safe-path-join "/var/www" "../etc/passwd")))
  (unless (path-result-ok-p result)
    (format t "Blocked: ~A~%" (path-result-error result))))
```

### SafeEmail - Email Validation

```lisp
;; Validate email format
(valid-email-p "user@example.com")  ; => T
(valid-email-p "invalid")           ; => NIL

;; Parse email into components
(let ((result (parse-email "user@example.com")))
  (when (email-result-ok-p result)
    (format t "Local: ~A, Domain: ~A~%"
            (email-result-local-part result)
            (email-result-domain result))))

;; Check for disposable emails
(disposable-email-p "user@tempmail.com")  ; => T

;; Normalize email (lowercase domain)
(normalize-email "User@EXAMPLE.COM")  ; => "User@example.com"
```

### SafeNetwork - IP Address Validation

```lisp
;; Parse IPv4 address
(let ((addr (parse-ipv4 "192.168.1.1")))
  (when (ipv4-address-valid-p addr)
    (format t "Valid IP: ~A~%" (format-ipv4 addr))))

;; Classify IP addresses
(let ((addr (parse-ipv4 "127.0.0.1")))
  (loopback-p addr))    ; => T

(let ((addr (parse-ipv4 "192.168.1.1")))
  (private-ip-p addr))  ; => T

(let ((addr (parse-ipv4 "8.8.8.8")))
  (public-ip-p addr))   ; => T

;; Get classification constant
(classify-ip (parse-ipv4 "10.0.0.1"))  ; => +IP-CLASS-PRIVATE+

;; Port validation
(valid-port-p 8080)      ; => T
(privileged-port-p 80)   ; => T
(privileged-port-p 8080) ; => NIL
```

### SafeCrypto - Cryptographic Utilities

```lisp
;; Constant-time string comparison (timing-attack resistant)
(constant-time-equal-p "secret" "secret")  ; => T
(constant-time-equal-p "secret" "Secret")  ; => NIL

;; Simple hashing (FNV-1a)
(simple-hash "password")  ; => unsigned 32-bit integer

;; Convert bytes to hex
(bytes-to-hex #(255 0 128))  ; => "ff0080"

;; Generate random tokens
(generate-token 32)  ; => "a1b2c3d4e5f6..."

;; Generate random integers
(random-int 1 100)  ; => random number 1-100

;; Secure memory wipe (best-effort)
(let ((secret (make-array 10 :element-type 'character
                             :fill-pointer 10
                             :adjustable t)))
  (setf (subseq secret 0) "secretkey!")
  ;; Use the secret...
  (secure-wipe secret))
```

## API Reference

### SafeMath

| Function | Description |
|----------|-------------|
| `safe-add (a b)` | Add with overflow check |
| `safe-sub (a b)` | Subtract with overflow check |
| `safe-mul (a b)` | Multiply with overflow check |
| `safe-div (a b)` | Divide with zero check |
| `safe-mod (a b)` | Modulo with zero check |
| `safe-abs (a)` | Absolute value with overflow check |
| `safe-negate (a)` | Negate with overflow check |
| `clamp (value min max)` | Clamp value to range |
| `in-range-p (value min max)` | Check if value in range |

### SafeString

| Function | Description |
|----------|-------------|
| `escape-html (input)` | Escape HTML special characters |
| `escape-sql (input)` | Escape SQL single quotes |
| `escape-js (input)` | Escape JavaScript special characters |
| `sanitize-default (input)` | Keep only alphanumeric, underscore, hyphen |
| `url-encode (input)` | URL-encode string |
| `slugify (input)` | Convert to URL-safe slug |

### SafePath

| Function | Description |
|----------|-------------|
| `has-traversal-p (path)` | Check for traversal patterns |
| `sanitize-filename (input)` | Remove dangerous filename characters |
| `safe-path-join (base filename)` | Safely join paths |

### SafeEmail

| Function | Description |
|----------|-------------|
| `valid-email-p (email)` | Validate email format |
| `parse-email (email)` | Parse into local/domain |
| `disposable-email-p (email)` | Check for disposable provider |
| `normalize-email (email)` | Normalize email address |

### SafeNetwork

| Function | Description |
|----------|-------------|
| `parse-ipv4 (ip-string)` | Parse IPv4 address |
| `format-ipv4 (addr)` | Format as dotted-decimal |
| `loopback-p (addr)` | Check if loopback |
| `private-ip-p (addr)` | Check if private |
| `reserved-ip-p (addr)` | Check if reserved |
| `public-ip-p (addr)` | Check if public |
| `classify-ip (addr)` | Get classification constant |
| `valid-port-p (port)` | Validate port number |
| `privileged-port-p (port)` | Check if privileged port |

### SafeCrypto

| Function | Description |
|----------|-------------|
| `constant-time-equal-p (a b)` | Timing-safe comparison |
| `simple-hash (input)` | FNV-1a hash |
| `bytes-to-hex (bytes)` | Convert bytes to hex |
| `generate-token (length)` | Generate random hex token |
| `random-int (min max)` | Random integer in range |
| `secure-wipe (string)` | Best-effort memory wipe |

## Result Types

### safe-result
```lisp
(defstruct safe-result
  (value 0 :type integer)
  (ok-p nil :type boolean))
```

### path-result
```lisp
(defstruct path-result
  (path "" :type string)
  (error "" :type string)
  (ok-p nil :type boolean))
```

### email-result
```lisp
(defstruct email-result
  (local-part "" :type string)
  (domain "" :type string)
  (error "" :type string)
  (ok-p nil :type boolean))
```

### ipv4-address
```lisp
(defstruct ipv4-address
  (octets #(0 0 0 0) :type (simple-vector 4))
  (valid-p nil :type boolean))
```

## Constants

```lisp
+max-int64+        ; => 9223372036854775807
+min-int64+        ; => -9223372036854775808
+ip-class-invalid+ ; => 0
+ip-class-loopback+ ; => 1
+ip-class-private+  ; => 2
+ip-class-reserved+ ; => 3
+ip-class-public+   ; => 4
```

## Compatibility

- **SBCL** - Fully supported
- **CCL** (Clozure) - Fully supported
- **ECL** - Fully supported
- **ABCL** - Fully supported
- **CLISP** - Fully supported

## License

PMPL-1.0 (Polymath Public License)
