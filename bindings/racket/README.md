# Proven for Racket

Safe, validated operations library for Racket applications.

## Installation

Install from the package catalog:

```bash
raco pkg install proven
```

Or install from local directory:

```bash
raco pkg install /path/to/proven/bindings/racket
```

## Usage

```racket
#lang racket

(require proven)

;; Safe math with overflow checking
(define result (safe-add 1000000000 2000000000))
(if (safe-result-ok? result)
    (printf "Sum: ~a~n" (safe-result-value result))
    (displayln "Overflow!"))

;; XSS prevention
(define safe-html (escape-html "<script>alert('xss')</script>"))
;; "&lt;script&gt;alert('xss')&lt;/script&gt;"

;; Path safety
(when (has-traversal? "../../../etc/passwd")
  (error "Dangerous path!"))

;; Email validation
(define email-res (parse-email "user@example.com"))
(when (email-result-ok? email-res)
  (printf "Local: ~a~n" (email-result-local-part email-res))
  (printf "Domain: ~a~n" (email-result-domain email-res)))

;; IP classification
(define ip (parse-ipv4 "192.168.1.1"))
(when (ipv4-address-valid? ip)
  (printf "Private: ~a~n" (private-ip? ip))
  (printf "Classification: ~a~n" (classify-ip ip)))

;; Secure tokens
(define token (generate-token 32))
(printf "Token: ~a~n" token)
```

## Modules

### safe-math

Overflow-checked arithmetic with result structs:

```racket
(require proven)

;; All operations return safe-result struct
(define result (safe-add a b))
(safe-result-value result)  ; The value
(safe-result-ok? result)    ; #t if succeeded

;; Available operations
(safe-add a b)      ; Addition
(safe-sub a b)      ; Subtraction
(safe-mul a b)      ; Multiplication
(safe-div a b)      ; Division (checks zero)
(safe-mod a b)      ; Modulo
(safe-abs a)        ; Absolute value
(safe-negate a)     ; Negation

;; Pattern matching on results
(match (safe-mul big-num another-big)
  [(safe-result val #t) (printf "Result: ~a~n" val)]
  [(safe-result _ #f)   (displayln "Overflow!")])

;; Range operations
(clamp value min-val max-val)
(in-range? value min-val max-val)  ; Returns #t/#f
```

### safe-string

XSS prevention and string sanitization:

```racket
(require proven)

(escape-html "<script>")       ; "&lt;script&gt;"
(escape-sql "O'Brien")         ; "O''Brien"
(escape-js "line\nbreak")      ; "line\\nbreak"
(url-encode "hello world")     ; "hello%20world"
(sanitize-default "user@host") ; "userhost"
(slugify "Hello World!")       ; "hello-world"
```

### safe-path

Directory traversal protection:

```racket
(require proven)

;; Check for traversal
(has-traversal? "../../../etc/passwd")  ; #t

;; Sanitize filename
(sanitize-filename "../bad<file>.txt")  ; "__bad_file_.txt"

;; Safe path joining - returns path-result struct
(define result (safe-path-join "/base" "file.txt"))
(when (path-result-ok? result)
  (displayln (path-result-path result)))
```

### safe-email

Email validation:

```racket
(require proven)

;; Simple validation
(valid-email? "user@example.com")  ; #t

;; Parse with result checking
(define result (parse-email "user@example.com"))
(email-result-ok? result)          ; #t
(email-result-local-part result)   ; "user"
(email-result-domain result)       ; "example.com"
(email-result-error result)        ; "" (empty if ok)

;; Check for disposable emails
(disposable-email? "mailinator.com")  ; #t

;; Normalize (lowercase domain)
(normalize-email "User@EXAMPLE.COM")  ; "User@example.com"
```

### safe-network

IP address validation and classification:

```racket
(require proven)

;; Parse IPv4 - returns ipv4-address struct
(define ip (parse-ipv4 "192.168.1.1"))
(ipv4-address-valid? ip)    ; #t
(ipv4-address-octets ip)    ; '(192 168 1 1)

;; Type checks
(loopback? ip)              ; #f
(private-ip? ip)            ; #t
(reserved-ip? ip)           ; #f
(public-ip? ip)             ; #f

;; Classification (returns integer constant)
(classify-ip ip)            ; ip-class-private

;; Classification constants
ip-class-invalid   ; 0
ip-class-loopback  ; 1
ip-class-private   ; 2
ip-class-reserved  ; 3
ip-class-public    ; 4

;; Format as string
(format-ipv4 ip)            ; "192.168.1.1"

;; Port validation
(valid-port? 8080)          ; #t
(privileged-port? 80)       ; #t
```

### safe-crypto

Cryptographic operations:

```racket
(require proven)

;; Constant-time comparison (timing attack prevention)
(constant-time-equal? actual expected)  ; #t/#f

;; SHA-256 hash
(sha256-hash "data")

;; SHA-1 hash (compatibility only)
(sha1-hash "data")

;; Convert bytes to hex
(bytes->hex #"hello")

;; Generate random token
(generate-token 32)

;; Random integer in range
(random-int 1 100)

;; Secure wipe (use box for mutable string)
(define sensitive (box "secret data"))
(secure-wipe! sensitive)
```

## Data Types

### safe-result

```racket
(struct safe-result (value ok?) #:transparent)

;; Usage
(safe-result-value result)  ; Get the value
(safe-result-ok? result)    ; Check if operation succeeded
```

### path-result

```racket
(struct path-result (path error ok?) #:transparent)

;; Usage
(path-result-path result)   ; Get the path
(path-result-error result)  ; Get error message
(path-result-ok? result)    ; Check if operation succeeded
```

### email-result

```racket
(struct email-result (local-part domain error ok?) #:transparent)

;; Usage
(email-result-local-part result)
(email-result-domain result)
(email-result-error result)
(email-result-ok? result)
```

### ipv4-address

```racket
(struct ipv4-address (octets valid?) #:transparent)

;; Usage
(ipv4-address-octets ip)    ; List of 4 integers
(ipv4-address-valid? ip)    ; #t/#f
```

## Examples

### Safe Input Processing

```racket
#lang racket

(require proven)

(define (process-user-input name email path)
  ;; Sanitize name for display
  (define safe-name (escape-html name))

  ;; Validate email
  (define email-res (parse-email email))
  (unless (email-result-ok? email-res)
    (error 'process-user-input
           (email-result-error email-res)))

  ;; Check path is safe
  (when (has-traversal? path)
    (error 'process-user-input "Invalid path"))

  (values safe-name
          (normalize-email email)
          (sanitize-filename path)))

;; Usage
(define-values (name email path)
  (process-user-input
   "<script>Bob</script>"
   "user@EXAMPLE.COM"
   "../file.txt"))
```

### Network Request Validation

```racket
#lang racket

(require proven)

(define (validate-request ip-addr port)
  ;; Parse and validate IP
  (define ip (parse-ipv4 ip-addr))
  (unless (ipv4-address-valid? ip)
    (error 'validate-request "Invalid IP address"))

  ;; Block private IPs (SSRF protection)
  (when (or (private-ip? ip) (loopback? ip))
    (error 'validate-request "Private IPs not allowed"))

  ;; Validate port
  (unless (valid-port? port)
    (error 'validate-request "Invalid port"))

  (values ip-addr port))
```

### Pattern Matching with Results

```racket
#lang racket

(require proven racket/match)

(define (calculate-with-overflow a b c)
  (match* ((safe-mul a b) (safe-add a c))
    [((safe-result prod #t) (safe-result sum #t))
     (printf "Product: ~a, Sum: ~a~n" prod sum)]
    [(_ _)
     (displayln "Calculation overflow!")]))
```

## Compatibility

This library requires:
- Racket 7.0 or later

## License

PMPL-1.0
