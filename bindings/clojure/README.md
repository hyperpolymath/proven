# Proven for Clojure

Safe, validated operations library for Clojure applications.

## Installation

Add to your `project.clj`:

```clojure
[com.hyperpolymath/proven "0.1.0"]
```

Or `deps.edn`:

```clojure
com.hyperpolymath/proven {:mvn/version "0.1.0"}
```

## Usage

```clojure
(require '[proven.core :as proven])
(require '[proven.safe-math :as math])
(require '[proven.safe-string :as string])
(require '[proven.safe-path :as path])
(require '[proven.safe-email :as email])
(require '[proven.safe-network :as network])
(require '[proven.safe-crypto :as crypto])

;; Safe math with overflow checking
(math/add 1000000000000 2000000000000)
;; => 3000000000000 (or nil on overflow)

;; XSS prevention
(string/escape-html "<script>alert('xss')</script>")
;; => "&lt;script&gt;alert('xss')&lt;/script&gt;"

;; Path safety
(path/has-traversal? "../../../etc/passwd")
;; => true

;; Email validation
(email/parse "user@example.com")
;; => {:ok {:local-part "user" :domain "example.com"}}

;; IP classification
(when-let [ip (network/parse-ipv4 "192.168.1.1")]
  (network/classify ip))
;; => :private

;; Secure tokens
(crypto/generate-token)
;; => "xK7f9..."
```

## Modules

### proven.safe-math

Overflow-checked arithmetic returning `nil` on overflow:

```clojure
(require '[proven.safe-math :as math])

;; All operations return result or nil
(math/add a b)
(math/sub a b)
(math/mul a b)
(math/div a b)

;; Use with when-let or if-let
(when-let [result (math/mul big-number another-big)]
  (println result))

;; Default values
(or (math/div x y) 0)

;; Thread through calculations
(some-> 100
        (math/mul 200)
        (math/add 50))

;; Sum with overflow checking
(math/safe-sum [1 2 3 4 5])
;; => 15 (or nil if any overflow)
```

### proven.safe-string

XSS prevention and string sanitization:

```clojure
(require '[proven.safe-string :as string])

;; HTML escaping
(string/escape-html "<script>")
;; => "&lt;script&gt;"

;; SQL escaping (prefer parameterized queries!)
(string/escape-sql "O'Brien")
;; => "O''Brien"

;; JavaScript escaping
(string/escape-js user-input)

;; URL encoding
(string/url-encode "hello world")
;; => "hello+world"

;; String sanitization
(string/sanitize input "a-zA-Z0-9")
```

### proven.safe-path

Directory traversal protection:

```clojure
(require '[proven.safe-path :as path])

;; Check for traversal
(when (path/has-traversal? user-path)
  (throw (ex-info "Path traversal detected" {:path user-path})))

;; Sanitize filename
(path/sanitize-filename "../../../etc/passwd")
;; => "etc_passwd"

;; Safe path joining
(path/join "/base" "subdir" "file.txt")
;; => {:ok "/base/subdir/file.txt"}

;; Resolve within base directory
(case (:ok (path/resolve-within "/var/www" user-path))
  nil (throw (ex-info "Path escape" {}))
  path (slurp path))
```

### proven.safe-email

Email validation with idiomatic Clojure results:

```clojure
(require '[proven.safe-email :as email])

;; Simple validation
(email/valid? "user@example.com")
;; => true

;; Parse with error handling
(let [result (email/parse email)]
  (if (:ok result)
    (let [{:keys [local-part domain]} (:ok result)]
      (println "Local:" local-part "Domain:" domain))
    (println "Error:" (:error result))))

;; Check for disposable emails
(email/disposable? "user@mailinator.com")
;; => true

;; Normalize
(email/normalize "User@EXAMPLE.COM")
;; => "User@example.com"
```

### proven.safe-network

IP address validation and classification:

```clojure
(require '[proven.safe-network :as network])

;; Parse IPv4
(when-let [ip (network/parse-ipv4 "192.168.1.1")]
  (println "Loopback:" (network/loopback? ip))
  (println "Private:" (network/private? ip))
  (println "Classification:" (network/classify ip)))

;; Validate port
(network/valid-port? 8080)
;; => true

;; SSRF protection
(when (network/private-url? url)
  (throw (ex-info "Cannot access private URLs" {:url url})))
```

### proven.safe-crypto

Cryptographic operations:

```clojure
(require '[proven.safe-crypto :as crypto])

;; Secure random generation
(crypto/random-bytes 32)
(crypto/random-hex 16)
(crypto/generate-token)

;; Hashing
(crypto/sha256 "data")
(crypto/sha512 "data")

;; HMAC
(crypto/hmac-sha256 key message)
(crypto/verify-hmac-sha256 key message expected-mac)

;; Constant-time comparison (timing attack prevention)
(crypto/constant-time-equals actual expected)

;; Key derivation
(crypto/pbkdf2 password salt 100000 32)
```

## License

PMPL-1.0
