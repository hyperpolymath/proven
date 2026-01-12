# Proven for R

Safe, validated operations library for R applications.

## Installation

```r
# Install from GitHub
devtools::install_github("hyperpolymath/proven", subdir = "bindings/r")

# Or install from local
install.packages("proven", repos = NULL, type = "source")
```

## Usage

```r
library(proven)

# Safe math with overflow checking
result <- safe_add(1000000000000, 2000000000000)
if (!is.na(result)) {
  print(paste("Sum:", result))
} else {
  print("Overflow!")
}

# XSS prevention
safe_html <- escape_html("<script>alert('xss')</script>")
# "&lt;script&gt;alert('xss')&lt;/script&gt;"

# Path safety
if (has_traversal("../../../etc/passwd")) {
  stop("Dangerous path!")
}

# Email validation
result <- parse_email("user@example.com")
if (result$ok) {
  print(paste("Local:", result$local_part, "Domain:", result$domain))
} else {
  print(paste("Invalid:", result$error))
}

# IP classification
ip <- parse_ipv4("192.168.1.1")
if (!is.null(ip)) {
  print(paste("Private:", is_private_ip(ip)))
  print(paste("Classification:", classify_ip(ip)))
}

# Secure tokens
token <- generate_token_default()
print(paste("CSRF token:", token))
```

## Modules

### SafeMath

Overflow-checked arithmetic returning NA on overflow:

```r
library(proven)

# All operations return NA on overflow
safe_add(a, b)
safe_sub(a, b)
safe_mul(a, b)
safe_div(a, b)

# Check for overflow
result <- safe_mul(big_number, another_big)
if (!is.na(result)) {
  print(result)
} else {
  print("Overflow!")
}

# Vector operations
safe_sum(c(1, 2, 3, 4, 5))      # 15
safe_product(c(2, 3, 4))        # 24
```

### SafeString

XSS prevention and string sanitization:

```r
library(proven)

escape_html("<script>")        # "&lt;script&gt;"
escape_sql("O'Brien")          # "O''Brien"
escape_js(user_input)
url_encode("hello world")      # "hello%20world"
sanitize_default(input)        # Only alphanumeric + _-
slugify("Hello World!")        # "hello-world"
```

### SafePath

Directory traversal protection:

```r
library(proven)

# Check for traversal
if (has_traversal(user_path)) {
  stop("Path traversal detected")
}

# Sanitize filename
sanitize_filename("../../../etc/passwd")  # "etc_passwd"

# Safe path joining
result <- path_join("/base", "subdir", "file.txt")
if (result$ok) {
  print(result$path)
} else {
  print(paste("Error:", result$error))
}

# Resolve within base directory
result <- resolve_within("/var/www", user_path)
if (result$ok) {
  # Safe to use result$path
} else {
  # Path escape attempt
}
```

### SafeEmail

Email validation with result lists:

```r
library(proven)

# Simple validation
if (is_valid_email(email)) {
  # Valid format
}

# Parse with result checking
result <- parse_email(email)
if (result$ok) {
  print(paste("Local:", result$local_part, "Domain:", result$domain))
} else {
  print(paste("Error:", result$error))
}

# Check for disposable emails
is_disposable_email("user@mailinator.com")  # TRUE

# Normalize
normalize_email("User@EXAMPLE.COM")  # "User@example.com"
```

### SafeNetwork

IP address validation and classification:

```r
library(proven)

# Parse IPv4
ip <- parse_ipv4("192.168.1.1")
if (!is.null(ip)) {
  print(paste("Loopback:", is_loopback(ip)))
  print(paste("Private:", is_private_ip(ip)))

  classification <- classify_ip(ip)
  if (classification == IP_PRIVATE) {
    print("Private network")
  } else if (classification == IP_LOOPBACK) {
    print("Localhost")
  } else if (classification == IP_PUBLIC) {
    print("Public IP")
  }
}

# Validate port
is_valid_port(8080)  # TRUE

# SSRF protection
if (is_private_url(url)) {
  stop("Cannot access private URLs")
}
```

### SafeCrypto

Cryptographic operations:

```r
library(proven)

# Secure random generation
random_bytes(32)
random_hex(16)
generate_token_default()

# Hashing
sha256_hash("data")
sha512_hash("data")

# HMAC
hmac_sha256(key, message)
verify_hmac_sha256(key, message, expected_mac)

# Constant-time comparison (timing attack prevention)
constant_time_equals_string(actual, expected)

# Password generation
generate_password_default()
```

## Dependencies

- `openssl` - For cryptographic operations
- `base64enc` - For base64 encoding/decoding

## License

PMPL-1.0
