# Proven for Crystal

Safe, validated operations library for Crystal applications.

## Installation

Add this to your `shard.yml`:

```yaml
dependencies:
  proven:
    github: hyperpolymath/proven
    version: ~> 0.1.0
```

Then run:

```bash
shards install
```

## Usage

```crystal
require "proven"

# Safe math with overflow checking
if result = Proven::SafeMath.add(1000000000000_i64, 2000000000000_i64)
  puts "Sum: #{result}"
else
  puts "Overflow!"
end

# XSS prevention
safe_html = Proven::SafeString.escape_html("<script>alert('xss')</script>")
# "&lt;script&gt;alert('xss')&lt;/script&gt;"

# Path safety
if Proven::SafePath.has_traversal?("../../../etc/passwd")
  raise "Dangerous path!"
end

# Email validation
case result = Proven::SafeEmail.parse("user@example.com")
when .ok?
  puts "Local: #{result.local_part}, Domain: #{result.domain}"
when .error?
  puts "Invalid: #{result.error}"
end

# IP classification
if ip = Proven::SafeNetwork.parse_ipv4("192.168.1.1")
  puts "Private: #{Proven::SafeNetwork.private?(ip)}"
  puts "Classification: #{Proven::SafeNetwork.classify(ip)}"
end

# Secure tokens
token = Proven::SafeCrypto.generate_token_default
puts "CSRF token: #{token}"
```

## Modules

### SafeMath

Overflow-checked arithmetic returning `nil` on overflow:

```crystal
require "proven"

# All operations return Nilable types
Proven::SafeMath.add(a, b)     # Int64?
Proven::SafeMath.sub(a, b)     # Int64?
Proven::SafeMath.mul(a, b)     # Int64?
Proven::SafeMath.div(a, b)     # Int64?

# Pattern matching with if
if result = Proven::SafeMath.mul(big_number, another_big)
  puts result
else
  puts "Overflow!"
end

# Array operations
Proven::SafeMath.safe_sum([1_i64, 2_i64, 3_i64])      # 6_i64
Proven::SafeMath.safe_product([2_i64, 3_i64, 4_i64]) # 24_i64
```

### SafeString

XSS prevention and string sanitization:

```crystal
require "proven"

Proven::SafeString.escape_html("<script>")     # "&lt;script&gt;"
Proven::SafeString.escape_sql("O'Brien")       # "O''Brien"
Proven::SafeString.escape_js(user_input)
Proven::SafeString.url_encode("hello world")   # "hello%20world"
Proven::SafeString.sanitize_default(input)     # Only alphanumeric + _-
Proven::SafeString.slugify("Hello World!")     # "hello-world"
```

### SafePath

Directory traversal protection:

```crystal
require "proven"

# Check for traversal
if Proven::SafePath.has_traversal?(user_path)
  raise "Path traversal detected"
end

# Sanitize filename
Proven::SafePath.sanitize_filename("../../../etc/passwd") # "etc_passwd"

# Safe path joining
result = Proven::SafePath.join("/base", "subdir", "file.txt")
if result.ok?
  puts result.path
else
  puts "Error: #{result.error}"
end

# Resolve within base directory
result = Proven::SafePath.resolve_within("/var/www", user_path)
case result
when .ok?
  # Safe to use result.path
when .error?
  # Path escape attempt
end
```

### SafeEmail

Email validation with result types:

```crystal
require "proven"

# Simple validation
if Proven::SafeEmail.valid?(email)
  # Valid format
end

# Parse with pattern matching
result = Proven::SafeEmail.parse(email)
if result.ok?
  puts "Local: #{result.local_part}, Domain: #{result.domain}"
else
  puts "Error: #{result.error}"
end

# Check for disposable emails
Proven::SafeEmail.disposable?("user@mailinator.com") # true

# Normalize
Proven::SafeEmail.normalize("User@EXAMPLE.COM") # "User@example.com"
```

### SafeNetwork

IP address validation and classification:

```crystal
require "proven"

# Parse IPv4
if ip = Proven::SafeNetwork.parse_ipv4("192.168.1.1")
  puts "Loopback: #{Proven::SafeNetwork.loopback?(ip)}"
  puts "Private: #{Proven::SafeNetwork.private?(ip)}"

  case Proven::SafeNetwork.classify(ip)
  when Proven::IpClassification::Private
    puts "Private network"
  when Proven::IpClassification::Loopback
    puts "Localhost"
  when Proven::IpClassification::Public
    puts "Public IP"
  end
end

# Validate port
Proven::SafeNetwork.valid_port?(8080) # true

# SSRF protection
if Proven::SafeNetwork.private_url?(url)
  raise "Cannot access private URLs"
end
```

### SafeCrypto

Cryptographic operations:

```crystal
require "proven"

# Secure random generation
Proven::SafeCrypto.random_bytes(32)
Proven::SafeCrypto.random_hex(16)
Proven::SafeCrypto.generate_token_default

# Hashing
Proven::SafeCrypto.sha256("data")
Proven::SafeCrypto.sha512("data")

# HMAC
Proven::SafeCrypto.hmac_sha256(key, message)
Proven::SafeCrypto.verify_hmac_sha256?(key, message, expected_mac)

# Constant-time comparison (timing attack prevention)
Proven::SafeCrypto.constant_time_equals_string?(actual, expected)

# Key derivation
Proven::SafeCrypto.pbkdf2_default(password, salt)
```

## License

PMPL-1.0
