# Proven - Prolog Bindings

Safe, validated operations library for SWI-Prolog.

## Installation

### Using pack_install

```prolog
?- pack_install(proven).
```

### Manual Installation

1. Copy the `proven` directory to your Prolog library path
2. Load the main module:

```prolog
?- use_module(proven).
```

## Usage

```prolog
%% Load all modules
?- use_module(proven).

%% Or load specific modules
?- use_module(proven_math).
?- use_module(proven_string).
```

## Modules

### proven_math - Overflow-Checked Arithmetic

```prolog
%% Safe addition with overflow checking
?- safe_add(100, 200, Result).
Result = ok(300).

%% Overflow detection
?- max_int64(Max), safe_add(Max, 1, Result).
Result = error(overflow).

%% Safe division (handles zero)
?- safe_div(100, 0, Result).
Result = error(division_by_zero).

%% Safe multiplication
?- safe_mul(1000, 1000, Result).
Result = ok(1000000).

%% Clamp values to range
?- clamp(150, 0, 100, Result).
Result = 100.

%% Check if value in range
?- in_range(50, 0, 100).
true.

%% Constants
?- max_int64(Max).
Max = 9223372036854775807.

?- min_int64(Min).
Min = -9223372036854775808.
```

### proven_string - XSS Prevention

```prolog
%% HTML escaping
?- escape_html('<script>alert("xss")</script>', Result).
Result = '&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;'.

%% SQL escaping (single quotes)
?- escape_sql('O\'Reilly', Result).
Result = 'O\'\'Reilly'.

%% JavaScript escaping
?- escape_js('alert("hello")', Result).
Result = 'alert(\\"hello\\")'.

%% Sanitize to alphanumeric
?- sanitize_default('user@123!', Result).
Result = 'user123'.

%% URL encoding
?- url_encode('hello world', Result).
Result = 'hello%20world'.

%% Create URL-safe slugs
?- slugify('Hello World!', Result).
Result = 'hello-world'.
```

### proven_path - Directory Traversal Prevention

```prolog
%% Check for traversal attempts
?- has_traversal('../etc/passwd').
true.

?- has_traversal('documents/file.txt').
false.

%% Sanitize filenames
?- sanitize_filename('file<name>.txt', Result).
Result = 'file_name_.txt'.

%% Safe path joining
?- safe_path_join('/var/www', 'uploads/file.txt', Result).
Result = ok('/var/www/uploads/file.txt').

%% Traversal attempt blocked
?- safe_path_join('/var/www', '../etc/passwd', Result).
Result = error(traversal_detected).
```

### proven_email - Email Validation

```prolog
%% Validate email format
?- is_valid_email('user@example.com').
true.

?- is_valid_email('invalid').
false.

%% Parse email into components
?- parse_email('user@example.com', LocalPart, Domain).
LocalPart = user,
Domain = 'example.com'.

%% Check for disposable emails
?- is_disposable_email('user@tempmail.com').
true.

%% Normalize email (lowercase domain)
?- normalize_email('User@EXAMPLE.COM', Result).
Result = 'User@example.com'.
```

### proven_network - IP Address Validation

```prolog
%% Parse IPv4 address
?- parse_ipv4('192.168.1.1', Ip).
Ip = ipv4(192, 168, 1, 1).

%% Format IPv4 address
?- format_ipv4(ipv4(192, 168, 1, 1), Str).
Str = '192.168.1.1'.

%% Classify IP addresses
?- parse_ipv4('127.0.0.1', Ip), is_loopback(Ip).
true.

?- parse_ipv4('192.168.1.1', Ip), is_private_ip(Ip).
true.

?- parse_ipv4('8.8.8.8', Ip), is_public_ip(Ip).
true.

%% Get classification
?- parse_ipv4('10.0.0.1', Ip), classify_ip(Ip, Class).
Class = private.

%% Port validation
?- is_valid_port(8080).
true.

?- is_privileged_port(80).
true.

?- is_privileged_port(8080).
false.
```

### proven_crypto - Cryptographic Utilities

```prolog
%% Constant-time string comparison (timing-attack resistant)
?- constant_time_equals(secret, secret).
true.

?- constant_time_equals(secret, 'Secret').
false.

%% Simple hashing (FNV-1a)
?- simple_hash(password, Hash).
Hash = 116329031.

%% Convert bytes to hex
?- bytes_to_hex([255, 0, 128], Hex).
Hex = 'ff0080'.

%% Generate random tokens
?- generate_token(32, Token).
Token = 'a1b2c3d4...'.

%% Generate random integers
?- random_int(1, 100, Result).
Result = 42.  % (random value)
```

## API Reference

### proven_math

| Predicate | Description |
|-----------|-------------|
| `max_int64(-Max)` | Get maximum 64-bit integer |
| `min_int64(-Min)` | Get minimum 64-bit integer |
| `safe_add(+A, +B, -Result)` | Add with overflow check |
| `safe_sub(+A, +B, -Result)` | Subtract with overflow check |
| `safe_mul(+A, +B, -Result)` | Multiply with overflow check |
| `safe_div(+A, +B, -Result)` | Divide with zero/overflow check |
| `safe_mod(+A, +B, -Result)` | Modulo with zero check |
| `safe_abs(+A, -Result)` | Absolute value with overflow check |
| `safe_negate(+A, -Result)` | Negate with overflow check |
| `clamp(+Value, +Min, +Max, -Result)` | Clamp value to range |
| `in_range(+Value, +Min, +Max)` | Check if value in range |

### proven_string

| Predicate | Description |
|-----------|-------------|
| `escape_html(+Input, -Output)` | Escape HTML special characters |
| `escape_sql(+Input, -Output)` | Escape SQL single quotes |
| `escape_js(+Input, -Output)` | Escape JavaScript special characters |
| `sanitize_default(+Input, -Output)` | Keep only alphanumeric, underscore, hyphen |
| `url_encode(+Input, -Output)` | URL-encode string |
| `slugify(+Input, -Output)` | Convert to URL-safe slug |

### proven_path

| Predicate | Description |
|-----------|-------------|
| `has_traversal(+Path)` | Check for traversal patterns |
| `sanitize_filename(+Input, -Output)` | Remove dangerous filename characters |
| `safe_path_join(+Base, +Filename, -Result)` | Safely join paths |

### proven_email

| Predicate | Description |
|-----------|-------------|
| `is_valid_email(+Email)` | Validate email format |
| `parse_email(+Email, -LocalPart, -Domain)` | Parse into local/domain |
| `is_disposable_email(+Email)` | Check for disposable provider |
| `normalize_email(+Email, -Normalized)` | Normalize email address |

### proven_network

| Predicate | Description |
|-----------|-------------|
| `parse_ipv4(+IpString, -Ip)` | Parse IPv4 address |
| `format_ipv4(+Ip, -String)` | Format as dotted-decimal |
| `is_loopback(+Ip)` | Check if loopback |
| `is_private_ip(+Ip)` | Check if private |
| `is_reserved_ip(+Ip)` | Check if reserved |
| `is_public_ip(+Ip)` | Check if public |
| `classify_ip(+Ip, -Class)` | Get classification atom |
| `is_valid_port(+Port)` | Validate port number |
| `is_privileged_port(+Port)` | Check if privileged port |

### proven_crypto

| Predicate | Description |
|-----------|-------------|
| `constant_time_equals(+A, +B)` | Timing-safe comparison |
| `simple_hash(+Input, -Hash)` | FNV-1a hash |
| `bytes_to_hex(+Bytes, -Hex)` | Convert bytes to hex |
| `generate_token(+Length, -Token)` | Generate random hex token |
| `random_int(+Min, +Max, -Result)` | Random integer in range |

## Result Types

### Math/Path Results
```prolog
ok(Value)                  % Success with value
error(overflow)            % Arithmetic overflow
error(division_by_zero)    % Division by zero
error(traversal_detected)  % Path traversal detected
```

### IP Address
```prolog
ipv4(A, B, C, D)          % Valid IPv4 address
invalid                    % Invalid IP address
```

### IP Classification
```prolog
invalid   % Invalid IP address
loopback  % Loopback address (127.x.x.x)
private   % Private address (10.x, 172.16-31.x, 192.168.x)
reserved  % Reserved address
public    % Public address
```

## Compatibility

- **SWI-Prolog 8.0+** - Fully supported
- **SWI-Prolog 7.x** - Should work (not tested)

## Building Pack

```prolog
?- pack_create(proven, [author('Hyperpolymath'), version('0.1.0')]).
```

## License

PMPL-1.0 (Polymath Public License)
