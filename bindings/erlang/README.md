# Proven - Erlang Bindings

Safe, validated operations library for Erlang/OTP.

## Installation

### Using Rebar3

Add to your `rebar.config`:

```erlang
{deps, [
    {proven, "0.1.0"}
]}.
```

### Using Hex

```bash
rebar3 hex install proven
```

## Usage

```erlang
%% Import modules
-import(proven_math, [safe_add/2, clamp/3]).
-import(proven_string, [escape_html/1, slugify/1]).

%% Or use fully qualified names
proven_math:safe_add(1, 2).
```

## Modules

### proven_math - Overflow-Checked Arithmetic

```erlang
%% Safe addition with overflow checking
case proven_math:safe_add(100, 200) of
    {ok, Result} -> io:format("Sum: ~p~n", [Result]);
    {error, overflow} -> io:format("Overflow!~n")
end.

%% Safe multiplication
{ok, Product} = proven_math:safe_mul(10, 20),

%% Safe division (handles zero)
case proven_math:safe_div(100, 0) of
    {ok, _} -> ok;
    {error, division_by_zero} -> io:format("Division by zero prevented~n")
end.

%% Clamp values to range
100 = proven_math:clamp(150, 0, 100).

%% Check if value in range
true = proven_math:in_range(50, 0, 100).

%% Constants
Max = proven_math:max_int64(),  % 9223372036854775807
Min = proven_math:min_int64().  % -9223372036854775808
```

### proven_string - XSS Prevention

```erlang
%% HTML escaping
"&lt;script&gt;" = proven_string:escape_html("<script>").

%% SQL escaping (single quotes)
"O''Reilly" = proven_string:escape_sql("O'Reilly").

%% JavaScript escaping
"alert(\\\"hello\\\")" = proven_string:escape_js("alert(\"hello\")").

%% Sanitize to alphanumeric
"user123" = proven_string:sanitize_default("user@123!").

%% URL encoding
"hello%20world" = proven_string:url_encode("hello world").

%% Create URL-safe slugs
"hello-world" = proven_string:slugify("Hello World!").
```

### proven_path - Directory Traversal Prevention

```erlang
%% Check for traversal attempts
true = proven_path:has_traversal("../etc/passwd"),
false = proven_path:has_traversal("documents/file.txt").

%% Sanitize filenames
"file_name_.txt" = proven_path:sanitize_filename("file<name>.txt").

%% Safe path joining
case proven_path:safe_path_join("/var/www", "uploads/file.txt") of
    {ok, Path} -> io:format("Safe path: ~s~n", [Path]);
    {error, traversal_detected} -> io:format("Blocked!~n")
end.

%% Traversal attempt blocked
{error, traversal_detected} = proven_path:safe_path_join("/var/www", "../etc/passwd").
```

### proven_email - Email Validation

```erlang
%% Validate email format
true = proven_email:is_valid("user@example.com"),
false = proven_email:is_valid("invalid").

%% Parse email into components
case proven_email:parse("user@example.com") of
    {ok, LocalPart, Domain} ->
        io:format("Local: ~s, Domain: ~s~n", [LocalPart, Domain]);
    {error, Reason} ->
        io:format("Invalid: ~p~n", [Reason])
end.

%% Check for disposable emails
true = proven_email:is_disposable("user@tempmail.com").

%% Normalize email (lowercase domain)
{ok, "User@example.com"} = proven_email:normalize("User@EXAMPLE.COM").
```

### proven_network - IP Address Validation

```erlang
%% Parse IPv4 address
Ip = proven_network:parse_ipv4("192.168.1.1"),
{ipv4, 192, 168, 1, 1} = Ip.

%% Format IPv4 address
"192.168.1.1" = proven_network:format_ipv4(Ip).

%% Classify IP addresses
Loopback = proven_network:parse_ipv4("127.0.0.1"),
true = proven_network:is_loopback(Loopback).

Private = proven_network:parse_ipv4("192.168.1.1"),
true = proven_network:is_private(Private).

Public = proven_network:parse_ipv4("8.8.8.8"),
true = proven_network:is_public(Public).

%% Get classification
loopback = proven_network:classify(proven_network:parse_ipv4("127.0.0.1")),
private = proven_network:classify(proven_network:parse_ipv4("10.0.0.1")),
public = proven_network:classify(proven_network:parse_ipv4("8.8.8.8")).

%% Port validation
true = proven_network:is_valid_port(8080),
true = proven_network:is_privileged_port(80),
false = proven_network:is_privileged_port(8080).
```

### proven_crypto - Cryptographic Utilities

```erlang
%% Constant-time string comparison (timing-attack resistant)
true = proven_crypto:constant_time_equals("secret", "secret"),
false = proven_crypto:constant_time_equals("secret", "Secret").

%% Works with binaries too
true = proven_crypto:constant_time_equals(<<"key">>, <<"key">>).

%% Simple hashing (FNV-1a)
Hash = proven_crypto:simple_hash("password").

%% Convert bytes to hex
"ff0080" = proven_crypto:bytes_to_hex(<<255, 0, 128>>).

%% Generate random tokens
Token = proven_crypto:generate_token(32).

%% Generate random integers
Rand = proven_crypto:random_int(1, 100).

%% Secure memory wipe (best-effort)
Wiped = proven_crypto:secure_wipe(<<"secret">>).
```

## API Reference

### proven_math

| Function | Description |
|----------|-------------|
| `safe_add(A, B)` | Add with overflow check |
| `safe_sub(A, B)` | Subtract with overflow check |
| `safe_mul(A, B)` | Multiply with overflow check |
| `safe_div(A, B)` | Divide with zero/overflow check |
| `safe_mod(A, B)` | Modulo with zero check |
| `safe_abs(A)` | Absolute value with overflow check |
| `safe_negate(A)` | Negate with overflow check |
| `clamp(Value, Min, Max)` | Clamp value to range |
| `in_range(Value, Min, Max)` | Check if value in range |
| `max_int64()` | Get maximum 64-bit integer |
| `min_int64()` | Get minimum 64-bit integer |

### proven_string

| Function | Description |
|----------|-------------|
| `escape_html(Input)` | Escape HTML special characters |
| `escape_sql(Input)` | Escape SQL single quotes |
| `escape_js(Input)` | Escape JavaScript special characters |
| `sanitize_default(Input)` | Keep only alphanumeric, underscore, hyphen |
| `url_encode(Input)` | URL-encode string |
| `slugify(Input)` | Convert to URL-safe slug |

### proven_path

| Function | Description |
|----------|-------------|
| `has_traversal(Path)` | Check for traversal patterns |
| `sanitize_filename(Input)` | Remove dangerous filename characters |
| `safe_path_join(Base, Filename)` | Safely join paths |

### proven_email

| Function | Description |
|----------|-------------|
| `is_valid(Email)` | Validate email format |
| `parse(Email)` | Parse into local/domain |
| `is_disposable(Email)` | Check for disposable provider |
| `normalize(Email)` | Normalize email address |

### proven_network

| Function | Description |
|----------|-------------|
| `parse_ipv4(IpString)` | Parse IPv4 address |
| `format_ipv4(Ip)` | Format as dotted-decimal |
| `is_loopback(Ip)` | Check if loopback |
| `is_private(Ip)` | Check if private |
| `is_reserved(Ip)` | Check if reserved |
| `is_public(Ip)` | Check if public |
| `classify(Ip)` | Get classification atom |
| `is_valid_port(Port)` | Validate port number |
| `is_privileged_port(Port)` | Check if privileged port |

### proven_crypto

| Function | Description |
|----------|-------------|
| `constant_time_equals(A, B)` | Timing-safe comparison |
| `simple_hash(Input)` | FNV-1a hash |
| `bytes_to_hex(Bytes)` | Convert bytes to hex |
| `generate_token(Length)` | Generate random hex token |
| `random_int(Min, Max)` | Random integer in range |
| `secure_wipe(Bin)` | Best-effort memory wipe |

## Return Types

### proven_math
```erlang
{ok, integer()} | {error, overflow | division_by_zero}
```

### proven_path
```erlang
{ok, string()} | {error, traversal_detected}
```

### proven_email
```erlang
{ok, LocalPart, Domain} | {error, Reason}
```

### proven_network
```erlang
{ipv4, A, B, C, D} | invalid
```

## OTP Compatibility

- **OTP 24+** - Fully supported
- **OTP 23** - Fully supported
- **OTP 22** - Fully supported

## Building

```bash
rebar3 compile
rebar3 dialyzer
rebar3 xref
```

## Testing

```bash
rebar3 eunit
rebar3 ct
```

## License

PMPL-1.0 (Polymath Public License)
