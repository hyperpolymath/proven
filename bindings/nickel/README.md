# proven - Nickel Bindings

> Code that cannot crash. Mathematically proven safe operations for Nickel.

## Installation

Copy the `proven/` directory to your project, or import directly:

```nickel
let proven = import "path/to/proven.ncl" in
```

## Modules

- **SafeMath** - Overflow-checked arithmetic with result types
- **SafeString** - XSS, SQL, and shell injection prevention
- **SafePath** - Path operations without traversal attacks
- **SafeEmail** - RFC 5321 compliant email validation
- **SafeNetwork** - IPv4, IPv6, CIDR, hostname, and port validation
- **SafeConfig** - Configuration-specific contracts and utilities
- **SafeUuid** - RFC 4122 UUID validation and parsing
- **SafeHex** - Hexadecimal validation and hash contracts

## Usage

### Safe Arithmetic

```nickel
let { SafeMath, .. } = import "proven.ncl" in

let result = SafeMath.safe_div 10 0 in
# result = { value = 0, ok = false }

let result2 = SafeMath.safe_add 5 3 in
# result2 = { value = 8, ok = true }
```

### Contracts

```nickel
let { SafeNetwork, SafeConfig, .. } = import "proven.ncl" in

{
  server = {
    host | SafeNetwork.IPv4 = "192.168.1.1",
    port | SafeNetwork.Port = 8080,
    workers | SafeConfig.WorkerCount = 4,
    log_level | SafeConfig.LogLevel = "info",
  }
}
```

### XSS Prevention

```nickel
let { SafeString, .. } = import "proven.ncl" in

let user_input = "<script>alert('xss')</script>" in
let safe = SafeString.escape_html user_input in
# safe = "&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;"
```

### Path Safety

```nickel
let { SafePath, .. } = import "proven.ncl" in

let result = SafePath.join_safe "/var/uploads" "../../../etc/passwd" in
# result = { path = "", ok = false, error = "Path traversal detected" }

let safe_result = SafePath.join_safe "/var/uploads" "user-file.txt" in
# safe_result = { path = "/var/uploads/user-file.txt", ok = true, error = "" }
```

### Configuration Parsing

```nickel
let { SafeConfig, .. } = import "proven.ncl" in

let timeout = SafeConfig.parse_duration "30s" in
# timeout = { value = 30, ok = true }

let memory = SafeConfig.parse_memory "512M" in
# memory = { value = 536870912, ok = true }
```

## Available Contracts

| Module | Contract | Description |
|--------|----------|-------------|
| SafeMath | `Percentage` | 0-100 |
| SafeMath | `Port` | 0-65535 |
| SafeMath | `Positive` | > 0 |
| SafeMath | `NonNegative` | >= 0 |
| SafeString | `NonEmpty` | Non-empty string |
| SafeString | `MaxLength(n)` | String with max length |
| SafeString | `Identifier` | Valid identifier |
| SafePath | `SafePath` | No traversal sequences |
| SafePath | `RelativePath` | Relative, safe path |
| SafePath | `Filename` | Valid filename |
| SafeEmail | `Email` | Valid email address |
| SafeNetwork | `IPv4` | IPv4 address |
| SafeNetwork | `IPv6` | IPv6 address |
| SafeNetwork | `Port` | Port number |
| SafeNetwork | `Hostname` | Valid hostname |
| SafeNetwork | `CIDR` | CIDR notation |
| SafeConfig | `Duration` | Positive duration |
| SafeConfig | `Bytes` | Non-negative bytes |
| SafeConfig | `LogLevel` | trace/debug/info/warn/error/fatal |
| SafeConfig | `Environment` | development/staging/production/test |
| SafeConfig | `Url` | HTTP(S) URL |
| SafeConfig | `SecureUrl` | HTTPS URL only |
| SafeConfig | `SemVer` | Semantic version |
| SafeUuid | `UUID` | Any UUID version |
| SafeUuid | `UUIDv4` | UUID version 4 |
| SafeHex | `SHA256` | 64-char hex string |
| SafeHex | `SHA512` | 128-char hex string |

## License

PMPL-1.0 (Palimpsest-MPL-1.0)
