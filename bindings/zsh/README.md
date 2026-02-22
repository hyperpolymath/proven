# Proven Safety Library - Zsh Binding

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Zsh shell plugin that wraps `proven-cli` as native shell functions with full
tab-completion support. Compatible with Oh-My-Zsh, Zinit, Antigen, and manual
sourcing. ALL computation delegates to the formally verified Idris 2 core via
the Zig FFI bridge. No safety-critical logic is reimplemented in this binding.

## Prerequisites

- Zsh >= 5.0
- `proven-cli` on `PATH` (built from `ffi/zig/`), or set `PROVEN_CLI` env var

## Files

| File | Purpose |
|------|---------|
| `proven.zsh` | Main plugin file (auto-loads functions and completions) |
| `functions/proven-safe-math` | Safe math functions |
| `functions/proven-validate` | Validation functions |
| `functions/proven-crypto` | Crypto and encoding functions |
| `functions/proven-format` | Formatting, date, color, version functions |
| `_proven` | Zsh completion file for all subcommands |

## Installation

### Oh-My-Zsh

```bash
# Copy plugin directory
cp -r bindings/zsh $ZSH_CUSTOM/plugins/proven

# Add to ~/.zshrc
plugins=(... proven)

# Reload
source ~/.zshrc
```

### Zinit

```zsh
zinit light /path/to/proven/bindings/zsh
```

### Antigen

```zsh
antigen bundle /path/to/proven/bindings/zsh
```

### Manual

```bash
source /path/to/proven/bindings/zsh/proven.zsh
```

## Quick Start

```bash
# Check plugin status
proven-info

# Safe math (overflow-checked)
proven-safe-add 100 200       # => 300
proven-safe-div 100 0         # => error (not crash)
proven-safe-mul 999999 999999 # => overflow-checked

# Validation
proven-validate-email "user@example.com"
proven-validate-url "https://example.com"
proven-validate-ipv4 "192.168.1.1"
proven-validate-json '{"key": "value"}'
proven-validate-path "../etc/passwd"

# String sanitization
proven-sanitize-string "<script>alert(1)</script>"
proven-escape-sql "Robert'; DROP TABLE students;--"

# Crypto
proven-hash-sha256 "hello world"
proven-random-hex 32
proven-hex-encode "Hello"
proven-hex-decode "48656c6c6f"

# Formatting
proven-format-datetime "2026-01-15T10:30:00Z"
proven-parse-color "#FF5733"
proven-version-compare "1.2.3" "2.0.0"
proven-convert-temp 100 celsius fahrenheit
```

## Short Aliases

The plugin defines short aliases for common operations:

| Alias | Expands To |
|-------|-----------|
| `pv-add` | `proven-safe-add` |
| `pv-sub` | `proven-safe-sub` |
| `pv-mul` | `proven-safe-mul` |
| `pv-div` | `proven-safe-div` |
| `pv-mod` | `proven-safe-mod` |
| `pv-email` | `proven-validate-email` |
| `pv-url` | `proven-validate-url` |
| `pv-ip` | `proven-validate-ipv4` |
| `pv-sha256` | `proven-hash-sha256` |
| `pv-hex-enc` | `proven-hex-encode` |
| `pv-hex-dec` | `proven-hex-decode` |
| `pv-json` | `proven-validate-json` |
| `pv-ver` | `proven-version-compare` |

## Available Functions

### Math

| Function | Description |
|----------|-------------|
| `proven-safe-add a b` | Overflow-checked addition |
| `proven-safe-sub a b` | Underflow-checked subtraction |
| `proven-safe-mul a b` | Overflow-checked multiplication |
| `proven-safe-div a b` | Division (no div-by-zero crash) |
| `proven-safe-mod a b` | Modulo (no div-by-zero crash) |
| `proven-safe-abs n` | Absolute value (handles MIN_INT) |
| `proven-safe-pow base exp` | Overflow-checked exponentiation |
| `proven-safe-clamp min max val` | Clamp to range |
| `proven-float-div a b` | Safe float division |
| `proven-float-sqrt n` | Square root (rejects negatives) |
| `proven-float-ln n` | Natural log (rejects non-positive) |
| `proven-calc-eval expr` | Evaluate arithmetic expression |

### Validation

| Function | Description |
|----------|-------------|
| `proven-validate-email addr` | RFC 5321 email validation |
| `proven-validate-url url` | URL validation |
| `proven-validate-ipv4 ip` | IPv4 address validation |
| `proven-ipv4-is-private ip` | RFC 1918 private check |
| `proven-ipv4-is-loopback ip` | Loopback check |
| `proven-validate-path path` | Directory traversal detection |
| `proven-sanitize-filename name` | Remove dangerous filename chars |
| `proven-validate-json str` | JSON syntax validation |
| `proven-json-type str` | Determine JSON value type |
| `proven-validate-password pwd` | Password strength check |
| `proven-is-common-password pwd` | Common password list check |
| `proven-is-valid-utf8 str` | UTF-8 validity check |
| `proven-validate-phone num` | Phone number validation |
| `proven-validate-uuid str` | UUID validation |

### Crypto and Encoding

| Function | Description |
|----------|-------------|
| `proven-hash-sha256 input` | SHA-256 hash |
| `proven-constant-time-eq a b` | Timing-safe comparison |
| `proven-random-hex [n]` | Random bytes (hex, default 32) |
| `proven-checksum-crc32 input` | CRC32 checksum |
| `proven-hex-encode input` | String to hex |
| `proven-hex-decode hex` | Hex to string |

### Formatting and Parsing

| Function | Description |
|----------|-------------|
| `proven-format-datetime dt` | Parse ISO 8601 datetime |
| `proven-is-leap-year year` | Leap year check |
| `proven-days-in-month year month` | Days in a month |
| `proven-parse-color hex` | Parse hex color to RGB |
| `proven-parse-version ver` | Parse semver string |
| `proven-version-compare a b` | Compare two semver strings |
| `proven-deg-to-rad degrees` | Degrees to radians |
| `proven-rad-to-deg radians` | Radians to degrees |
| `proven-convert-length val from to` | Length unit conversion |
| `proven-convert-temp val from to` | Temperature unit conversion |
| `proven-uuid-v4` | Generate UUID v4 |
| `proven-parse-currency amount` | Parse currency string |
| `proven-sanitize-string input` | HTML-escape for XSS prevention |
| `proven-escape-sql input` | SQL injection prevention |
| `proven-escape-js input` | JS injection prevention |

### Meta

| Function | Description |
|----------|-------------|
| `proven-info` | Plugin version, CLI path, library version |

## Tab Completion

All functions have full Zsh tab-completion support. The `_proven` completion
file provides:

- Argument count validation with usage hints
- File path completion for `proven-validate-path`
- Unit name suggestions for `proven-convert-length` and `proven-convert-temp`
- Description-annotated function listings

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PROVEN_CLI` | `proven-cli` | Path to the proven-cli binary |

## Architecture

```
Zsh function (e.g. proven-safe-add)
    |
    v  (subprocess call)
proven-cli          (Zig binary)
    |
    v
libproven.so        (Zig FFI bridge)
    |
    v
Proven (Idris 2)    (formally verified core)
```

## Error Handling

All functions:
- Print results to stdout on success (exit code 0)
- Print error messages to stderr on failure (exit code 1)
- Print usage hints to stderr if called with wrong argument count
- Report if `proven-cli` is not found on PATH
