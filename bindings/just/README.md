# Proven Safety Library - Just Binding

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

[Just](https://github.com/casey/just) command runner recipes that wrap
`proven-cli`. Every recipe delegates ALL computation to the formally verified
Idris 2 core via the Zig FFI bridge. No safety-critical logic is reimplemented
in this binding.

## Prerequisites

- [just](https://github.com/casey/just) >= 1.0
- `proven-cli` on `PATH` (built from `ffi/zig/`), or set `PROVEN_CLI` env var

## Files

| File | Purpose |
|------|---------|
| `justfile` | Main entry point with meta-recipes and batch helpers |
| `proven.just` | Importable module with all proven operations |

## Quick Start

```bash
# Check proven-cli is available
just check

# Safe math
just safe-add 100 200
just safe-div 100 0     # Returns error, not crash

# Validation
just validate-email "user@example.com"
just validate-url "https://example.com"
just validate-ipv4 "192.168.1.1"

# Crypto
just hash-sha256 "hello world"
just random-hex 32

# Formatting
just format-datetime "2026-01-15T10:30:00Z"
just parse-color "#FF5733"
just version-compare "1.2.3" "2.0.0"
```

## Importing into Your Project

Add to your own justfile:

```just
import '/path/to/proven/bindings/just/proven.just'
```

Then all `proven.just` recipes are available directly:

```bash
just safe-add 10 20
just validate-email "test@example.com"
```

## Batch Operations

The main `justfile` includes batch helpers for processing files:

```bash
# Validate every email in a file (one per line)
just validate-emails-file emails.txt

# Validate every URL in a file
just validate-urls-file urls.txt
```

## Available Recipes

### Math

| Recipe | Description |
|--------|-------------|
| `safe-add a b` | Overflow-checked addition |
| `safe-sub a b` | Underflow-checked subtraction |
| `safe-mul a b` | Overflow-checked multiplication |
| `safe-div a b` | Division (no div-by-zero crash) |
| `safe-mod a b` | Modulo (no div-by-zero crash) |
| `safe-abs n` | Absolute value (handles MIN_INT) |
| `safe-pow base exp` | Overflow-checked exponentiation |
| `safe-clamp min max value` | Clamp to range |
| `float-div a b` | Safe float division |
| `float-sqrt n` | Square root (rejects negatives) |
| `float-ln n` | Natural log (rejects non-positive) |
| `calc-eval expr` | Evaluate arithmetic expression |

### Validation

| Recipe | Description |
|--------|-------------|
| `validate-email addr` | RFC 5321 email validation |
| `validate-url url` | URL validation |
| `validate-ipv4 ip` | IPv4 address validation |
| `ipv4-is-private ip` | RFC 1918 private check |
| `ipv4-is-loopback ip` | Loopback check |
| `validate-path path` | Directory traversal detection |
| `sanitize-filename name` | Remove dangerous filename chars |
| `validate-json input` | JSON syntax validation |
| `validate-json-file file` | Validate JSON from file |
| `json-type input` | Determine JSON value type |
| `validate-password pwd` | Password strength check |
| `is-common-password pwd` | Common password list check |
| `validate-phone number` | Phone number validation |

### String

| Recipe | Description |
|--------|-------------|
| `sanitize-string input` | HTML-escape dangerous characters |
| `escape-sql input` | SQL injection prevention |
| `escape-js input` | JS injection prevention |
| `is-valid-utf8 input` | UTF-8 validity check |

### Crypto

| Recipe | Description |
|--------|-------------|
| `hash-sha256 input` | SHA-256 hash |
| `constant-time-eq a b` | Timing-safe comparison |
| `random-hex n` | Random bytes (hex-encoded) |
| `checksum-crc32 input` | CRC32 checksum |
| `hex-encode input` | String to hex |
| `hex-decode input` | Hex to string |

### Formatting and Parsing

| Recipe | Description |
|--------|-------------|
| `format-datetime dt` | Parse ISO 8601 datetime |
| `is-leap-year year` | Leap year check |
| `days-in-month year month` | Days in a month |
| `parse-color hex` | Parse hex color to RGB |
| `parse-version ver` | Parse semver string |
| `version-compare a b` | Compare two semver strings |
| `deg-to-rad degrees` | Degrees to radians |
| `rad-to-deg radians` | Radians to degrees |
| `convert-length val from to` | Length unit conversion |
| `convert-temp val from to` | Temperature unit conversion |
| `uuid-v4` | Generate UUID v4 |
| `parse-uuid uuid` | Validate UUID |
| `parse-currency amount` | Parse currency string |

## Architecture

```
justfile / proven.just
        |
        v
   proven-cli          (Zig binary)
        |
        v
   libproven.so        (Zig FFI bridge)
        |
        v
   Proven (Idris 2)    (formally verified core)
```

## Raw Escape Hatch

For any proven-cli subcommand not wrapped as a recipe:

```bash
just proven-raw <module> <subcommand> [args...]
```
