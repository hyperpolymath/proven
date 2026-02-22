# Proven -- Unison Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

Unison bindings for **libproven**, a formally verified safety library.
All computation is performed in the Idris 2 / Zig core via the `proven-cli`
command-line tool.

Unison is a content-addressed language without traditional FFI. These
bindings use the `IO` ability and `Process.exec` to shell out to `proven-cli`.

**No logic is reimplemented.** Every function calls `proven-cli` via IO.

## Prerequisites

- [Unison](https://www.unison-lang.org/) (UCM)
- `proven-cli` binary on PATH (built from the proven repository)

## Usage

```unison
-- Add the proven namespace to your codebase via UCM
-- .> pull git(https://github.com/hyperpolymath/proven) .proven

-- Safe arithmetic
result = proven.math.safeAdd 9223372036854775800 100
match result with
  Some n -> printLine (Int.toText n)
  None   -> printLine "Overflow detected"

-- Email validation
valid = proven.email.isValid "user@example.com"
printLine (Boolean.toText valid)

-- JSON validation
isJson = proven.json.isValid "{\"key\": \"value\"}"
printLine (Boolean.toText isJson)
```

## Modules

| Module | Description |
|--------|-------------|
| `proven/LibProven.u` | Core helpers, CLI runner, status codes |
| `proven/SafeMath.u` | Checked arithmetic (add, sub, mul, div, mod, abs, pow) |
| `proven/SafeString.u` | String escaping (SQL, HTML, JavaScript) and UTF-8 validation |
| `proven/SafeEmail.u` | Email address validation (RFC 5321) |
| `proven/SafeUrl.u` | URL parsing and IPv4 validation |
| `proven/SafeCrypto.u` | Constant-time comparison, hex encode/decode, random bytes |
| `proven/SafeJson.u` | JSON validation and type detection |

## Error Handling

All functions that can fail return `Optional a`:

- `None` indicates an error (overflow, invalid input, CLI failure, etc.)
- `Some value` indicates success

## Architecture

```
Unison (IO ability)  -->  proven-cli binary  -->  libproven  -->  Zig FFI  -->  Idris 2
       ^                        ^                                                  ^
  this package            CLI wrapper                                     formally verified
```

## Limitations

Since Unison communicates with proven via CLI, there is per-call overhead
from process spawning. For high-throughput use cases, consider using a
language binding with direct C FFI (Nelua, Rust, Zig, etc.).

The `proven-cli` binary must be on PATH or the path can be overridden
by modifying `proven.cliPath` in `LibProven.u`.
