# Proven C# Bindings

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

C# P/Invoke bindings for **libproven**, the formally verified safety library.

All computation is performed in verified Idris 2 code via a Zig FFI bridge
exposing a stable C ABI. These bindings contain **only** marshaling logic
and **never** reimplement any algorithms.

## Modules

| Class | Description |
|-------|-------------|
| `SafeMath` | Overflow-checked integer/float arithmetic, exponentiation, expression evaluation |
| `SafeString` | UTF-8 validation, SQL/HTML/JS escaping, hex encoding, URL encoding/decoding |
| `SafePath` | Directory traversal detection, filename sanitization |
| `SafeEmail` | RFC 5321 email validation |
| `SafeUrl` | URL parsing into components (scheme, host, port, path, query, fragment) |
| `SafeNetwork` | IPv4 parsing, private/loopback range detection |
| `SafeCrypto` | Constant-time comparison, CSPRNG bytes, CRC32 checksums |
| `SafeJson` | JSON string validation, root-level type detection |
| `SafeDateTime` | ISO 8601 parsing/formatting, leap year, days-in-month |

## Requirements

- .NET 8.0 or later
- `libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows) on the library search path

## Usage

```csharp
using Proven;

// Initialize the runtime (call once at startup)
LibProven.proven_init();

// Safe arithmetic
long? sum = SafeMath.Add(long.MaxValue, 1);
// sum is null (overflow detected by Idris 2)

long? quotient = SafeMath.Div(10, 0);
// quotient is null (division by zero handled safely)

// String escaping (XSS prevention)
string? escaped = SafeString.EscapeHtml("<script>alert('xss')</script>");
// escaped is "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"

// Email validation
bool? valid = SafeEmail.IsValid("user@example.com");
// valid is true

// IPv4 parsing
IPv4Address? addr = SafeNetwork.ParseIPv4("192.168.1.1");
if (addr is not null)
{
    bool isPrivate = SafeNetwork.IsPrivate(addr.Value);
    // isPrivate is true
}

// JSON validation
bool? isJson = SafeJson.IsValid("{\"key\": \"value\"}");
// isJson is true

// ISO 8601 date/time
ProvenDateTime? dt = SafeDateTime.Parse("2026-02-22T14:30:00Z");
bool isLeap = SafeDateTime.IsLeapYear(2024);
// isLeap is true

// Cleanup (call once at shutdown)
LibProven.proven_deinit();
```

## Architecture

```
  .NET Application
       |
       v
  Proven (C# P/Invoke)   <-- This package (thin wrappers)
       |
       v
  libproven.so (C ABI)   <-- Zig FFI bridge
       |
       v
  Idris 2 (RefC)          <-- Formally verified core
```

## Licence

PMPL-1.0-or-later
