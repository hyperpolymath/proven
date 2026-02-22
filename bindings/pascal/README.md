# Proven Pascal/Delphi Bindings

Pascal/Delphi FFI bindings for **libproven** -- a formally verified safety
library with an Idris 2 core and Zig FFI bridge.

All computation is performed by the verified Idris 2 core. These bindings are
thin wrappers that ONLY call libproven via `external 'proven'` declarations.
No logic is reimplemented in Pascal.

## Compatibility

- Free Pascal (FPC) 3.2+
- Delphi 10+
- Links dynamically against `libproven.so` (Linux), `libproven.dylib` (macOS),
  or `proven.dll` (Windows)

## Unit Structure

| Unit | Description |
|------|-------------|
| `LibProven` | Low-level C function declarations and ABI types |
| `SafeMath` | Checked integer arithmetic (add, sub, mul, div, mod, abs, pow, clamp) |
| `SafeString` | UTF-8 validation, SQL/HTML/JS string escaping |
| `SafePath` | Directory traversal detection, filename sanitization |
| `SafeEmail` | Email address validation (RFC 5321) |
| `SafeUrl` | URL parsing into components |
| `SafeNetwork` | IPv4 parsing, private/loopback detection |
| `SafeCrypto` | Constant-time comparison, secure random, hex encode/decode, CRC32 |
| `SafeJson` | JSON validation and root-level type detection |
| `SafeDateTime` | ISO 8601 parsing/formatting, leap year, days-in-month |
| `Proven` | Main unit: re-exports all modules, lifecycle management |

## Quick Start

```pascal
program Example;

uses
  Proven, SafeMath, SafeEmail;

var
  MathResult: TSafeIntResult;
  EmailResult: TSafeEmailResult;
begin
  ProvenInitialize;
  try
    // Safe arithmetic -- cannot overflow or crash
    MathResult := SafeAdd(High(Int64), 1);
    if not MathResult.Success then
      WriteLn('Overflow detected: ', ProvenStatusName(MathResult.ErrorCode));

    // Email validation
    EmailResult := ValidateEmail('user@example.com');
    if EmailResult.Success and EmailResult.IsValid then
      WriteLn('Valid email')
    else
      WriteLn('Invalid email');

  finally
    ProvenFinalize;
  end;
end.
```

## Building

### Free Pascal

```bash
fpc -Fu/path/to/proven/bindings/pascal/src -Fl/path/to/libproven example.pas
```

### Delphi

Add `bindings/pascal/src` to your unit search path and ensure `libproven` is
in your library search path.

## Memory Management

String results from libproven are automatically copied into Pascal `AnsiString`
values and the C-allocated memory is freed. You do not need to manually free
strings returned by the Safe* wrapper functions.

If using `LibProven` directly, you must call `proven_free_string` for any
`TProvenStringResult.Value`, `proven_url_free` for URL components,
`proven_version_free` for version results with prerelease strings, and
`proven_hex_free` for hex decode results.

## Error Handling

All wrapper functions return result records with a `Success: Boolean` field.
Check this before accessing the value. On failure, `ErrorCode` contains the
specific Proven status code. Use `ProvenStatusName()` to get a human-readable
name.

## License

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
