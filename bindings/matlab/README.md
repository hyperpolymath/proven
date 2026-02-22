<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - MATLAB/Octave Binding

FFI binding to **libproven**, a formally verified safety library implemented
in Idris 2 with a Zig FFI bridge exposing a stable C ABI.

**All computation is performed in the Idris 2 core.  This binding is a thin
wrapper; it does NOT reimplement any logic.**

## Requirements

- MATLAB R2019b+ or GNU Octave 6.0+
- `libproven.so` (Linux), `libproven.dylib` (macOS), or `proven.dll` (Windows)
  on the system library path
- `proven_header.h` (included in this directory)

## Quick Start

```matlab
% Add the binding directory to the MATLAB path.
addpath('/path/to/proven/bindings/matlab');

% Load and initialise.
proven.LibProven.init();

% Safe arithmetic.
result = proven.SafeMath.add(int64(2), int64(3));         % 5
result = proven.SafeMath.div(int64(10), int64(0));        % []  (div by zero)

% String safety.
html = proven.SafeString.escapeHtml('<script>alert(1)</script>');

% Email validation.
tf = proven.SafeEmail.isValid('user@example.com');        % true

% Path traversal detection.
tf = proven.SafePath.hasTraversal('../../etc/passwd');     % true

% JSON validation.
tf = proven.SafeJson.isValid('{"key": "value"}');         % true

% URL encoding.
encoded = proven.SafeUrl.encode('hello world');           % 'hello%20world'

% Cryptographic random bytes.
bytes = proven.SafeCrypto.randomBytes(32);                % 1x32 uint8

% Date/time.
tf = proven.SafeDateTime.isLeapYear(2024);                % true

% Clean up.
proven.LibProven.deinit();
proven.LibProven.unload();
```

## Error Handling

All methods return `[]` (empty matrix) on error, which is the idiomatic
MATLAB/Octave convention.  Check with `isempty()`:

```matlab
result = proven.SafeMath.div(int64(10), int64(0));
if isempty(result)
    fprintf('Division by zero detected safely.\n');
end
```

## Modules

| Class | Description |
|-------|-------------|
| `proven.LibProven` | Library loader and lifecycle management |
| `proven.SafeMath` | Overflow-safe integer arithmetic |
| `proven.SafeString` | XSS/SQL/JS escaping, UTF-8 validation |
| `proven.SafePath` | Directory traversal prevention |
| `proven.SafeEmail` | RFC 5321 email validation |
| `proven.SafeUrl` | RFC 3986 URL encoding/decoding |
| `proven.SafeNetwork` | IPv4 parsing and classification |
| `proven.SafeCrypto` | Constant-time comparison, random bytes, hex encoding |
| `proven.SafeJson` | JSON validation and type detection |
| `proven.SafeDateTime` | Leap year, days-in-month, expression evaluation |

## Architecture

```
MATLAB/Octave  -->  loadlibrary/calllib  -->  libproven.so
                                                   |
                                              Zig FFI bridge
                                                   |
                                              Idris 2 core
                                          (formally verified)
```

## License

PMPL-1.0-or-later
