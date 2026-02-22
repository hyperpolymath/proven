# Proven - Raku NativeCall Binding

Raku (Perl 6) binding to **libproven**, a formally verified safety library.

All computation is performed in Idris 2 (with dependent types and totality
checking) via a Zig FFI layer exposing a stable C ABI. This Raku package is a
thin NativeCall wrapper; it does **not** reimplement any logic.

## Prerequisites

- Raku 6.d or later
- `libproven.so` (or `libproven.dylib`) installed on the library search path

## Installation

```
zef install Proven
```

Or from source:

```
zef install .
```

## Usage

```raku
use Proven;

# Initialize the runtime (required before any other call)
proven-init();

# Safe arithmetic (returns Nil on overflow / division-by-zero)
say safe-add(9223372036854775800, 100);   # Nil (overflow)
say safe-div(10, 0);                       # Nil (division by zero)
say safe-div(10, 3);                       # 3

# String escaping
say escape-html('<script>alert(1)</script>');

# Email validation
say email-is-valid('user@example.com');     # True

# JSON validation
say json-is-valid('{"key": "value"}');      # True
say json-type-name('[1, 2, 3]');            # "array"

# Path traversal detection
say has-traversal('../../etc/passwd');       # True

# Cryptographic random bytes
my $buf = random-bytes(32);

# Shut down when done
proven-deinit();
```

## Modules

| Module                   | Purpose                                    |
|--------------------------|--------------------------------------------|
| `Proven::LibProven`      | Raw NativeCall declarations and CStruct types |
| `Proven::SafeMath`       | Checked integer arithmetic                 |
| `Proven::SafeString`     | UTF-8 validation and escaping (SQL, HTML, JS) |
| `Proven::SafePath`       | Directory traversal detection and filename sanitization |
| `Proven::SafeEmail`      | Email address validation (RFC 5321)        |
| `Proven::SafeUrl`        | URL percent-encoding and decoding          |
| `Proven::SafeNetwork`    | IPv4 address parsing and classification    |
| `Proven::SafeCrypto`     | Constant-time comparison, random bytes, hex, CRC32 |
| `Proven::SafeJson`       | JSON validation and type detection         |
| `Proven::SafeDateTime`   | ISO 8601 parsing, formatting, calendar utils |

## Error Handling

All fallible operations return `Nil` on error, following the idiomatic Raku
pattern for optional results. Use `with` / `without` or `//` to handle:

```raku
with safe-div($a, $b) -> $result {
    say "Result: $result";
} else {
    say "Division failed (probably division by zero)";
}
```

## License

PMPL-1.0-or-later

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
