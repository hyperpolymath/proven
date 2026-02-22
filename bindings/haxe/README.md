# Proven for Haxe

FFI binding to libproven -- a formally verified safety library. All computation
is performed in verified Idris 2 code via the Zig FFI layer. This binding is a
thin wrapper that calls libproven via CFFI (hxcpp target); it does NOT
reimplement any logic.

## Requirements

- Haxe 4.x with hxcpp target
- libproven shared library (`libproven.so` / `libproven.dylib` / `proven.dll`)
  installed and accessible on the library search path

## Installation

```bash
haxelib install proven
```

Or from Git:

```bash
haxelib git proven https://github.com/hyperpolymath/proven.git bindings/haxe
```

## Usage

```haxe
import proven.LibProven;
import proven.SafeMath;
import proven.SafeString;
import proven.SafePath;
import proven.SafeEmail;
import proven.SafeUrl;
import proven.SafeJson;

class Main {
    static function main() {
        // Initialize the runtime
        LibProven.init();

        // Safe math with overflow checking
        var sum = SafeMath.addChecked(haxe.Int64.make(0, 1000000000),
                                      haxe.Int64.make(0, 2000000000));
        if (sum != null) {
            trace("Sum: " + haxe.Int64.toStr(sum));
        }

        // Division by zero returns null instead of crashing
        var div = SafeMath.div(haxe.Int64.make(0, 10), haxe.Int64.make(0, 0));
        if (div == null) {
            trace("Division by zero handled safely");
        }

        // XSS prevention
        var escaped = SafeString.escapeHtml("<script>alert('xss')</script>");
        if (escaped != null) {
            trace("Escaped: " + escaped);
        }

        // Path traversal detection
        var hasTrav = SafePath.hasTraversal("../../../etc/passwd");
        if (hasTrav == true) {
            trace("Traversal detected");
        }

        // Email validation
        var valid = SafeEmail.isValid("user@example.com");
        if (valid == true) {
            trace("Valid email");
        }

        // URL parsing
        var url = SafeUrl.parse("https://example.com:8080/path?q=1#frag");
        if (url != null) {
            trace("Host: " + url.host + ", Port: " + Std.string(url.port));
        }

        // JSON validation
        var jsonOk = SafeJson.isValid('{"key": "value"}');
        if (jsonOk == true) {
            trace("Valid JSON of type: " + SafeJson.getType('{"key": "value"}').toString());
        }

        // Cleanup
        LibProven.deinit();
    }
}
```

## Modules

| Module | Description |
|--------|-------------|
| `LibProven` | Low-level CFFI declarations and lifecycle management |
| `SafeMath` | Overflow-checked arithmetic (add, sub, mul, div, mod, abs, pow) |
| `SafeString` | String escaping for SQL, HTML, and JavaScript; UTF-8 validation |
| `SafePath` | Directory traversal detection and filename sanitization |
| `SafeEmail` | Email address validation (RFC 5321) |
| `SafeUrl` | URL parsing into scheme, host, port, path, query, fragment |
| `SafeNetwork` | IPv4 parsing, private/loopback classification |
| `SafeCrypto` | Constant-time byte comparison (timing-attack safe) |
| `SafeJson` | JSON validation and root type detection |
| `SafeDateTime` | Leap year and days-in-month calculations |

## Error Handling

All fallible operations return `Null<T>` where `null` indicates an error.
Check for null before using the result:

```haxe
var result = SafeMath.div(a, b);
if (result != null) {
    // Use result safely
} else {
    // Handle error (division by zero, overflow, etc.)
}
```

For detailed error information, use the low-level `LibProven` methods that
return `IntResult`, `BoolResult`, `FloatResult`, or `StringResult` with a
`ProvenStatus` code.

## Architecture

```
Haxe (this binding)
  |
  v  CFFI / hxcpp
libproven.so
  |
  v  Zig FFI bridge
Idris 2 (formally verified core)
```

## License

PMPL-1.0-or-later
