# Proven Pike Binding

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

FFI binding for the [Pike](https://pike.lysator.liu.se/) programming language,
providing access to the formally verified **libproven** safety library.

All computation is performed in the verified Idris 2 core via the Zig FFI layer.
This binding is a thin marshaling layer; it does **not** reimplement any logic.

## Architecture

```
Pike script (.pike)
    |
    v
Proven.pmod/*.pike  (Pike module calling proven-cli)
    |
    v
proven-cli          (CLI wrapper around libproven)
    |
    v
libproven.so        (Zig FFI -> Idris 2 verified core)
```

Pike does not have direct dlopen/FFI support in its standard library.
This binding uses `Process.run()` to invoke the `proven-cli` command-line
tool, which wraps the same libproven C ABI and outputs JSON results.

For higher performance, a native C Pike module can be compiled (see the
"Native C Module" section below).

## Installation

1. Ensure `proven-cli` is in your PATH, or set `LibProven.cli_path`.
2. Copy the `src/Proven.pmod/` directory to your Pike module path.

```bash
cp -r src/Proven.pmod /usr/local/lib/pike/modules/
```

## Usage

```pike
import Proven;

int main()
{
    // Initialize the runtime
    LibProven.init();

    // -- SafeMath: overflow-checked arithmetic --
    int|zero sum = SafeMath.add(1000000000, 2000000000);
    if (!zero_type(sum))
        write("Sum: %d\n", sum);

    // Division by zero returns UNDEFINED, not an error
    int|zero bad = SafeMath.div(42, 0);
    if (zero_type(bad))
        write("Division by zero handled safely\n");

    // -- SafeString: encoding-safe text operations --
    string|zero safe = SafeString.escape_html("<script>alert(1)</script>");
    if (safe)
        write("Safe: %s\n", safe);

    // -- SafeEmail: email validation --
    if (SafeEmail.is_valid("user@example.com"))
        write("Valid email\n");

    // -- SafePath: filesystem traversal prevention --
    if (SafePath.has_traversal("../../etc/passwd"))
        write("Traversal attack detected!\n");

    // -- SafeUrl: URL parsing --
    mapping|zero parts = SafeUrl.parse("https://example.com:8080/path?q=1");
    if (parts)
        write("Host: %s\n", parts->host);

    // -- SafeCrypto: secure operations --
    if (SafeCrypto.constant_time_eq("secret", "secret"))
        write("Secrets match\n");
    string|zero hex = SafeCrypto.random_hex(16);
    if (hex)
        write("Random: %s\n", hex);

    // -- SafeJson: JSON validation --
    if (SafeJson.is_valid("{\"key\": 42}"))
        write("Valid JSON of type: %s\n", SafeJson.get_type("{\"key\": 42}"));

    // Cleanup
    LibProven.deinit();
    return 0;
}
```

## Module Structure

```
src/Proven.pmod/
    module.pmod       - Main module definition and version info
    LibProven.pike    - Low-level FFI bridge (Process.run -> proven-cli)
    SafeMath.pike     - Overflow-checked integer arithmetic
    SafeString.pike   - Encoding-safe text operations
    SafePath.pike     - Filesystem traversal prevention
    SafeEmail.pike    - Email validation
    SafeUrl.pike      - URL parsing and encoding
    SafeCrypto.pike   - Cryptographic primitives
    SafeJson.pike     - JSON validation
```

## Available Classes

### LibProven (Low-level)
| Method | Parameters | Returns |
|--------|-----------|---------|
| `init()` | none | void |
| `deinit()` | none | void |
| `is_initialized()` | none | int(0..1) |
| `call(func, args)` | string, array(string) | mapping or UNDEFINED |

### SafeMath
| Method | Parameters | Returns |
|--------|-----------|---------|
| `add(a, b)` | int, int | int or UNDEFINED |
| `sub(a, b)` | int, int | int or UNDEFINED |
| `mul(a, b)` | int, int | int or UNDEFINED |
| `div(a, b)` | int, int | int or UNDEFINED |
| `mod(a, b)` | int, int | int or UNDEFINED |
| `abs(n)` | int | int or UNDEFINED |
| `clamp(lo, hi, val)` | int, int, int | int |
| `pow(base, exp)` | int, int | int or UNDEFINED |

### SafeString
| Method | Parameters | Returns |
|--------|-----------|---------|
| `is_valid_utf8(str)` | string | int(0..1) or UNDEFINED |
| `escape_sql(str)` | string | string or UNDEFINED |
| `escape_html(str)` | string | string or UNDEFINED |
| `escape_js(str)` | string | string or UNDEFINED |

### SafePath
| Method | Parameters | Returns |
|--------|-----------|---------|
| `has_traversal(path)` | string | int(0..1) or UNDEFINED |
| `sanitize_filename(name)` | string | string or UNDEFINED |

### SafeEmail
| Method | Parameters | Returns |
|--------|-----------|---------|
| `is_valid(email)` | string | int(0..1) or UNDEFINED |

### SafeUrl
| Method | Parameters | Returns |
|--------|-----------|---------|
| `parse(url)` | string | mapping or UNDEFINED |
| `encode(str)` | string | string or UNDEFINED |
| `decode(str)` | string | string or UNDEFINED |

### SafeCrypto
| Method | Parameters | Returns |
|--------|-----------|---------|
| `constant_time_eq(a, b)` | string, string | int(0..1) or UNDEFINED |
| `random_hex(n)` | int | string or UNDEFINED |
| `hex_encode(data)` | string | string or UNDEFINED |
| `hex_decode(hex)` | string | string or UNDEFINED |
| `crc32(data)` | string | int or UNDEFINED |

### SafeJson
| Method | Parameters | Returns |
|--------|-----------|---------|
| `is_valid(json)` | string | int(0..1) or UNDEFINED |
| `get_type(json)` | string | string or UNDEFINED |

## Error Handling

All methods return `UNDEFINED` on error. Use Pike's `zero_type()` to
check for error conditions:

```pike
int|zero result = SafeMath.div(10, 0);
if (zero_type(result)) {
    write("Operation failed safely\n");
} else {
    write("Result: %d\n", result);
}
```

## Native C Module (Advanced)

For better performance, you can compile a native Pike C module that
calls libproven directly via dlopen. This avoids the overhead of
spawning a process for each operation.

```c
/* proven_pike.c - Native Pike C module (sketch) */
#include <pike/module.h>
#include <proven.h>

PIKE_MODULE_INIT {
    /* Register functions directly */
}
```

See the Pike documentation on writing C modules for details.

## License

PMPL-1.0-or-later
