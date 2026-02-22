# Proven Squirrel Binding

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

FFI binding for the [Squirrel](http://squirrel-lang.org/) scripting language,
providing access to the formally verified **libproven** safety library.

All computation is performed in the verified Idris 2 core via the Zig FFI layer.
This binding is a thin marshaling layer; it does **not** reimplement any logic.

## Architecture

```
Squirrel script (.nut)
    |
    v
proven_squirrel.cpp  (native closures registered via sq_newclosure)
    |
    v
libproven.so         (Zig FFI -> Idris 2 verified core)
```

## Building

Compile the C++ module against Squirrel headers and libproven:

```bash
g++ -shared -fPIC -o proven_squirrel.so src/proven_squirrel.cpp \
    -I/path/to/squirrel/include \
    -L/path/to/libproven -lproven \
    -lsquirrel -lsqstdlib
```

## Integration

In your host application (C/C++), after creating the Squirrel VM:

```cpp
#include <squirrel.h>

// Declared in proven_squirrel.cpp
extern "C" SQRESULT proven_squirrel_register(HSQUIRRELVM v);

int main() {
    HSQUIRRELVM v = sq_open(1024);
    // ... set up print function, etc. ...

    proven_squirrel_register(v);

    // Now Squirrel scripts can use the "proven" table
    // sq_dofile(v, "game_script.nut", SQFalse, SQTrue);

    sq_close(v);
    return 0;
}
```

## Usage in Squirrel

### Direct table access

```squirrel
// Initialize the runtime
proven.init();

// Safe arithmetic
local sum = proven.safe_add(1000000000, 2000000000);
if (sum != null) print("Sum: " + sum);

// Division by zero returns null, not a crash
local bad = proven.safe_div(42, 0);
if (bad == null) print("Division by zero handled safely");

// Validate email
if (proven.validate_email("user@example.com"))
    print("Valid email");

// Parse a URL
local parts = proven.parse_url("https://example.com:8080/path?q=1");
if (parts != null)
    print("Host: " + parts.host + " Port: " + parts.port);

// Cleanup
proven.deinit();
```

### OOP class wrappers

Load the `.nut` class files for a more structured API:

```squirrel
// Classes defined in src/*.nut
local math   = SafeMath();
local str    = SafeString();
local email  = SafeEmail();
local url    = SafeUrl();
local crypto = SafeCrypto();
local json   = SafeJson();

// Overflow-checked arithmetic
local result = math.add(2147483647, 1);  // null (overflow)

// XSS prevention
local safe = str.escape_html("<script>alert(1)</script>");

// JSON validation
if (json.is_valid(input_data))
    print("Valid JSON of type: " + json.get_type(input_data));
```

## Available Functions

### Lifecycle
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.init()` | none | integer (status) |
| `proven.deinit()` | none | null |
| `proven.is_initialized()` | none | bool |
| `proven.version()` | none | string |

### SafeMath
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.safe_add(a, b)` | integer, integer | integer or null |
| `proven.safe_sub(a, b)` | integer, integer | integer or null |
| `proven.safe_mul(a, b)` | integer, integer | integer or null |
| `proven.safe_div(a, b)` | integer, integer | integer or null |
| `proven.safe_mod(a, b)` | integer, integer | integer or null |
| `proven.safe_abs(n)` | integer | integer or null |
| `proven.safe_clamp(lo, hi, val)` | integer, integer, integer | integer |
| `proven.safe_pow(base, exp)` | integer, integer | integer or null |

### SafeString
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.is_valid_utf8(str)` | string | bool or null |
| `proven.escape_sql(str)` | string | string or null |
| `proven.escape_html(str)` | string | string or null |
| `proven.escape_js(str)` | string | string or null |
| `proven.path_has_traversal(path)` | string | bool or null |
| `proven.sanitize_filename(name)` | string | string or null |

### SafeEmail
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.validate_email(email)` | string | bool or null |

### SafeUrl
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.parse_url(url)` | string | table or null |
| `proven.url_encode(str)` | string | string or null |
| `proven.url_decode(str)` | string | string or null |

### SafeCrypto
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.constant_time_eq(a, b)` | string, string | bool or null |
| `proven.random_hex(nbytes)` | integer | string or null |
| `proven.hex_encode(str)` | string | string or null |
| `proven.hex_decode(hex)` | string | string or null |
| `proven.crc32(data)` | string | integer or null |

### SafeJson
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.json_is_valid(json)` | string | bool or null |
| `proven.json_get_type(json)` | string | string |

### SafeFloat
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.float_div(a, b)` | float, float | float or null |
| `proven.float_sqrt(x)` | float | float or null |
| `proven.float_ln(x)` | float | float or null |
| `proven.float_is_finite(x)` | float | bool |
| `proven.float_is_nan(x)` | float | bool |

### SafeColor
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.parse_color(hex)` | string | table {r,g,b} or null |
| `proven.color_to_hex(r,g,b)` | int, int, int | string or null |

### Other
| Function | Parameters | Returns |
|----------|-----------|---------|
| `proven.version_compare(a, b)` | string, string | integer or null |
| `proven.validate_password(str)` | string | table or null |
| `proven.is_common_password(str)` | string | bool |
| `proven.parse_ipv4(str)` | string | table or null |
| `proven.calc_eval(expr)` | string | float or null |

## Error Handling

All functions return `null` on error instead of throwing exceptions.
This design is intentional for game engine integration where exceptions
may not be available or desired.

```squirrel
local result = proven.safe_div(10, 0);
if (result == null) {
    // Handle error gracefully
    print("Operation failed safely");
}
```

## License

PMPL-1.0-or-later
