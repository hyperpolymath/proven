# Proven Io Binding

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

FFI binding for the [Io](https://iolanguage.org/) programming language,
providing access to the formally verified **libproven** safety library.

All computation is performed in the verified Idris 2 core via the Zig FFI layer.
This binding is a thin marshaling layer; it does **not** reimplement any logic.

## Architecture

```
Io script (.io)
    |
    v
IoProven.c  (C addon registered via IoObject_addMethodTable_)
    |
    v
libproven.so  (Zig FFI -> Idris 2 verified core)
```

## Building

Compile the C addon against Io headers and libproven:

```bash
cc -shared -fPIC -o IoProven.so src/IoProven.c \
    -I/path/to/io/include \
    -L/path/to/libproven -lproven -liovmall
```

## Usage

```io
# Load the addon (the exact mechanism depends on your Io setup)
# The C addon registers the Proven prototype automatically.

Proven init

# -- SafeMath: overflow-checked arithmetic --
result := Proven safeAdd(1000000000, 2000000000)
if(result isNil,
    "Overflow!" println,
    ("Sum: " .. result asString) println
)

# Division by zero returns nil, not a crash
bad := Proven safeDiv(42, 0)
if(bad isNil, "Division by zero handled safely" println)

# -- SafeString: encoding-safe text operations --
safe := Proven escapeHtml("<script>alert(1)</script>")
safe println   # "&lt;script&gt;alert(1)&lt;/script&gt;"

# -- SafeEmail: email validation --
valid := Proven isValidEmail("user@example.com")
valid println  # true

# -- SafeUrl: URL parsing --
parts := Proven parseUrl("https://example.com:8080/path?q=1")
if(parts isNil not,
    ("Host: " .. parts host) println
    ("Port: " .. parts port asString) println
)

# -- SafeCrypto: secure operations --
eq := Proven constantTimeEq("secret", "secret")
eq println  # true
hex := Proven randomHex(16)
hex println  # 32-char random hex string

# -- SafeJson: JSON validation --
Proven jsonIsValid("{\"key\": 42}") println  # true
Proven jsonGetType("[1, 2, 3]") println      # "array"

# -- SafeFloat: safe floating-point --
Proven floatDiv(10.0, 3.0) println   # 3.333...
Proven floatSqrt(144.0) println      # 12.0

# -- Cleanup --
Proven deinit
```

## Available Methods

### Lifecycle
| Method | Parameters | Returns |
|--------|-----------|---------|
| `init` | none | Number (status) |
| `deinit` | none | nil |
| `isInitialized` | none | true/false |
| `version` | none | Sequence |

### SafeMath
| Method | Parameters | Returns |
|--------|-----------|---------|
| `safeAdd(a, b)` | Number, Number | Number or nil |
| `safeSub(a, b)` | Number, Number | Number or nil |
| `safeMul(a, b)` | Number, Number | Number or nil |
| `safeDiv(a, b)` | Number, Number | Number or nil |
| `safeMod(a, b)` | Number, Number | Number or nil |
| `safeAbs(n)` | Number | Number or nil |
| `clamp(lo, hi, val)` | Number, Number, Number | Number |
| `safePow(base, exp)` | Number, Number | Number or nil |

### SafeString
| Method | Parameters | Returns |
|--------|-----------|---------|
| `isValidUtf8(str)` | Sequence | true/false or nil |
| `escapeSql(str)` | Sequence | Sequence or nil |
| `escapeHtml(str)` | Sequence | Sequence or nil |
| `escapeJs(str)` | Sequence | Sequence or nil |
| `pathHasTraversal(path)` | Sequence | true/false or nil |
| `sanitizeFilename(name)` | Sequence | Sequence or nil |

### SafeEmail
| Method | Parameters | Returns |
|--------|-----------|---------|
| `isValidEmail(email)` | Sequence | true/false or nil |

### SafeUrl
| Method | Parameters | Returns |
|--------|-----------|---------|
| `parseUrl(url)` | Sequence | Object or nil |
| `urlEncode(str)` | Sequence | Sequence or nil |
| `urlDecode(str)` | Sequence | Sequence or nil |

### SafeCrypto
| Method | Parameters | Returns |
|--------|-----------|---------|
| `constantTimeEq(a, b)` | Sequence, Sequence | true/false or nil |
| `randomHex(n)` | Number | Sequence or nil |
| `hexEncode(data)` | Sequence | Sequence or nil |
| `hexDecode(hex)` | Sequence | Sequence or nil |
| `crc32(data)` | Sequence | Number or nil |

### SafeJson
| Method | Parameters | Returns |
|--------|-----------|---------|
| `jsonIsValid(json)` | Sequence | true/false or nil |
| `jsonGetType(json)` | Sequence | Sequence |

### SafeFloat
| Method | Parameters | Returns |
|--------|-----------|---------|
| `floatDiv(a, b)` | Number, Number | Number or nil |
| `floatSqrt(x)` | Number | Number or nil |
| `floatLn(x)` | Number | Number or nil |
| `floatIsFinite(x)` | Number | true/false |
| `floatIsNan(x)` | Number | true/false |

### SafeColor
| Method | Parameters | Returns |
|--------|-----------|---------|
| `parseColor(hex)` | Sequence | Object {r,g,b} or nil |
| `colorToHex(r,g,b)` | Number, Number, Number | Sequence or nil |

### Other
| Method | Parameters | Returns |
|--------|-----------|---------|
| `versionCompare(a, b)` | Sequence, Sequence | Number or nil |
| `validatePassword(str)` | Sequence | Object or nil |
| `isCommonPassword(str)` | Sequence | true/false |
| `parseIpv4(str)` | Sequence | Object or nil |
| `calcEval(expr)` | Sequence | Number or nil |

## Error Handling

All methods return `nil` on error instead of raising exceptions. This
follows Io's prototype-based philosophy of returning sentinel values.

```io
result := Proven safeDiv(10, 0)
if(result isNil,
    "Operation failed safely" println,
    result println
)
```

## Prototype Organization

The `.io` files provide documentation and organizational structure:

- `proven/Proven.io` - Main prototype (namespace)
- `proven/SafeMath.io` - Safe integer arithmetic
- `proven/SafeString.io` - Encoding-safe text operations
- `proven/SafeEmail.io` - Email validation
- `proven/SafeUrl.io` - URL parsing and encoding
- `proven/SafeCrypto.io` - Cryptographic primitives
- `proven/SafeJson.io` - JSON validation

All actual method implementations are in the native C addon (`src/IoProven.c`).

## License

PMPL-1.0-or-later
