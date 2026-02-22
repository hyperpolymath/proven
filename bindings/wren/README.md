# Proven for Wren

FFI binding to libproven -- a formally verified safety library. All computation
is performed in verified Idris 2 code via the Zig FFI layer. This binding is a
thin C bridge module; it does NOT reimplement any logic.

## Requirements

- Wren VM (https://wren.io/)
- libproven shared library (`libproven.so` / `libproven.dylib` / `proven.dll`)
  installed and accessible on the library search path

## Building

Compile the C bridge module against both libproven and libwren:

```bash
cc -shared -fPIC -o proven_wren.so src/proven_wren.c -lproven -lwren
```

## Integration

Register the foreign method binder in your Wren host application:

```c
#include <wren.h>

// Declared in proven_wren.c
extern WrenForeignMethodFn provenBindForeignMethod(
    WrenVM* vm,
    const char* module,
    const char* className,
    bool isStatic,
    const char* signature);

int main(void) {
    WrenConfiguration config;
    wrenInitConfiguration(&config);
    config.bindForeignMethodFn = provenBindForeignMethod;

    WrenVM* vm = wrenNewVM(&config);
    // Load and run .wren files...
    wrenFreeVM(vm);
    return 0;
}
```

## Usage

```wren
import "proven" for Proven, SafeMath, SafeString, SafeEmail, SafeUrl,
                     SafeCrypto, SafeJson

// Initialize the runtime
Proven.init()

// Safe math with overflow checking
var sum = SafeMath.addChecked(1000000000, 2000000000)
if (sum != null) {
    System.print("Sum: %(sum)")
}

// Division by zero returns null
var div = SafeMath.div(10, 0)
if (div == null) {
    System.print("Division by zero handled safely")
}

// XSS prevention
var escaped = SafeString.escapeHtml("<script>alert('xss')</script>")
if (escaped != null) {
    System.print("Escaped: %(escaped)")
}

// Path traversal detection
var hasTrav = SafeString.hasTraversal("../../../etc/passwd")
if (hasTrav) {
    System.print("Traversal detected!")
}

// Email validation
var valid = SafeEmail.isValid("user@example.com")
if (valid) {
    System.print("Valid email")
}

// URL parsing
var url = SafeUrl.parse("https://example.com:8080/path?q=1#frag")
if (url != null) {
    System.print("Scheme: %(url[0]), Host: %(url[1]), Port: %(url[2])")
}

// JSON validation
var jsonOk = SafeJson.isValid("{\"key\": \"value\"}")
if (jsonOk) {
    var jtype = SafeJson.getType("{\"key\": \"value\"}")
    System.print("Valid JSON of type: %(jtype)")
}

// Cleanup
Proven.deinit()
```

## Modules

| Class | Description |
|-------|-------------|
| `Proven` | Runtime lifecycle (init, deinit, version) |
| `SafeMath` | Overflow-checked arithmetic (add, sub, mul, div, mod, abs, pow, clamp) |
| `SafeString` | HTML/SQL/JS escaping, UTF-8 validation, path traversal detection |
| `SafeEmail` | Email address validation (RFC 5321) |
| `SafeUrl` | URL parsing into [scheme, host, port, path, query, fragment] |
| `SafeCrypto` | Constant-time byte comparison (timing-attack safe) |
| `SafeJson` | JSON validation and root type detection |

## Error Handling

All fallible operations return `null` on error. Check for null before using:

```wren
var result = SafeMath.div(a, b)
if (result != null) {
    // Use result safely
} else {
    // Handle error
}
```

## Architecture

```
Wren (this binding)
  |
  v  Wren foreign methods (C bridge)
libproven.so
  |
  v  Zig FFI bridge
Idris 2 (formally verified core)
```

## License

PMPL-1.0-or-later
