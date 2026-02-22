# Proven C++ Bindings

Thin, header-only C++ wrapper for the Proven library (Idris2 core, Zig FFI bridge).

All computation is performed by the formally verified Idris2 core. This binding
provides only RAII lifetime management, `std::optional` error handling, and C++
type safety. **No logic is reimplemented in C++.**

## Architecture

```
C++ headers (this binding)
    |
    v  extern "C" calls
libproven.so / libproven.a  (Zig FFI bridge)
    |
    v  calls compiled Idris2 RefC
Idris2 core  (formally verified with dependent types)
```

## Requirements

- C++17 or later
- libproven (built from `ffi/zig/`)

## Installation

1. Build the Zig library:
```bash
cd ffi/zig
zig build
```

2. Include and link:
```bash
g++ -std=c++17 -o myapp myapp.cpp \
    -I/path/to/proven/bindings/cpp/include \
    -L/path/to/proven/ffi/zig/zig-out/lib \
    -lproven
```

Or with CMake:
```cmake
add_subdirectory(bindings/cpp)
target_link_libraries(myapp proven_cpp)
```

## Usage

```cpp
#include <proven/proven.hpp>
#include <iostream>

int main() {
    proven::Runtime runtime;  // RAII init/deinit
    if (!runtime.is_ok()) return 1;

    // Safe math (delegates to Idris2)
    auto result = proven::SafeMath::div(10, 0);
    if (!result) {
        std::cout << "Division by zero handled\n";
    }

    int64_t val = proven::SafeMath::div_or(42, 10, 0);  // returns 42

    // String escaping (delegates to Idris2)
    auto html = proven::SafeString::escape_html("<script>evil</script>");
    if (html) {
        std::cout << "Safe: " << *html << "\n";
    }

    // Path traversal detection (delegates to Idris2)
    if (proven::SafePath::has_traversal("../../../etc/passwd")) {
        std::cout << "Attack detected!\n";
    }

    // Float safety (delegates to Idris2)
    auto sq = proven::SafeFloat::sqrt(-1.0);
    if (!sq) {
        std::cout << "Negative sqrt handled\n";
    }

    // Version info
    auto ver = proven::LibVersion::get();
    std::cout << "Proven v" << ver.to_string() << "\n";

    return 0;
}  // Runtime automatically cleaned up
```

## File Structure

```
include/proven/
    ffi.hpp            All extern "C" declarations (single source of truth)
    proven.hpp         Umbrella include + Runtime + ProvenString + LibVersion
    safe_math.hpp      SafeMath (checked arithmetic)
    safe_string.hpp    SafeString (escaping, UTF-8 validation)
    safe_path.hpp      SafePath (traversal detection, filename sanitization)
    safe_crypto.hpp    SafeCrypto (constant-time compare, random bytes)
    safe_email.hpp     SafeEmail (email validation)
    safe_network.hpp   SafeNetwork + IPv4Address (IP parsing/classification)
    safe_url.hpp       SafeUrl + ParsedUrl (URL parsing with RAII)
    safe_uuid.hpp      SafeUuid + Uuid (UUID generation/parsing)
    safe_json.hpp      SafeJson (JSON validation, type detection)
    safe_float.hpp     SafeFloat (safe div, sqrt, ln)
    safe_version.hpp   SafeVersion + SemVer (semantic version parsing)
    safe_color.hpp     SafeColor + RGB + HSL (color parsing/conversion)
    safe_angle.hpp     Degrees + Radians (angle conversion/normalization)
CMakeLists.txt         CMake build configuration
```

## Design Principles

1. **FFI-only**: Every operation calls libproven. Zero reimplemented logic.
2. **Header-only**: No .cpp files needed. Just add include path and link.
3. **RAII**: All FFI-allocated resources freed automatically.
4. **No exceptions**: All fallible operations return `std::optional`.
   Runtime init failure is reported via `Runtime::is_ok()`.
5. **C++17 minimum**: Uses `std::optional`, `std::string_view`.

## License

PMPL-1.0-or-later
