# Proven Objective-C Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Objective-C FFI bindings for the Proven formally verified safety library.

## Architecture

All computation is performed by the formally verified Idris 2 core via the
Zig FFI bridge (libproven). These Objective-C wrappers are thin delegation
layers that **only** call libproven C functions. No logic is reimplemented.

```
Objective-C (PVN*) -> PVNLibProven (dlopen) -> libproven.so -> Zig FFI -> Idris 2
```

## Modules

| File | Description |
|------|-------------|
| `PVNLibProven` | C type declarations and dynamic library loading via dlopen/dlsym |
| `PVNSafeMath` | Checked arithmetic (add, sub, mul, div, mod, abs, pow) |
| `PVNSafeString` | UTF-8 validation and string escaping (SQL, HTML, JS) |
| `PVNSafePath` | Directory traversal detection and filename sanitization |
| `PVNSafeEmail` | RFC 5321 email address validation |
| `PVNSafeUrl` | URL parsing into components |
| `PVNSafeNetwork` | IPv4 parsing, private/loopback classification |
| `PVNSafeCrypto` | Constant-time comparison and secure random bytes |
| `PVNSafeJson` | JSON validation and root-level type detection |

## Usage

```objc
#import "PVNLibProven.h"
#import "PVNSafeMath.h"
#import "PVNSafeEmail.h"

// Load the library
NSError *error = nil;
PVNLibProven *lib = [PVNLibProven loadFromPath:@"/usr/local/lib/libproven.so"
                                         error:&error];
[lib provenInit];

// Safe math
PVNSafeMath *math = [[PVNSafeMath alloc] initWithLibrary:lib];
NSNumber *sum = [math addChecked:INT64_MAX b:1 error:&error];
// sum == nil, error.code == PVNProvenStatusErrOverflow

// Email validation
PVNSafeEmail *email = [[PVNSafeEmail alloc] initWithLibrary:lib];
NSNumber *valid = [email isValid:@"user@example.com" error:&error];
// valid.boolValue == YES

[lib provenDeinit];
```

## Conventions

- **PVN prefix**: All types and classes use the `PVN` (Proven) prefix
- **ARC**: All code uses Automatic Reference Counting
- **NSNumber nullable**: Numeric results return `NSNumber *` (nullable) -- nil on error
- **NSString nullable**: String results return `NSString *` (nullable) -- nil on error
- **NSError out-param**: Standard Cocoa error pattern with `(NSError **)error`
- **@autoreleasepool**: Used in methods that create temporary Objective-C objects

## Building

Requires `libproven.so` (or `libproven.dylib` on macOS) to be built and
available at a known path. See the main Proven repository for build instructions.

```bash
# Example: compile with clang (GNUstep on Linux)
clang -fobjc-arc -fmodules \
    $(gnustep-config --objc-flags) \
    -I./src \
    src/PVNLibProven.m src/PVNSafeMath.m src/PVNSafeString.m \
    src/PVNSafePath.m src/PVNSafeEmail.m src/PVNSafeUrl.m \
    src/PVNSafeNetwork.m src/PVNSafeCrypto.m src/PVNSafeJson.m \
    $(gnustep-config --objc-libs) -ldl \
    -o proven_objc_example
```

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
