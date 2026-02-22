# Proven Vala Bindings

SPDX-License-Identifier: PMPL-1.0-or-later

Idiomatic GLib/GObject Vala wrappers over the **libproven** C ABI.
All computation is performed inside libproven (Idris 2 core with Zig FFI
bridge); these bindings contain **zero reimplemented logic**.

## Modules

| File | Purpose |
|------|---------|
| `lib_proven.vapi` | VAPI declarations for every C extern in libproven |
| `SafeMath.vala` | Checked integer arithmetic (add, sub, mul, div, mod, abs, pow, clamp) |
| `SafeString.vala` | UTF-8 validation, SQL/HTML/JS escaping |
| `SafePath.vala` | Directory traversal detection, filename sanitisation |
| `SafeEmail.vala` | RFC 5321 email address validation |
| `SafeUrl.vala` | URL parsing, percent encoding/decoding |
| `SafeNetwork.vala` | IPv4 parsing, private/loopback classification |
| `SafeCrypto.vala` | Constant-time comparison, secure random, hex encode/decode, CRC32 |
| `SafeJson.vala` | JSON validation, root-level type detection |
| `SafeDateTime.vala` | ISO 8601 parsing/formatting, leap year, days-in-month |

## Building

Requires `valac >= 0.56`, GLib 2.70+, and `libproven` installed or in
the library search path.

```sh
meson setup build
meson compile -C build
```

## Usage example

```vala
// SPDX-License-Identifier: PMPL-1.0-or-later
using Proven;

void main () {
    LibProven.init ();

    // Safe arithmetic
    int64? sum = SafeMath.add (int64.MAX, 1);
    if (sum == null) {
        print ("Overflow detected\n");
    }

    // Email validation
    bool? valid = SafeEmail.is_valid ("user@example.com");
    print ("Email valid: %s\n", valid != null && valid ? "yes" : "no");

    // URL parsing
    var url = SafeUrl.parse ("https://example.com:8080/path?q=1#frag");
    if (url != null) {
        print ("Host: %s  Port: %d\n", url.host, url.port);
    }

    LibProven.deinit ();
}
```

## Error handling

Wrapper methods return nullable types (`int64?`, `string?`, `bool?`).
A `null` return indicates that the underlying libproven function reported
an error (overflow, invalid input, parse failure, etc.).  For the raw
status code, call the C function directly via `LibProven.*`.

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
