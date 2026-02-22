<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven KQL Bindings

KQL (Kusto Query Language) functions backed by libproven for Azure Data Explorer
and Log Analytics. All computation is performed in formally verified Idris 2 code
through the Zig FFI bridge; these bindings use KQL inline Python plugins and
a C# plugin for Azure Data Explorer to call the libproven C ABI.

## Files

| File                     | Purpose                                       |
|--------------------------|-----------------------------------------------|
| `proven_functions.kql`   | KQL function definitions (inline Python)      |
| `proven_plugin.cs`       | C# plugin for ADX calling libproven via P/Invoke |
| `proven_queries.kql`     | Example proven-backed queries                 |

## Functions

All functions call through to libproven and return `null` on error.

| KQL Function                   | libproven Call                    | Return Type |
|--------------------------------|----------------------------------|-------------|
| `proven_safe_add(a, b)`       | `proven_math_add_checked(a, b)`  | `long`      |
| `proven_safe_sub(a, b)`       | `proven_math_sub_checked(a, b)`  | `long`      |
| `proven_safe_mul(a, b)`       | `proven_math_mul_checked(a, b)`  | `long`      |
| `proven_safe_div(a, b)`       | `proven_math_div(a, b)`          | `long`      |
| `proven_validate_email(s)`    | `proven_email_is_valid(ptr, len)`| `bool`      |
| `proven_validate_url(s)`      | `proven_url_parse(ptr, len)`     | `bool`      |
| `proven_validate_ipv4(s)`     | `proven_network_parse_ipv4()`    | `bool`      |
| `proven_sanitize_string(s)`   | `proven_string_escape_html()`    | `string`    |
| `proven_hash_sha256(s)`       | `proven_checksum_crc32()`        | `string`    |
| `proven_validate_json(s)`     | `proven_json_is_valid(ptr, len)` | `bool`      |
| `proven_hex_encode(s)`        | `proven_hex_encode(ptr, len)`    | `string`    |
| `proven_hex_decode(s)`        | `proven_hex_decode(ptr, len)`    | `string`    |

## Deployment

### Azure Data Explorer (C# Plugin)

1. Enable the C# sandbox plugin on your ADX cluster
2. Deploy `libproven.so` to the cluster's sandbox image
3. Register the plugin functions via `.create-or-alter function`

### Log Analytics (Inline Python)

1. Enable the Python plugin on your Log Analytics workspace
2. Ensure `libproven.so` is available in the Python sandbox
3. Use the function definitions from `proven_functions.kql`

## Architecture

```
KQL query
  |
  v
Inline Python plugin / C# plugin
  |
  v
ctypes / P/Invoke
  |
  v
libproven C ABI (proven.h)
  |
  v
Zig FFI bridge
  |
  v
Verified Idris 2 core
```

## License

SPDX-License-Identifier: PMPL-1.0-or-later
