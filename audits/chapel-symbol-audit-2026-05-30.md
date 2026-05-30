<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-->

# Chapel binding — symbol-audit snapshot (2026-05-30)

This is the static audit table produced during the Chapel binding
standup (`docs/adr/0002-chapel-binding-standup.adoc`). It maps every
`extern proc` in `bindings/chapel/src/LibProven.chpl` to its
declaration in `bindings/c/include/proven.h` and to the actual export
in `ffi/zig/src/main.zig`.

The dynamic equivalent is the `chapel-symbol-audit` CI job in
`.github/workflows/chapel-ci.yml`, which runs `nm -D libproven.so`
against `bindings/chapel/symbol-manifest.txt` on every PR.

## WIRED (present in `libproven.so`; binding wrapper links + runs)

| C symbol                       | proven.h | Zig export | ABI match | Chapel wrapper                  |
|--------------------------------|----------|------------|-----------|---------------------------------|
| `proven_path_has_traversal`    | ✓        | ✓          | ✓         | `SafePath.hasTraversal`         |
| `proven_header_has_crlf`       | ✓        | ✓          | ✓         | `SafeHeader.hasCrlf` (new)      |
| `proven_free_string`           | ✓        | ✓          | ✓         | `LibProven.provenFreeString`    |
| `proven_version`               | (not in .h as string accessor) | ✓ (returns `[*:0]const u8`) | n/a (stub) | `LibProven.libraryVersion`      |
| `proven_build_info`            | (not in .h)                    | ✓ (returns `[*:0]const u8`) | n/a (stub) | `LibProven.libraryBuildInfo`    |

Notes:

- `proven_version` and `proven_build_info` are stub helpers in
  `ffi/zig/src/main.zig` (no Idris2 backing); the C ABI in
  `proven.h` declares per-component accessors (`proven_version_major`
  / `_minor` / `_patch`) but those have no Zig export today. The
  Chapel binding exposes the stub accessors as
  `libraryVersion()` / `libraryBuildInfo()` for the smoke test's
  use; they are not the documented long-term API.

## GATED (declared per proven.h; NOT exported from libproven.so)

These extern declarations are kept in the binding as the documented
ABI contract. Calling any of them from a Chapel program produces a
linker error today. Each will move to WIRED as the corresponding Zig
export lands under proven#88.

### Lifecycle (ABI in flux — proven#88 sign-off pending)

| C symbol                  | proven.h ABI         | Zig main.zig ABI         | Resolution        |
|---------------------------|----------------------|--------------------------|-------------------|
| `proven_init`             | `int32_t (void)`     | `?*Handle ()`            | Pick one (proven#88) |
| `proven_deinit`           | `void (void)`        | (not exported; `proven_free(?*Handle)` exists) | Pick one (proven#88) |
| `proven_is_initialized`   | `bool (void)`        | `u32 (?*Handle)`         | Pick one (proven#88) |

### SafeMath (8 symbols — all GATED on proven#88)

`proven_math_add_checked`, `proven_math_sub_checked`,
`proven_math_mul_checked`, `proven_math_div`, `proven_math_mod`,
`proven_math_abs_safe`, `proven_math_clamp`, `proven_math_pow_checked`.

### SafeString (4 symbols)

`proven_string_is_valid_utf8`, `proven_string_escape_sql`,
`proven_string_escape_html`, `proven_string_escape_js`.

### SafePath (1 remaining symbol — `hasTraversal` is WIRED above)

`proven_path_sanitize_filename`.

### SafeEmail (1 symbol)

`proven_email_is_valid`.

### SafeUrl (2 symbols)

`proven_http_url_encode`, `proven_http_url_decode`.

### SafeNetwork (3 symbols)

`proven_network_parse_ipv4`, `proven_network_ipv4_is_private`,
`proven_network_ipv4_is_loopback`.

### SafeCrypto (2 symbols)

`proven_crypto_constant_time_eq`, `proven_crypto_random_bytes`.

### SafeJson (2 symbols)

`proven_json_is_valid`, `proven_json_get_type`.

### SafeDateTime (4 symbols)

`proven_datetime_parse`, `proven_datetime_format_iso8601`,
`proven_datetime_is_leap_year`, `proven_datetime_days_in_month`.

### SafeFloat (5 symbols)

`proven_float_div`, `proven_float_is_finite`, `proven_float_is_nan`,
`proven_float_sqrt`, `proven_float_ln`.

### SafeHex (1 symbol)

`proven_hex_encode`.

### SafeColor (1 symbol)

`proven_color_to_hex`.

### SafeAngle (4 symbols)

`proven_angle_deg_to_rad`, `proven_angle_rad_to_deg`,
`proven_angle_normalize_degrees`, `proven_angle_normalize_radians`.

### SafeCalculator (1 symbol)

`proven_calculator_eval`.

### Version components (5 symbols)

`proven_ffi_abi_version`, `proven_version_major`, `proven_version_minor`,
`proven_version_patch`, `proven_module_count`.

## Summary

- WIRED: **5 symbols** (out of ~50 declared in `LibProven.chpl`)
- GATED: ~45 symbols, all blocked on proven#88
- Test coverage of WIRED: 4 test modules (`TestSafePath`,
  `TestSafeHeader`, `TestLibraryInfo`, `TestFfiContract`) plus the
  smoke target.

The chapel-symbol-audit CI job enforces this snapshot: any drift in
which symbols `libproven.so` exports versus `symbol-manifest.txt`
fails the PR.
