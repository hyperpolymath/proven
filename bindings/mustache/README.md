# Proven Safety Library - Mustache Binding

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

## Overview

[Mustache](https://mustache.github.io/) templates and lambda providers for
the proven safety library. Mustache is a logic-less template language, so all
computation is performed by lambda functions that shell out to `proven-cli`.

Templates can be rendered in two modes:

1. **Static mode** -- pass pre-computed data as the context (proven-cli called
   beforehand, results fed into the template).
2. **Lambda mode** -- lambda functions in the context call proven-cli at render
   time, replacing `{{#proven_validate_email}}...{{/proven_validate_email}}`
   sections with live results.

## Prerequisites

- `proven-cli` on `PATH` (built from `ffi/zig/`), or set `PROVEN_CLI` env var
- For JS lambdas: [Deno](https://deno.land) runtime
- For Ruby lambdas: Ruby >= 2.7 with `mustache` gem

## Files

| File | Purpose |
|------|---------|
| `templates/proven-status.mustache` | Library status display |
| `templates/proven-validation-report.mustache` | Validation report |
| `templates/proven-math-result.mustache` | Math result display |
| `helpers/proven-helpers.js` | JavaScript/Deno lambda provider |
| `helpers/proven-helpers.rb` | Ruby lambda provider |

## Quick Start (JavaScript / Deno)

```javascript
import { provenHelpers, withProvenHelpers } from "./helpers/proven-helpers.js";
import Mustache from "npm:mustache";

// Lambda mode: proven-cli called at render time
const template = `
Email: {{#proven_validate_email}}user@example.com{{/proven_validate_email}}
Hash:  {{#proven_hash_sha256}}hello world{{/proven_hash_sha256}}
Add:   {{#proven_safe_add}}10 20{{/proven_safe_add}}
`;

const output = Mustache.render(template, provenHelpers());
console.log(output);
```

Run with:

```bash
deno run --allow-run --allow-env helpers/proven-helpers.js --test
```

## Quick Start (Ruby)

```ruby
require_relative 'helpers/proven-helpers'
require 'mustache'

template = <<~MUSTACHE
  Email: {{#proven_validate_email}}user@example.com{{/proven_validate_email}}
  Hash:  {{#proven_hash_sha256}}hello world{{/proven_hash_sha256}}
  Add:   {{#proven_safe_add}}10 20{{/proven_safe_add}}
MUSTACHE

output = Mustache.render(template, ProvenHelpers.context)
puts output
```

Run self-test:

```bash
ruby helpers/proven-helpers.rb --test
```

## Static Mode

For pre-computed data (no lambda calls at render time), pass a plain Hash/object:

```javascript
import Mustache from "npm:mustache";
import { readFileSync } from "node:fs";

const template = readFileSync("templates/proven-math-result.mustache", "utf-8");
const context = {
  operation: "add",
  operands: [
    { label: "a", value: 100 },
    { label: "b", value: 200 },
  ],
  result: 300,
  success: true,
  overflow_checked: true,
  timestamp: "2026-02-22T14:00:00Z",
};

console.log(Mustache.render(template, context));
```

## Available Lambdas

### Math

| Lambda | Arguments | Example |
|--------|-----------|---------|
| `proven_safe_add` | `a b` (space-separated) | `{{#proven_safe_add}}10 20{{/proven_safe_add}}` |
| `proven_safe_sub` | `a b` | `{{#proven_safe_sub}}30 10{{/proven_safe_sub}}` |
| `proven_safe_mul` | `a b` | `{{#proven_safe_mul}}6 7{{/proven_safe_mul}}` |
| `proven_safe_div` | `a b` | `{{#proven_safe_div}}100 3{{/proven_safe_div}}` |
| `proven_safe_mod` | `a b` | `{{#proven_safe_mod}}17 5{{/proven_safe_mod}}` |
| `proven_safe_abs` | `n` | `{{#proven_safe_abs}}-42{{/proven_safe_abs}}` |
| `proven_safe_pow` | `base exp` | `{{#proven_safe_pow}}2 10{{/proven_safe_pow}}` |
| `proven_safe_clamp` | `min max value` | `{{#proven_safe_clamp}}0 100 150{{/proven_safe_clamp}}` |

### Validation

| Lambda | Arguments | Example |
|--------|-----------|---------|
| `proven_validate_email` | email string | `{{#proven_validate_email}}user@example.com{{/proven_validate_email}}` |
| `proven_validate_url` | URL string | `{{#proven_validate_url}}https://example.com{{/proven_validate_url}}` |
| `proven_validate_ipv4` | IPv4 string | `{{#proven_validate_ipv4}}192.168.1.1{{/proven_validate_ipv4}}` |
| `proven_validate_path` | path string | `{{#proven_validate_path}}../etc/passwd{{/proven_validate_path}}` |
| `proven_validate_json` | JSON string | `{{#proven_validate_json}}{"key": "value"}{{/proven_validate_json}}` |
| `proven_validate_password` | password | `{{#proven_validate_password}}MyP@ssw0rd!{{/proven_validate_password}}` |

### String

| Lambda | Arguments | Example |
|--------|-----------|---------|
| `proven_sanitize_string` | raw input | `{{#proven_sanitize_string}}<script>alert(1)</script>{{/proven_sanitize_string}}` |
| `proven_escape_sql` | raw input | `{{#proven_escape_sql}}Robert'; DROP TABLE--{{/proven_escape_sql}}` |
| `proven_escape_js` | raw input | `{{#proven_escape_js}}alert('xss'){{/proven_escape_js}}` |

### Crypto

| Lambda | Arguments | Example |
|--------|-----------|---------|
| `proven_hash_sha256` | input string | `{{#proven_hash_sha256}}hello world{{/proven_hash_sha256}}` |
| `proven_random_hex` | byte count | `{{#proven_random_hex}}32{{/proven_random_hex}}` |
| `proven_checksum_crc32` | input data | `{{#proven_checksum_crc32}}data{{/proven_checksum_crc32}}` |
| `proven_hex_encode` | raw string | `{{#proven_hex_encode}}Hello{{/proven_hex_encode}}` |
| `proven_hex_decode` | hex string | `{{#proven_hex_decode}}48656c6c6f{{/proven_hex_decode}}` |

### Formatting

| Lambda | Arguments | Example |
|--------|-----------|---------|
| `proven_format_datetime` | ISO 8601 string | `{{#proven_format_datetime}}2026-01-15T10:30:00Z{{/proven_format_datetime}}` |
| `proven_is_leap_year` | year | `{{#proven_is_leap_year}}2024{{/proven_is_leap_year}}` |
| `proven_parse_color` | hex color | `{{#proven_parse_color}}#FF5733{{/proven_parse_color}}` |
| `proven_parse_version` | semver string | `{{#proven_parse_version}}1.2.3-alpha{{/proven_parse_version}}` |
| `proven_version_compare` | `a b` (space-separated) | `{{#proven_version_compare}}1.2.3 2.0.0{{/proven_version_compare}}` |
| `proven_uuid_v4` | (none) | `{{#proven_uuid_v4}}{{/proven_uuid_v4}}` |
| `proven_parse_uuid` | UUID string | `{{#proven_parse_uuid}}550e8400-e29b-41d4-a716-446655440000{{/proven_parse_uuid}}` |

## Architecture

```
Mustache template
    |
    v
Lambda function (JS or Ruby)
    |
    v  (subprocess call)
proven-cli          (Zig binary)
    |
    v
libproven.so        (Zig FFI bridge)
    |
    v
Proven (Idris 2)    (formally verified core)
```

## Merging with Existing Context

Both the JS and Ruby helpers support merging into an existing context so
you can combine proven lambdas with your application data:

```javascript
// JavaScript
import { withProvenHelpers } from "./helpers/proven-helpers.js";
const context = withProvenHelpers({ title: "My Report", items: [...] });
```

```ruby
# Ruby
context = ProvenHelpers.merge_into({ 'title' => 'My Report', 'items' => [...] })
```
