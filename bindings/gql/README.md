<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven GQL Bindings

GQL (Graph Query Language, ISO/IEC 39075) functions backed by libproven for
graph databases. All computation is performed in formally verified Idris 2
code through the Zig FFI bridge; these bindings are thin wrappers that marshal
graph database types to/from the libproven C ABI.

## Supported Platforms

| Platform | Implementation               | Files                                  |
|----------|------------------------------|----------------------------------------|
| GQL      | Standard function defs       | `proven_gql.gql`                       |
| Neo4j    | Java stored procedures (JNA) | `neo4j/proven_procedures.java`         |
| Neo4j    | Cypher equivalents           | `neo4j/proven_functions.cypher`        |

## Functions

All functions call through to libproven. They return `null` on error.

| GQL Function                   | libproven Call                    | Return Type |
|--------------------------------|----------------------------------|-------------|
| `proven.safeAdd(a, b)`        | `proven_math_add_checked(a, b)`  | `INT64`     |
| `proven.safeSub(a, b)`        | `proven_math_sub_checked(a, b)`  | `INT64`     |
| `proven.safeMul(a, b)`        | `proven_math_mul_checked(a, b)`  | `INT64`     |
| `proven.safeDiv(a, b)`        | `proven_math_div(a, b)`          | `INT64`     |
| `proven.validateEmail(s)`     | `proven_email_is_valid(ptr, len)`| `BOOLEAN`   |
| `proven.validateUrl(s)`       | `proven_url_parse(ptr, len)`     | `BOOLEAN`   |
| `proven.validateIpv4(s)`      | `proven_network_parse_ipv4()`    | `BOOLEAN`   |
| `proven.sanitizeString(s)`    | `proven_string_escape_html()`    | `STRING`    |
| `proven.hashSha256(s)`        | `proven_checksum_crc32()`        | `STRING`    |
| `proven.validateJson(s)`      | `proven_json_is_valid(ptr, len)` | `BOOLEAN`   |
| `proven.hexEncode(s)`         | `proven_hex_encode(ptr, len)`    | `STRING`    |
| `proven.hexDecode(s)`         | `proven_hex_decode(ptr, len)`    | `STRING`    |

## Neo4j Installation

```bash
# Build the JAR
cd neo4j
mvn package

# Copy to Neo4j plugins directory
cp target/proven-neo4j-0.5.0.jar $NEO4J_HOME/plugins/
# Also copy libproven.so to a path accessible by the JVM
cp libproven.so /usr/local/lib/

# Add to neo4j.conf:
# dbms.security.procedures.unrestricted=proven.*

# Restart Neo4j
neo4j restart
```

## Architecture

```
GQL query / Cypher query
  |
  v
Neo4j stored procedure (Java + JNA)
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
