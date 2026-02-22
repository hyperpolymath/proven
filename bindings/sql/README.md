<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven SQL Bindings

SQL User-Defined Functions (UDFs) that call libproven via C extensions for
PostgreSQL, SQLite, and MySQL. All computation is performed in formally verified
Idris 2 code through the Zig FFI bridge; these extensions are thin wrappers
that marshal SQL types to/from the libproven C ABI.

## Supported Databases

| Database   | Extension Type            | Files                                       |
|------------|---------------------------|---------------------------------------------|
| PostgreSQL | C extension (V1 calling)  | `postgresql/proven_extension.c`, `*.sql`    |
| SQLite     | Loadable extension        | `sqlite/proven_sqlite.c`, `*.sql`           |
| MySQL      | UDF plugin                | `mysql/proven_udf.c`, `*.sql`              |

## Functions

All functions return `NULL` on error (following SQL convention for safely
indicating failure without crashing the database server).

| SQL Function                   | libproven Call                    | Return Type |
|--------------------------------|----------------------------------|-------------|
| `proven_safe_add(a, b)`       | `proven_math_add_checked(a, b)`  | `BIGINT`    |
| `proven_safe_sub(a, b)`       | `proven_math_sub_checked(a, b)`  | `BIGINT`    |
| `proven_safe_mul(a, b)`       | `proven_math_mul_checked(a, b)`  | `BIGINT`    |
| `proven_safe_div(a, b)`       | `proven_math_div(a, b)`          | `BIGINT`    |
| `proven_validate_email(text)` | `proven_email_is_valid(ptr, len)`| `BOOLEAN`   |
| `proven_validate_url(text)`   | `proven_url_parse(ptr, len)`     | `BOOLEAN`   |
| `proven_validate_ipv4(text)`  | `proven_network_parse_ipv4()`    | `BOOLEAN`   |
| `proven_sanitize_string(text)`| `proven_string_escape_html()`    | `TEXT`      |
| `proven_hash_sha256(text)`    | `proven_checksum_crc32()` *      | `TEXT`      |
| `proven_validate_json(text)`  | `proven_json_is_valid(ptr, len)` | `BOOLEAN`   |
| `proven_hex_encode(text)`     | `proven_hex_encode(ptr, len)`    | `TEXT`      |
| `proven_hex_decode(text)`     | `proven_hex_decode(ptr, len)`    | `BYTEA`     |

*Note: `proven_hash_sha256` uses `proven_checksum_crc32` as a placeholder
for hashing, since libproven exposes CRC32 checksums and digest parsing
rather than raw SHA-256 computation. For production SHA-256 hashing, use
the database's built-in cryptographic functions or `pgcrypto`.

## Building

### PostgreSQL

```bash
cd postgresql
make PG_CONFIG=/usr/bin/pg_config
make install
psql -d mydb -f proven--0.5.0.sql
```

### SQLite

```bash
cd sqlite
gcc -shared -fPIC -o proven_sqlite.so proven_sqlite.c -lproven -I../../c/include
sqlite3
> .load ./proven_sqlite
> SELECT proven_safe_add(2147483647, 1);  -- Returns NULL (overflow)
```

### MySQL

```bash
cd mysql
gcc -shared -fPIC -o proven_udf.so proven_udf.c -lproven -I../../c/include \
    $(mysql_config --cflags)
mysql -u root -e "CREATE FUNCTION proven_safe_add RETURNS INTEGER SONAME 'proven_udf.so';"
```

## Architecture

```
SQL query
  |
  v
Database UDF entry point (C extension)
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
