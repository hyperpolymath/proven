<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# Proven - Oblibeny Bindings

## Overview

Oblibeny bindings for **libproven**, the Idris 2 formally verified safety library.

Oblibeny is a Czech-inspired language focusing on "favorites" (oblibeny = favorite in Czech).
It uses `cizi("C", ...)` for FFI declarations and `Vysledek<T, Ch>` (Result) with
`Uspech`/`Neuspech` (Success/Failure) as its core result type, with Czech naming
throughout.

All computation is performed in Idris 2 via the Zig FFI layer. **No safety logic is
reimplemented in Oblibeny.** These bindings are thin FFI wrappers only.

## Architecture

```
Oblibeny (.obl)
  |  cizi("C", ...)
  v
libproven.so (Zig FFI vrstva)
  |  Zig -> Idris2 RefC volani
  v
Idris 2 (zavisle typy, kontrola totality)
```

## Modules

| Module | Description |
|--------|-------------|
| `Proven` | Core types, error types, lifecycle, re-exports |
| `Proven.FFI` | Raw FFI declarations to libproven C ABI |
| `Proven.SafeMath` | Overflow-checked integer arithmetic |
| `Proven.SafeString` | String escaping, UTF-8 validation, path safety |
| `Proven.SafeEmail` | RFC 5321 email validation |
| `Proven.SafeUrl` | URL parsing and validation |
| `Proven.SafeCrypto` | Constant-time comparison, random bytes, hex, CRC32 |
| `Proven.SafeJson` | JSON validation, type detection, expression eval |

## Error Types

Oblibeny's `Vysledek<T, Ch>` type provides exhaustive matching with Czech naming:

```oblibeny
typ ProvenChyba =
  | Preteceni | Podteceni | DeleniNulou
  | NulovyUkazatel | NeplatnyArgument
  | ChybaParsovani | ValidaceNeuspela
  | MimoRozsah | ChybaKodovani
  | AlokaceNeuspela | Neimplementovano
  | NeznamaChyba(Int32)
```

All operations return `Vysledek<T, ProvenChyba>`:

```oblibeny
match bezpecne_scitani(a, b) with
| Uspech(vysledek)      => zpracuj_uspech(vysledek)
| Neuspech(Preteceni)   => zpracuj_preteceni()
| Neuspech(e)           => zpracuj_jinou_chybu(e)
```

## Usage

```oblibeny
import Proven

fn hlavní() -> Vysledek<Unit, ProvenChyba> =
  inicializuj()?
  let soucet = bezpecne_scitani(100i64, 200i64)?
  let email_ok = je_platny_email("user@example.com")?
  deinicializuj()
  Uspech(())
```

## Requirements

- libproven shared library (libproven.so / libproven.dylib / proven.dll)
- Oblibeny compiler with C FFI support

## License

PMPL-1.0-or-later
