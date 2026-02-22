<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-22 -->

# proven — System Architecture & Completion Dashboard

## System Architecture

```
                        ┌─────────────────────────────────────────────┐
                        │          APPLICATION LAYER (7%)              │
                        │                                              │
                        │  proven-bgp ✅   proven-httpd ░░            │
                        │  proven-dns ░░   proven-smtp ░░             │
                        │  proven-lpd ░░   proven-mqtt ░░             │
                        │  proven-ntp ░░   proven-syslog ░░           │
                        │  proven-tftp ░░  proven-ssh-bastion ░░      │
                        │  proven-ws ░░    proven-cli ░░              │
                        │  proven-wasm ░░                              │
                        └──────────────────────┬──────────────────────┘
                                               │
                        ┌──────────────────────▼──────────────────────┐
                        │       FRAMEWORK CONVENIENCE LAYER (0%)       │
                        │                                              │
                        │  Proven.Web ░░      Proven.Crypto ░░        │
                        │  Proven.Network ░░  Proven.Data ░░          │
                        │  Proven.System ░░   Proven.Math ░░          │
                        │  Proven.Hardware ░░                          │
                        └──────────────────────┬──────────────────────┘
                                               │
 ┌─────────────────────────────────────────────▼─────────────────────────────────────────────┐
 │                            IDRIS2 VERIFIED CORE (100%)                                     │
 │                                                                                            │
 │  104 Safe* Modules — Dependent Type Proofs — Totality Checking — believe_me = 0            │
 │                                                                                            │
 │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────────┐     │
 │  │  Core Safety  │ │ Data Formats │ │ Web/Network  │ │  Security    │ │   System     │     │
 │  │ SafeMath      │ │ SafeJson     │ │ SafeUrl      │ │ SafeCrypto   │ │ SafeShell    │     │
 │  │ SafeString    │ │ SafeXML      │ │ SafeHtml     │ │ SafePassword │ │ SafeProcess  │     │
 │  │ SafePath      │ │ SafeYAML     │ │ SafeNetwork  │ │ SafeJWT      │ │ SafePipe     │     │
 │  │ SafeEmail     │ │ SafeTOML     │ │ SafeHeader   │ │ SafeCert     │ │ SafeSignal   │     │
 │  │ SafeRegex     │ │ SafeCSV      │ │ SafeCookie   │ │ SafeOAuth    │ │ SafeTerminal │     │
 │  │ SafeSQL       │ │ SafeBase64   │ │ SafeDNS      │ │ SafeDigest   │ │ SafeDocker   │     │
 │  │ SafeCommand   │ │ SafeHex      │ │ SafeSSH      │ │ SafeChecksum │ │ SafeGit      │     │
 │  └──────────────┘ └──────────────┘ └──────────────┘ └──────────────┘ └──────────────┘     │
 │                                                                                            │
 │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌──────────────┐                      │
 │  │  Mathematics  │ │Data Structs  │ │  Distributed │ │   Domain     │                      │
 │  │ SafeFloat     │ │ SafeTree     │ │ SafeConsensus│ │ SafeColor    │                      │
 │  │ SafeDecimal   │ │ SafeGraph    │ │ SafeOrdering │ │ SafeGeo      │                      │
 │  │ SafeRational  │ │ SafeHeap     │ │ SafeResource │ │ SafeVersion  │                      │
 │  │ SafeComplex   │ │ SafeSet      │ │ SafeTransact │ │ SafeUUID     │                      │
 │  │ SafeFinField  │ │ SafeQueue    │ │ SafeCapabil  │ │ SafeCurrency │                      │
 │  │ SafeProbabil  │ │ SafeBloom    │ │ SafeRateLimit│ │ SafePhone    │                      │
 │  │ SafeUnit      │ │ SafeLRU      │ │ SafeCircBrk  │ │ SafeDateTime │                      │
 │  │ SafeAngle     │ │ SafeMatrix   │ │ SafeRetry    │ │ SafeI18n     │                      │
 │  │ SafeTensor    │ │ SafeBitset   │ │ SafeMonotonic│ │ SafeCalc     │                      │
 │  └──────────────┘ └──────────────┘ └──────────────┘ └──────────────┘                      │
 │                                                                                            │
 │  ECHIDNA: ProofTerm + Validator + Soundness (neurosymbolic verification)                   │
 └───────────────────────────────────────┬────────────────────────────────────────────────────┘
                                         │
                        ┌────────────────▼────────────────┐
                        │       ZIG FFI BRIDGE (100%)      │
                        │    (Pure C ABI — No Logic)       │
                        │                                  │
                        │  65 FFI wrapper modules          │
                        │  build.zig + build.zig.zon       │
                        │  Idris2 RefC → C → Zig → .so    │
                        └────────────────┬────────────────┘
                                         │
          ┌──────────────────────────────▼──────────────────────────────┐
          │                  LANGUAGE BINDINGS (15%)                     │
          │                  (Thin Wrappers — FFI Calls Only)           │
          │                                                             │
          │  ✅ Complete (18): Python, Rust, JavaScript, Deno,          │
          │     ReScript, Gleam, Julia, Swift, Kotlin, Go, Elixir,      │
          │     Zig, Lua, Ruby, Nim, OCaml, Haskell, Ada               │
          │                                                             │
          │  ░░ Scaffolded (102): C, C++, Erlang, Dart, Java,          │
          │     Scala, PHP, Perl, R, Clojure, F#, etc.                 │
          └─────────────────────────────────────────────────────────────┘

          ┌─────────────────────────────────────────────────────────────┐
          │                  HARDWARE ACCELERATION (0%)                 │
          │                                                             │
          │  SafeGPU ░░    SafeVPU ░░     SafeTPU ░░                   │
          │  SafeCryptoHW ░░  SafeVulkanCompute ░░                     │
          │  SafeOpenCL ░░    SafeMetal ░░   SafeCUDA ░░               │
          └─────────────────────────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE MODULES (IDRIS2)
  Core Safety (Math/String/Path)   ██████████ 100%    believe_me=0, all proofs total
  Data Formats (Json/XML/CSV)      ██████████ 100%    Parsing verified end-to-end
  Web & Network                    ██████████ 100%    URL/HTML/HTTP/DNS/SSH
  Security & Auth                  ██████████ 100%    Crypto/JWT/OAuth/Cert
  Mathematics & Physics            ██████████ 100%    Float/Decimal/Complex/Tensor
  Data Structures                  ██████████ 100%    Tree/Graph/Bloom/LRU/Matrix
  Distributed Systems              ██████████ 100%    Consensus/CircuitBreaker/Retry
  System & I/O                     ██████████ 100%    Shell/Process/Pipe/Signal
  Domain Values                    ██████████ 100%    UUID/Currency/Phone/DateTime

VERIFICATION
  ECHIDNA Integration              ██████████ 100%    ProofTerm + Validator + Soundness
  Totality Checking                ██████████ 100%    --total flag enforced
  believe_me Count                 ██████████ 100%    0 instances (was 4,566)

BRIDGE & BINDINGS
  Zig FFI Bridge                   ██████████ 100%    65 exports, stable C ABI
  Bindings Complete                ██░░░░░░░░  15%    18/120 fully implemented
  Bindings Scaffolded              █████████░  85%    102/120 have FFI stubs

APPLICATIONS
  Server Apps                      █░░░░░░░░░   7%    1/13 (only proven-bgp)
  CLI Tool                         ░░░░░░░░░░   0%    Not started
  WASM Build                       ░░░░░░░░░░   0%    Not started

HARDWARE ACCELERATION
  GPU/VPU/TPU Backends             ░░░░░░░░░░   0%    8 modules planned
  Crypto HW (AES-NI/SHA)           ░░░░░░░░░░   0%    Not started

FRAMEWORK CONVENIENCE
  Re-export Modules                ░░░░░░░░░░   0%    7 modules planned (Web/Crypto/etc)

CONTAINER & DEPLOYMENT
  Containerfile                    ██████████ 100%    OCI-compliant, built
  Stapeln Hardening                ░░░░░░░░░░   0%    Not configured
  Firewalld Rules                  ░░░░░░░░░░   0%    Not configured
  Svalinn Compose                  ░░░░░░░░░░   0%    Not configured

TESTING
  Unit Tests                       ██░░░░░░░░  17%    18/104 modules covered
  Property Tests                   ███░░░░░░░  32%    33/104 modules covered
  Fuzz Harnesses                   ░░░░░░░░░░   0%    Workflows exist, no harnesses
  E2E Cross-Language               ░░░░░░░░░░   0%    Not started
  Benchmarks                       █░░░░░░░░░   6%    6/104 modules benchmarked

RSR COMPLIANCE
  SPDX Headers                     ██████████ 100%    PMPL-1.0-or-later everywhere
  .editorconfig                    ██████████ 100%    Fixed 2026-02-22
  0-AI-MANIFEST.a2ml               ██████████ 100%    Created 2026-02-22
  CODEOWNERS                       ██████████ 100%    Created 2026-02-22
  .well-known/security.txt         ██████████ 100%    Created 2026-02-22
  CI/CD Workflows                  ██████████ 100%    27 workflows
  .machine_readable/               ██████████ 100%    STATE/META/ECOSYSTEM up to date

─────────────────────────────────────────────────────────────────────────────
OVERALL:                           ██████░░░░  55%    Core solid, ecosystem incomplete
```

## Key Dependencies

| Dependency     | Version  | Purpose                           | Status       |
|----------------|----------|-----------------------------------|--------------|
| Idris2         | >= 0.6.0 | Core language + totality checker  | Required     |
| Zig            | 0.13+    | FFI bridge compilation            | Required     |
| pack           | latest   | Idris2 package manager            | NOT INSTALLED|
| Chainguard     | latest   | Container base images             | Planned      |
| stapeln        | latest   | Container layer management        | Planned      |
| hypatia        | v2       | Security scanning                 | Integrated   |
| echidnabot     | latest   | Proof verification bot            | Integrated   |

## Build Pipeline

```
idris2 --codegen refc proven-ffi.ipkg
          │
          ▼
    build/refc/*.c  (generated C from Idris2)
          │
          ▼
    zig build -Didris-refc=build/refc ...
          │
          ▼
    ffi/zig/zig-out/lib/libproven.{so,dylib,dll}
          │
          ▼
    Language bindings link against libproven
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
