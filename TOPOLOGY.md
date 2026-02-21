<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# proven — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              CONSUMER APP               │
                        │        (89 Language Targets)            │
                        └───────────────────┬─────────────────────┘
                                            │ FFI Call
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           LANGUAGE BINDINGS             │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ Python    │  │ Rust      │  │ JS /  ││
                        │  │ (ctypes)  │  │ (bindgen) │  │ WASM  ││
                        │  └─────┬─────┘  └─────┬─────┘  └───────┘│
                        └────────│──────────────│─────────────────┘
                                 │              │
                                 ▼              ▼
                        ┌─────────────────────────────────────────┐
                        │           ZIG FFI BRIDGE LAYER          │
                        │    (Pure ABI, Memory Safe, Stable)      │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           IDRIS2 VERIFIED CORE          │
                        │                                         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │ 104 Proof │  │  Totality         │  │
                        │  │ Modules   │  │  Checking         │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           VERIFICATION LAYER            │
                        │      (ECHIDNA, Coq, Lean, Z3)           │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Pack / ipkg        .machine_readable/  │
                        │  Guix / Nix         0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE MODULES (IDRIS2)
  Core Safety (Math/String)         ██████████ 100%    Unbreakable proofs stable
  Data Formats (Json/XML)           ██████████ 100%    Parsing verified
  Security & Auth                   ██████████ 100%    Argon2id/AES-GCM verified
  Distributed Primitives            ██████████ 100%    Consensus/ACID stable

BINDINGS & BRIDGE
  Zig FFI Bridge                    ██████████ 100%    Stable C ABI bridge
  89 Language Targets               ██████████ 100%    All bindings generated
  Binding Coverage                  ██████████ 100%    100% proofs passing

REPO INFRASTRUCTURE
  Pack Automation                   ██████████ 100%    Standard build/test tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  ECHIDNA Integration               ██████████ 100%    CI proof verification active

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ██████████ 100%    v1.1.0 Stable Release
```

## Key Dependencies

```
Idris2 Core ──────► Zig FFI Bridge ──────► Lang Binding ──────► Application
     │                 │                      │                    │
     ▼                 ▼                      ▼                    ▼
Formal Proof ─────► C ABI Header ────────► Logic Safety ──────► No Crashes
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).
