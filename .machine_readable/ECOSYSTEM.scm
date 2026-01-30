; SPDX-License-Identifier: MPL-2.0-or-later
; SPDX-FileCopyrightText: 2025 Hyperpolymath
;; ECOSYSTEM.scm - proven ecosystem relationships
;; How this project relates to and integrates with other projects

(ecosystem
 (version . "1.0")
 (name . "proven")
 (type . "verified-library")
 (purpose . "Provide mathematically proven safe operations for common dangerous programming patterns")

 (position-in-ecosystem
  (role . "foundation-library")
  (layer . "core-safety")
  (consumers . ("applications" "frameworks" "other-libraries"))
  (description
   "proven sits at the foundation layer, providing verified safe
    primitives that higher-level code can build upon. It integrates with
    ECHIDNA for theorem proving and echidnabot for CI verification."))

 (related-projects
  ;; Primary integrations
  ((name . "echidna")
   (relationship . "prover-backend")
   (repo . "github.com/hyperpolymath/echidna")
   (integration-type . "verification-target")
   (description
    "ECHIDNA is a neurosymbolic theorem proving platform. proven's
     Idris 2 proofs can be verified by ECHIDNA's multi-prover system.
     Future: Add Idris 2 as a Tier 2 prover backend.")
   (integration-points
    ("Idris 2 proofs -> ECHIDNA verification"
     "Aspect tagging: SafetyVerified, MathematicallySafe, etc."
     "Theorem library for proof reuse"
     "Neural tactic suggestion for proof synthesis")))

  ((name . "echidnabot")
   (relationship . "ci-integration")
   (repo . "github.com/hyperpolymath/echidnabot")
   (integration-type . "automation")
   (description
    "echidnabot orchestrates ECHIDNA for CI/CD. PRs modifying proven
     trigger automatic proof verification via echidnabot webhooks.")
   (integration-points
    ("Webhook triggers on .idr file changes"
     "Proof verification as GitHub Check Run"
     "Tactic suggestions in PR comments"
     "Merge blocking on proof failures")))

  ;; idris2-* ecosystem
  ((name . "idris2-pack")
   (relationship . "package-manager")
   (repo . "github.com/stefan-hoeck/idris2-pack")
   (integration-type . "distribution")
   (description . "Package manager for Idris 2 - proven will be distributed via pack"))

  ((name . "idris2-lsp")
   (relationship . "development-tool")
   (repo . "github.com/idris-community/idris2-lsp")
   (integration-type . "tooling")
   (description . "Language server providing IDE support for proven development"))

  ;; hyperpolymath ecosystem
  ((name . "idris2-quickcheck")
   (relationship . "sibling-library")
   (repo . "github.com/hyperpolymath/idris2-quickcheck")
   (integration-type . "testing")
   (description . "Property-based testing - complements proven proofs with runtime checks"))

  ((name . "idris2-elab-util")
   (relationship . "sibling-library")
   (repo . "github.com/hyperpolymath/idris2-elab-util")
   (integration-type . "metaprogramming")
   (description . "Elaboration utilities - used for proof automation in proven"))

  ((name . "idris2-json")
   (relationship . "sibling-library")
   (repo . "github.com/hyperpolymath/idris2-json")
   (integration-type . "data-format")
   (description . "JSON library - SafeJson builds upon this with additional safety guarantees"))

  ((name . "idris2-dom")
   (relationship . "sibling-library")
   (repo . "github.com/hyperpolymath/idris2-dom")
   (integration-type . "web")
   (description . "DOM bindings - SafeHTML integrates with this for web safety"))

  ;; Specification projects (proven as reference implementation)
  ((name . "aggregate-library")
   (relationship . "specification-implementor")
   (repo . "github.com/hyperpolymath/aggregate-library")
   (integration-type . "reference-implementation")
   (description
    "aggregate-library (aLib) defines universal operations across 7 languages.
     proven provides formally verified Idris 2 implementations of aLib operations.")
   (integration-points
    ("SafeMath implements aLib arithmetic: add, subtract, multiply, divide, modulo"
     "SafeString implements aLib string: concat, length, substring"
     "All implementations have termination proofs (totality)"
     "proven serves as gold standard for aLib correctness verification"
     "89-language FFI bindings allow aLib compliance testing in any language")))

  ((name . "a2ml")
   (relationship . "documentation-tool")
   (repo . "github.com/hyperpolymath/a2ml")
   (integration-type . "markup-language")
   (description
    "A2ML (Attested Markup Language) is a typed, verifiable markup format.
     proven uses a2ml for specification documents with structural guarantees.")
   (integration-points
    ("Progressive strictness for documentation (lax -> checked -> attested)"
     "Required sections, resolved references, unique IDs"
     "Shared Idris2 foundation (a2ml has Idris2 typed core)"
     "Future: proven specs migrate to a2ml format (v1.1+)"
     "Renderer portability across HTML/Markdown/PDF pipelines")))

  ;; Potential consumers
  ((name . "ubicity")
   (relationship . "potential-consumer")
   (repo . "github.com/hyperpolymath/ubicity")
   (integration-type . "application")
   (description . "Universal interface toolkit - could use SafeJson, SafeUrl for data handling"))

  ((name . "bunsenite")
   (relationship . "potential-consumer")
   (repo . "github.com/hyperpolymath/bunsenite")
   (integration-type . "configuration")
   (description . "Configuration language - could use SafePath, SafeConfig"))

  ((name . "januskey")
   (relationship . "potential-consumer")
   (repo . "github.com/hyperpolymath/januskey")
   (integration-type . "security")
   (description . "Key management - could use SafeCrypto, SafePassword"))

  ;; External inspirations
  ((name . "safe-string")
   (relationship . "inspiration")
   (repo . "hackage.haskell.org/package/safe")
   (description . "Haskell safe library - inspired the safe operation approach"))

  ((name . "refined")
   (relationship . "inspiration")
   (repo . "hackage.haskell.org/package/refined")
   (description . "Haskell refinement types - inspired bounded/constrained types")))

 (what-this-is
  ("A mathematically verified safety library for common dangerous operations"
   "Idris 2 code with dependent types proving operations cannot crash"
   "Foundation for building reliable systems"
   "Cross-language via Zig FFI bridge"
   "Integration point for formal verification CI"))

 (what-this-is-not
  ("Not a general-purpose standard library"
   "Not a replacement for Idris prelude"
   "Not a web framework or application"
   "Not a theorem prover itself (uses ECHIDNA for that)"))

 (integration-architecture
  "
  ┌─────────────────────────────────────────────────────────────────┐
  │                     Application Layer                           │
  │  (ubicity, bunsenite, januskey, your-app)                      │
  └─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
  ┌─────────────────────────────────────────────────────────────────┐
  │                   Language Bindings (89 targets)                │
  │  High-level: Rust, Python, Go, JavaScript, TypeScript, Deno    │
  │  System: C, C++, Zig, Nim, D, Crystal, Fortran, COBOL, Ada     │
  │  JVM/CLR: Kotlin, Scala, F#, Clojure                           │
  │  Functional: Haskell, OCaml, Elixir, Gleam, Erlang, Prolog     │
  │  Dynamic: Ruby, PHP, Perl, Lua, Julia, R, Tcl, Bash            │
  │  Other: Swift, Dart, ReScript, Racket, Common Lisp, V, Odin    │
  └─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
  ┌─────────────────────────────────────────────────────────────────┐
  │                     Zig FFI Bridge                              │
  │  Pure Zig ABI, memory management, type conversion              │
  └─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
  ┌─────────────────────────────────────────────────────────────────┐
  │                       proven (Idris 2)                          │
  │  SafeMath │ SafeString │ SafeJson │ SafeUrl │ SafeEmail │ ...  │
  │  SafeUUID │ SafeCurrency │ SafePhone │ SafeHex │ SafeHeader    │
  │  SafeCookie │ SafeContentType │ 87 verified modules            │
  └─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
  ┌─────────────────────────────────────────────────────────────────┐
  │                    Verification Layer                           │
  │  ECHIDNA (multi-prover) ◄─── echidnabot (CI)                   │
  │  Coq │ Lean │ Agda │ Z3 │ Idris2                               │
  └─────────────────────────────────────────────────────────────────┘
  "))
