; SPDX-License-Identifier: Palimpsest-MPL
;; META.scm - bulletproof-core meta-level information
;; Architecture decisions, design rationale, development practices

(define meta
  `((metadata
     (version . "1.0")
     (schema-version . "1.0")
     (media-type . "application/meta+scheme"))

    (architecture-decisions
     ((adr-001
       (title . "Use Idris 2 for verified safety")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Need a language that can prove code cannot crash at compile time")
       (decision . "Use Idris 2 with dependent types and totality checking")
       (consequences
        ("All functions proven total by compiler"
         "Type-level proofs verify safety properties"
         "Requires FFI bridge for other language interop"
         "Smaller ecosystem than mainstream languages")))

      (adr-002
       (title . "Result types instead of exceptions")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Exceptions break referential transparency and can crash programs")
       (decision . "Use Maybe and Either/Result for all fallible operations")
       (consequences
        ("No hidden control flow"
         "All error cases explicit in types"
         "Composable via monad operations"
         "Slightly more verbose call sites")))

      (adr-003
       (title . "Zig as FFI bridge layer")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Need to expose Idris 2 code to Python, Rust, JS, Go")
       (decision . "Use Zig for FFI bridge - compiles to C ABI, no runtime")
       (consequences
        ("Single FFI layer serves all target languages"
         "Zig's safety features complement Idris proofs"
         "Fast compilation, small binaries"
         "Requires Zig toolchain for builds")))

      (adr-004
       (title . "believe_me for runtime-dependent proofs")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Some proofs depend on FFI behavior or runtime values")
       (decision . "Use believe_me for proofs that cannot be verified statically")
       (consequences
        ("Marks trusted boundaries explicitly"
         "Runtime behavior must match assumptions"
         "FFI implementations must satisfy contracts"
         "Document all believe_me usages")))

      (adr-005
       (title . "Submodule organization pattern")
       (status . accepted)
       (date . "2025-01-10")
       (context . "Each safety domain has types, operations, and proofs")
       (decision . "Structure as SafeX.idr + SafeX/{Parser,Operations,Proofs}.idr")
       (consequences
        ("Clear separation of concerns"
         "Proofs isolated from implementation"
         "Easy to add new safety domains"
         "Consistent navigation across modules")))))

    (development-practices
     (code-style
      (indentation . "2 spaces")
      (line-length . 100)
      (naming . "camelCase for functions, PascalCase for types")
      (documentation . "Idris doc comments with |||"))

     (security
      (constant-time . "All crypto comparisons must be constant-time")
      (no-unsafe . "Avoid believe_me except for FFI boundaries")
      (totality . "All functions must be proven total")
      (escaping . "All string escaping functions must be injection-safe"))

     (testing
      (property-tests . "Use type-level proofs instead of runtime tests where possible")
      (ffi-tests . "Test FFI boundaries with known vectors")
      (integration . "Test full pipelines from Idris through FFI to target language"))

     (versioning . "Semantic versioning with proof stability guarantees")

     (documentation
      (readme . "AsciiDoc with examples")
      (api-docs . "Generated from doc comments")
      (proofs . "Each proof documented with mathematical meaning"))

     (branching
      (main . "Stable, proven code only")
      (develop . "Integration branch for new modules")
      (feature . "feature/safe-xxx for new safety domains")))

    (design-rationale
     (why-idris2
      "Idris 2 was chosen because it offers full dependent types with a mature
       totality checker. Unlike Haskell, we can express precise invariants in
       types. Unlike Coq/Agda, it's designed for practical programming with FFI
       support. The Quantitative Type Theory foundation enables efficient
       erasure of proof terms at runtime.")

     (why-safety-modules
      "Each SafeX module addresses a specific class of runtime errors that cause
       crashes in production systems. By providing verified alternatives, we
       eliminate entire categories of bugs at compile time rather than testing.")

     (why-proofs-in-separate-files
      "Proofs are separated from implementations because: 1) They can be large
       and complex, 2) Users who trust the library don't need to understand
       proofs, 3) Different verification backends may require different proof
       styles, 4) Enables proof caching and incremental verification.")

     (why-believe-me-pattern
      "believe_me is used sparingly for: 1) FFI calls where behavior depends on
       external code, 2) Proofs requiring runtime values, 3) Performance-critical
       paths where proof terms would slow compilation. Each use is documented
       with the assumption being trusted.")

     (why-zig-ffi
      "Zig provides: 1) C ABI compatibility without C's safety issues, 2) No
       runtime or GC to conflict with target languages, 3) Cross-compilation
       support for all major platforms, 4) Safety features that complement our
       verified code. The Zig layer translates between Idris's representations
       and target language conventions."))))
