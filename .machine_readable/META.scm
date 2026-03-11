;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm — Meta-level information for proven
;; Last updated: 2026-02-21

(meta
  (metadata
    (version "1.1.0")
    (last-updated "2026-02-21"))

  (project-info
    (type library)
    (name "proven")
    (languages (idris2 zig))
    (license "PMPL-1.0-or-later")
    (author "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"))

  (architecture-decisions
    (adr "ADR-001" (status accepted) (title "Idris2 as sole computation layer")
      (decision "All safety logic lives in Idris2 with dependent types and totality checking. No other language reimplements logic."))
    (adr "ADR-002" (status accepted) (title "Zig as FFI bridge")
      (decision "Zig provides the C ABI bridge layer. Pure data marshaling, no safety logic."))
    (adr "ADR-008" (status accepted) (title "FFI-only bindings")
      (decision "All 89 language bindings must call Idris via Zig FFI. Reimplementation is forbidden."))
    (adr "ADR-009" (status accepted) (title "ECHIDNA proof verification in CI")
      (decision "Every PR runs ECHIDNA neurosymbolic proof verification before merge."))
    (adr "ADR-010" (status accepted) (title "Unbreakable guarantee scope")
      (decision "Formal safety guarantees apply only to Idris2 code. The Zig FFI layer is a pure ABI bridge with no safety logic. Language bindings must only marshal data and call Idris via FFI. Any non-Idris logic in bindings is non-proven and must be removed.")))

  (development-practices
    (practice "totality-required" "All Idris2 functions must pass --total checking")
    (practice "no-believe-me" "believe_me only permitted for FFI stubs with documented justification")
    (practice "hypatia-scan" "Pre-submission hypatia scan required for all external publications")))
