;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm — Ecosystem position for proven
;; Last updated: 2026-02-21

(ecosystem
  (metadata
    (version "1.1.0")
    (last-updated "2026-02-21"))

  (project
    (name "proven")
    (purpose "Formally verified safety library — crash-proof operations via dependent types")
    (role verified-safety-library))

  (position-in-ecosystem
    (category "core-infrastructure")
    (consumers "Any application needing crash-proof operations across 89 languages"))

  (related-projects
    (project "language-bridges" (relationship sibling-standard) (description "FFI bridges that proven's Zig layer builds upon"))
    (project "hypatia" (relationship tooling) (description "Neurosymbolic security scanner used for pre-submission validation"))
    (project "echidna" (relationship tooling) (description "Proof verification system integrated into CI"))
    (project "valence-shell" (relationship potential-consumer) (description "Shell that uses proven's SafeShell/SafePipe/SafeProcess modules"))))
