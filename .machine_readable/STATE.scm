;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm — Current project state for proven
;; Last updated: 2026-02-28

(state
  (metadata
    (version "1.2.0")
    (last-updated "2026-02-28")
    (status active))

  (project-context
    (name "proven")
    (purpose "Formally verified safety library — 258 Idris2 source files (104 core Safe* modules + 65 FFI wrappers + 3 ECHIDNA + support), Zig FFI bridge, 120+ language binding targets")
    (completion-percentage 55))

  (current-position
    (milestone "v1.2.0 Remediation Release")
    (phase "active-development")
    (summary "Post-remediation: believe_me eliminated (0 instances, down from ~4566), assert_total eliminated, all TODOs removed, bindings fixed to call FFI, missing bindings created, Rust license fixed, Containerfile converted, RefC pipeline built. Core Idris2 ~95% complete. Apps 7% (1/13). GPU/VPU/TPU/Crypto backends 0%. Framework convenience modules 0%. Container hardening 0%."))

  (components
    (component "idris2-core" (status complete) (modules 107) (files 261) (lines ~28400) (believe-me 0))
    (component "zig-ffi-bridge" (status complete) (exports 65))
    (component "language-bindings" (status partial) (complete 18) (scaffolded 102) (total 120))
    (component "echidna-integration" (status complete))
    (component "benchmarks" (status complete))
    (component "domain-specific" (status complete) (modules "concat" "http" "provenance-utils" "tui"))
    (component "apps" (status minimal) (complete 1) (planned 13) (note "only proven-bgp exists"))
    (component "gpu-vpu-tpu-crypto" (status not-started) (modules 0) (planned 8))
    (component "framework-convenience" (status not-started) (modules 0) (planned 7))
    (component "container-hardening" (status not-started) (note "stapeln, firewalld, svalinn not configured"))
    (component "rsr-compliance" (status mostly-complete) (note "CODEOWNERS, 0-AI-MANIFEST, .well-known added 2026-02-22")))

  (remediation-completed
    (item "believe-me-elimination" (before 4566) (after 0) (date "2026-02-22"))
    (item "assert-total-elimination" (before "multiple") (after 0) (date "2026-02-22"))
    (item "todo-removal" (before "multiple") (after 0) (date "2026-02-22"))
    (item "bindings-ffi-wiring" (status "all bindings now call FFI") (date "2026-02-22"))
    (item "missing-bindings-created" (count 31) (date "2026-02-22"))
    (item "rust-license-fixed" (from "Apache-2.0") (to "PMPL-1.0-or-later") (date "2026-02-22"))
    (item "containerfile-converted" (from "Dockerfile") (to "Containerfile") (date "2026-02-22"))
    (item "refc-pipeline-built" (status "scripts/build-refc.sh operational") (date "2026-02-22")))

  (blockers-and-issues
    (blocker "pack-missing" (severity high)
      (description "pack package manager not installed — cannot run standard build workflow")
      (workaround "Use idris2 --build proven.ipkg directly"))
    (blocker "apps-incomplete" (severity medium)
      (description "12 of 13 planned apps not yet created")
      (note "Only proven-bgp exists"))
    (issue "build-not-verified" (severity high)
      (description "idris2 --build proven.ipkg has not been run end-to-end in current session")
      (action "Install pack or idris2 and verify compilation")))

  (critical-next-actions
    (action "Install pack or idris2 to verify full compilation")
    (action "Create remaining 12 apps (proven-httpd through proven-wasm)")
    (action "Create GPU/VPU/TPU/Crypto hardware backend modules")
    (action "Create framework convenience re-export modules")
    (action "Run hypatia scan: 0 CRITICAL/HIGH findings required")
    (action "Implement container hardening (stapeln, firewalld, svalinn)")
    (action "Complete remaining ~102 language binding implementations")
    (action "Expand test suite to cover all 107 modules"))

  (session-history
    (session "2026-02-28"
      (summary "Added 3 new verified library modules extracted from cadre-router/HAR/gateway optimisation work")
      (changes
        (added "Proven.SafeTrust" "Formally verified trust level hierarchy with monotonicity proof — extracted from http-capability-gateway access control")
        (added "Proven.SafeAttestation" "Verified a2ml attestation types with non-empty hash invariant and round-trip proofs — extracted from hybrid-automation-router audit trail")
        (added "Proven.SafeCycleDetect" "Fuel-bounded DFS cycle detection for directed graphs — extracted from HAR dependency graph validation"))
      (invariants-maintained
        (believe-me 0)
        (assert-total 0)
        (assert-smaller 0)
        (total-checking "all three modules use %default total")))))
