;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2025 Hyperpolymath

(ns proven.core
  "Proven - FFI bindings to Idris2 verified code.

  IMPORTANT: This is a minimal FFI-only version.
  Language bindings MUST be thin wrappers, not reimplementations.
  See ADR-008 in META.scm and ARCHITECTURE-CLEANUP-2026-01-25.md

  Architecture Note:

  This file previously required 38 safe-* modules.
  Those modules were REIMPLEMENTATIONS in Clojure, not FFI wrappers.
  They bypassed Idris2 verification entirely.

  They have been deleted (backup: .UNSAFE-ALL-BINDINGS-DELETED-FINAL-20260125/).

  Future work:
  1. Build proper Zig FFI bridge (ffi/zig/)
  2. Create thin FFI wrappers in Clojure that call the Zig bridge via JNI
  3. Add modules back here as FFI wrappers only

  Until then, this is a minimal placeholder.")

;; Library metadata
(def version "0.9.1-ffi")  ;; Breaking change from v0.9.0
(def module-count 0)  ;; All modules removed pending FFI implementation
