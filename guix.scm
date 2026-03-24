;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
;;
;; Guix development environment for proven.
;; Usage: guix shell -D -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (gnu packages idris)
             (gnu packages zig)
             (gnu packages rust)
             (gnu packages crates-io)
             (gnu packages pkg-config))

(package
  (name "proven")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   (list idris2
         zig
         rust
         rust-cargo
         pkg-config))
  (synopsis "Formally verified safety via Idris2")
  (description
   "Proven provides formally verified safety primitives using Idris2
dependent types, with Zig FFI bindings and Rust integration.")
  (license #f))
