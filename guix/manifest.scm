;; Guix manifest for proven development tooling.
(use-modules (guix profiles))

(specifications->manifest
  (list
    "idris2"
    "zig"
    "git"
    "gcc-toolchain"
    "gnumake"
    "pkg-config"))
