;; SPDX-License-Identifier: PMPL-1.0-or-later
(bot-directive
  (bot "*")
  (scope "proven-core")
  (deny
    ("src/**/*.idr" "ffi/zig/**" "bindings/**"))
  (notes
    "No bot may edit Idris core, Zig FFI, or bindings without explicit approval."))
