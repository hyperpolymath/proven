;; SPDX-License-Identifier: Apache-2.0
(bot-directive
  (bot "*")
  (scope "proven-core")
  (deny
    ("src/**/*.idr" "ffi/zig/**" "bindings/**"))
  (notes
    "No bot may edit Idris core, Zig FFI, or bindings without explicit approval."))
