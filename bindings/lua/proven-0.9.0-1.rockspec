-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

package = "proven"
version = "0.9.0-1"
source = {
   url = "git://github.com/hyperpolymath/proven.git",
   tag = "v0.9.0"
}
description = {
   summary = "Safety-first utility functions with formal verification guarantees",
   detailed = [[
      Proven provides safe operations for math, strings, paths, email,
      network addresses, and cryptographic comparisons. All functions
      are designed to never crash and return nil or error on invalid input.
   ]],
   homepage = "https://github.com/hyperpolymath/proven",
   license = "PMPL-1.0"
}
dependencies = {
   "lua >= 5.1"
}
build = {
   type = "builtin",
   modules = {
      ["proven"] = "proven/init.lua",
      ["proven.safe_math"] = "proven/safe_math.lua",
      ["proven.safe_string"] = "proven/safe_string.lua",
      ["proven.safe_path"] = "proven/safe_path.lua",
      ["proven.safe_email"] = "proven/safe_email.lua",
      ["proven.safe_network"] = "proven/safe_network.lua",
      ["proven.safe_crypto"] = "proven/safe_crypto.lua"
   }
}
