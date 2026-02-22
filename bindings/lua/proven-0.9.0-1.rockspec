-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

package = "proven"
version = "0.9.0-1"
source = {
   url = "git://github.com/hyperpolymath/proven.git",
   tag = "v0.9.0"
}
description = {
   summary = "FFI binding to libproven - formally verified safety library",
   detailed = [[
      Proven provides safe operations backed by formally verified Idris 2
      code. This binding uses LuaJIT FFI to call libproven directly.
      All computation happens in the verified Idris core; this module
      is a thin FFI wrapper that never reimplements any logic.
   ]],
   homepage = "https://github.com/hyperpolymath/proven",
   license = "PMPL-1.0-or-later"
}
dependencies = {
   "lua >= 5.1"
}
external_dependencies = {
   PROVEN = {
      library = "proven"
   }
}
build = {
   type = "builtin",
   modules = {
      ["proven"] = "proven/init.lua"
   }
}
