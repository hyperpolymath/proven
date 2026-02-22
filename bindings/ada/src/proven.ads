--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Proven: Thin FFI bindings to libproven shared library.
--
--  Architecture:
--    Core implementation: Idris 2 with dependent types and totality checking
--    FFI layer: Zig bridges Idris to C ABI (libproven.so / libproven.a)
--    This package: Ada wrappers that call libproven via pragma Import(C, ...)
--
--  All computation happens in the Idris2/Zig core. Ada code only
--  marshals data to/from the C ABI -- no logic is reimplemented.
--
--  Version: 0.9.0
--  Module Count: 38

package Proven is

   Version      : constant String := "0.9.0";
   Module_Count : constant := 38;

end Proven;
