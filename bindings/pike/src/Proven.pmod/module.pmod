// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven.pmod/module.pmod - Main module for the Pike Proven binding.
//
// This module provides access to the formally verified libproven safety
// library from Pike. All computation is performed in the verified Idris 2
// core; this binding is a thin wrapper that delegates to libproven via
// the LibProven helper class.
//
// Usage:
//   import Proven;
//
//   LibProven.init();
//   int result = SafeMath.add(1000000000, 2000000000);
//   if (result != UNDEFINED)
//       write("Sum: %d\n", result);
//   LibProven.deinit();

//! @module Proven
//! Formally verified safety library backed by Idris 2.
//!
//! All computation is performed in the verified Idris 2 core via the Zig FFI
//! layer. This Pike module is a thin wrapper; it does @b{not} reimplement
//! any logic.

constant __version = "0.9.0";
constant __author  = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>";

//! @decl string version()
//! Get the library version string.
string version()
{
    return __version;
}
