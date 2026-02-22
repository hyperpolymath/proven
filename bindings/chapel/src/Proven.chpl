// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Main module that re-exports all Proven sub-modules.
//
// All computation is performed in the Idris 2 core via the Zig FFI bridge.
// This module is a thin wrapper; it does NOT reimplement any logic.
//
// Usage:
//   use Proven;
//   provenInit();
//   var result = safeAdd(2: int(64), 3: int(64));
//   provenDeinit();

module Proven {

  // Core FFI declarations and types.
  public use LibProven;

  // Domain-specific modules.
  public use SafeMath;
  public use SafeString;
  public use SafePath;
  public use SafeEmail;
  public use SafeUrl;
  public use SafeCrypto;
  public use SafeJson;
  public use SafeDateTime;

}
