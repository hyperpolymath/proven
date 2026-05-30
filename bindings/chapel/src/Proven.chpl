// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Proven - Umbrella module that re-exports the WIRED Proven sub-modules.
//
// All computation is performed in the libproven core via the Zig FFI
// bridge.  This module is a thin wrapper; it does NOT reimplement any
// logic.
//
// The umbrella pulls in ONLY the WIRED safety modules (those whose
// extern procs are exported by libproven 0.9.0 and verified by
// chapel-symbol-audit).  GATED modules — SafeMath, SafeString,
// SafeEmail, SafeUrl, SafeCrypto, SafeJson, SafeDateTime — remain
// in src/ as the documented C-ABI contract per proven.h, but they
// are NOT re-exported here, because that would pull their GATED
// extern call sites into the link line of any program that does
// `use Proven`, breaking the link.
//
// A user who wants to call a GATED wrapper today (knowing it will
// link-fail until proven#88 lands the corresponding export) can
// import it directly: `use SafeMath;`.  Each GATED module's header
// comment names the precise proven#88 sub-export it waits on.
//
// Usage (wired subset, links + runs against libproven 0.9.0):
//   use Proven;
//   var trav = SafePath.hasTraversal("../etc/passwd");      // some(true)
//   var bad  = SafeHeader.hasCrlf("X-foo: bar\r\nX-y: z");  // some(true)
//   writeln(libraryVersion());

module Proven {

  // Core FFI declarations + helpers (extern types, WIRED externs,
  // GATED extern decls kept under a GATED banner inside LibProven).
  // `use` brings procs into scope unqualified; `import` makes the
  // module name itself visible as a qualifier.
  public use LibProven;
  public import LibProven;

  // WIRED safety modules (smoke + chapel-tests target these).
  public use SafePath;
  public import SafePath;
  public use SafeHeader;
  public import SafeHeader;

}
