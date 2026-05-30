// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// hello_smoke.chpl - End-to-end "does the binding link and run" smoke test.
//
// Runs in well under 2s and is kept green by chapel-ci.yml even if larger
// jobs go red.
//
// Calls the WIRED subset only.  Lifecycle (provenInit/Deinit) is GATED on
// proven#88 because the Zig export ABI does not yet match the proven.h
// declaration; the wired primitives are pure functions and need no init.

use Proven;

proc main(): int {
  // SafePath.hasTraversal — happy + detection paths.
  const okSafe = SafePath.hasTraversal("/var/www/html/index.html");
  const okEvil = SafePath.hasTraversal("../../etc/passwd");

  if !okSafe.present || okSafe.value != false {
    writeln("FAIL: SafePath.hasTraversal('/var/www/html/index.html') did not return some(false)");
    return 1;
  }
  if !okEvil.present || okEvil.value != true {
    writeln("FAIL: SafePath.hasTraversal('../../etc/passwd') did not return some(true)");
    return 1;
  }

  // SafeHeader.hasCrlf — happy + detection paths.
  const headerOkSafe = SafeHeader.hasCrlf("application/json");
  const headerOkEvil = SafeHeader.hasCrlf("value\r\nX-Injected: hi");

  if !headerOkSafe.present || headerOkSafe.value != false {
    writeln("FAIL: SafeHeader.hasCrlf('application/json') did not return some(false)");
    return 1;
  }
  if !headerOkEvil.present || headerOkEvil.value != true {
    writeln("FAIL: SafeHeader.hasCrlf(<CRLF>) did not return some(true)");
    return 1;
  }

  // Library identification (read-only, no allocation; verifies symbol table).
  const ver = libraryVersion();
  const info = libraryBuildInfo();
  if ver.size == 0 {
    writeln("FAIL: libraryVersion() returned empty string");
    return 1;
  }
  if info.size == 0 {
    writeln("FAIL: libraryBuildInfo() returned empty string");
    return 1;
  }

  writeln("hello_smoke: OK (libproven ", ver, ")");
  return 0;
}
