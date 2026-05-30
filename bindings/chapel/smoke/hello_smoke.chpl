// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// hello_smoke.chpl - End-to-end "does the binding link and run" smoke test.
//
// Builds:
//   chpl -o hello_smoke smoke/hello_smoke.chpl -M src/ -lproven
//
// Runs in well under 2s and is kept green by chapel-ci.yml even if larger
// jobs go red.  This is the "the binding actually works" gate.
//
// Calls the WIRED subset only.  Lifecycle (provenInit/Deinit) is GATED on
// proven#88 because the Zig export ABI does not yet match the proven.h
// declaration; the wired primitives are pure functions and need no init.

use Proven;

proc main(): int {
  // SafePath.hasTraversal — happy path (no traversal) and detection path.
  const safePath = "/var/www/html/index.html";
  const evilPath = "../../etc/passwd";

  const okSafe = SafePath.hasTraversal(safePath);
  const okEvil = SafePath.hasTraversal(evilPath);

  if okSafe == none || okSafe! != false {
    writeln("FAIL: SafePath.hasTraversal('", safePath, "') did not return some(false)");
    return 1;
  }
  if okEvil == none || okEvil! != true {
    writeln("FAIL: SafePath.hasTraversal('", evilPath, "') did not return some(true)");
    return 1;
  }

  // SafeHeader.hasCrlf — happy path (no CRLF) and detection path.
  const safeHeader = "application/json";
  const evilHeader = "value\r\nX-Injected: hi";

  const headerOkSafe = SafeHeader.hasCrlf(safeHeader);
  const headerOkEvil = SafeHeader.hasCrlf(evilHeader);

  if headerOkSafe == none || headerOkSafe! != false {
    writeln("FAIL: SafeHeader.hasCrlf('", safeHeader, "') did not return some(false)");
    return 1;
  }
  if headerOkEvil == none || headerOkEvil! != true {
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
