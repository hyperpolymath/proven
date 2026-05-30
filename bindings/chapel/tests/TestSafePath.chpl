// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestSafePath.chpl - Per-module test for the WIRED SafePath wrapper.
// Exercises >= 3 inputs per public proc (happy, boundary, error).

use Proven;

proc main(): int {
  // ------- hasTraversal: safe paths --------------------------------------
  const r1 = SafePath.hasTraversal("/var/www/index.html");
  assert(r1.present, "hasTraversal(safe absolute): FFI error");
  assert(r1.value == false, "hasTraversal(safe absolute): expected false");

  const r2 = SafePath.hasTraversal("file.txt");
  assert(r2.present, "hasTraversal(safe relative): FFI error");
  assert(r2.value == false, "hasTraversal(safe relative): expected false");

  const r3 = SafePath.hasTraversal("");
  assert(r3.present, "hasTraversal(empty): FFI error");
  assert(r3.value == false, "hasTraversal(empty): expected false");

  // ------- hasTraversal: traversal sequences -----------------------------
  const r4 = SafePath.hasTraversal("../etc/passwd");
  assert(r4.present, "hasTraversal(leading-dotdot): FFI error");
  assert(r4.value == true, "hasTraversal(leading-dotdot): expected true");

  const r5 = SafePath.hasTraversal("/tmp/foo/../../root");
  assert(r5.present, "hasTraversal(embedded-dotdot): FFI error");
  assert(r5.value == true, "hasTraversal(embedded-dotdot): expected true");

  const r6 = SafePath.hasTraversal("a/b/c/../d");
  assert(r6.present, "hasTraversal(mid-dotdot): FFI error");
  assert(r6.value == true, "hasTraversal(mid-dotdot): expected true");

  // ------- hasTraversal: boundary cases (substring, not component) ------
  // ".." inside a longer component is NOT a traversal sequence.
  const r7 = SafePath.hasTraversal("foo..bar");
  assert(r7.present, "hasTraversal(dotdot-in-component): FFI error");
  assert(r7.value == false, "hasTraversal(dotdot-in-component): expected false");

  const r8 = SafePath.hasTraversal("...");
  assert(r8.present, "hasTraversal(triple-dot component): FFI error");
  assert(r8.value == false, "hasTraversal(triple-dot component): expected false");

  writeln("TestSafePath: OK");
  return 0;
}
