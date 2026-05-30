// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestSafePath.chpl - Per-module test for the WIRED SafePath wrapper.
//
// Builds:
//   chpl -o TestSafePath tests/TestSafePath.chpl -M src/ -lproven
//
// Exercises >= 3 inputs per public proc (happy, boundary, error).

use Proven;

proc main(): int {
  // ------- hasTraversal: safe paths --------------------------------------
  const r1 = SafePath.hasTraversal("/var/www/index.html");
  assert(r1 != none, "hasTraversal(safe absolute): FFI error");
  assert(r1! == false, "hasTraversal(safe absolute): expected false");

  const r2 = SafePath.hasTraversal("file.txt");
  assert(r2 != none, "hasTraversal(safe relative): FFI error");
  assert(r2! == false, "hasTraversal(safe relative): expected false");

  const r3 = SafePath.hasTraversal("");
  assert(r3 != none, "hasTraversal(empty): FFI error");
  assert(r3! == false, "hasTraversal(empty): expected false");

  // ------- hasTraversal: traversal sequences -----------------------------
  const r4 = SafePath.hasTraversal("../etc/passwd");
  assert(r4 != none, "hasTraversal(leading-dotdot): FFI error");
  assert(r4! == true, "hasTraversal(leading-dotdot): expected true");

  const r5 = SafePath.hasTraversal("/tmp/foo/../../root");
  assert(r5 != none, "hasTraversal(embedded-dotdot): FFI error");
  assert(r5! == true, "hasTraversal(embedded-dotdot): expected true");

  const r6 = SafePath.hasTraversal("a/b/c/../d");
  assert(r6 != none, "hasTraversal(mid-dotdot): FFI error");
  assert(r6! == true, "hasTraversal(mid-dotdot): expected true");

  // ------- hasTraversal: boundary cases (substring, not component) ------
  // ".." inside a longer component is NOT a traversal sequence.
  const r7 = SafePath.hasTraversal("foo..bar");
  assert(r7 != none, "hasTraversal(dotdot-in-component): FFI error");
  assert(r7! == false, "hasTraversal(dotdot-in-component): expected false");

  const r8 = SafePath.hasTraversal("...");
  assert(r8 != none, "hasTraversal(triple-dot component): FFI error");
  assert(r8! == false, "hasTraversal(triple-dot component): expected false");

  writeln("TestSafePath: OK");
  return 0;
}
