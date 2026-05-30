// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestSafeHeader.chpl - Per-module test for the WIRED SafeHeader wrapper.
//
// Builds:
//   chpl -o TestSafeHeader tests/TestSafeHeader.chpl -M src/ -lproven
//
// Exercises >= 3 inputs per public proc (happy, boundary, error).

use Proven;

proc main(): int {
  // ------- hasCrlf: safe values -----------------------------------------
  const r1 = SafeHeader.hasCrlf("application/json");
  assert(r1 != none, "hasCrlf(content-type-style): FFI error");
  assert(r1! == false, "hasCrlf(content-type-style): expected false");

  const r2 = SafeHeader.hasCrlf("Bearer abc.def.ghi");
  assert(r2 != none, "hasCrlf(authorization-style): FFI error");
  assert(r2! == false, "hasCrlf(authorization-style): expected false");

  const r3 = SafeHeader.hasCrlf("");
  assert(r3 != none, "hasCrlf(empty): FFI error");
  assert(r3! == false, "hasCrlf(empty): expected false");

  // ------- hasCrlf: injection sequences ---------------------------------
  const r4 = SafeHeader.hasCrlf("value\r\nX-Admin: true");
  assert(r4 != none, "hasCrlf(classic-CRLF-injection): FFI error");
  assert(r4! == true, "hasCrlf(classic-CRLF-injection): expected true");

  const r5 = SafeHeader.hasCrlf("trailing\r\n");
  assert(r5 != none, "hasCrlf(trailing-CRLF): FFI error");
  assert(r5! == true, "hasCrlf(trailing-CRLF): expected true");

  const r6 = SafeHeader.hasCrlf("\r\nleading");
  assert(r6 != none, "hasCrlf(leading-CRLF): FFI error");
  assert(r6! == true, "hasCrlf(leading-CRLF): expected true");

  // ------- hasCrlf: boundary cases --------------------------------------
  // CR alone (no LF) is NOT a CRLF injection.
  const r7 = SafeHeader.hasCrlf("bare-CR\r");
  assert(r7 != none, "hasCrlf(bare-CR): FFI error");
  assert(r7! == false, "hasCrlf(bare-CR): expected false");

  // LF alone (no CR) is also not the CRLF sequence.
  const r8 = SafeHeader.hasCrlf("bare-LF\n");
  assert(r8 != none, "hasCrlf(bare-LF): FFI error");
  assert(r8! == false, "hasCrlf(bare-LF): expected false");

  // Reversed (LF then CR) is not CRLF.
  const r9 = SafeHeader.hasCrlf("reversed\n\r");
  assert(r9 != none, "hasCrlf(reversed-LFCR): FFI error");
  assert(r9! == false, "hasCrlf(reversed-LFCR): expected false");

  writeln("TestSafeHeader: OK");
  return 0;
}
