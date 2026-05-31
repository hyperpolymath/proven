// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestSafeHeader.chpl - Per-module test for the WIRED SafeHeader wrapper.
// Exercises >= 3 inputs per public proc (happy, boundary, error).

use Proven;

proc main(): int {
  // ------- hasCrlf: safe values -----------------------------------------
  const r1 = SafeHeader.hasCrlf("application/json");
  assert(r1.present, "hasCrlf(content-type-style): FFI error");
  assert(r1.value == false, "hasCrlf(content-type-style): expected false");

  const r2 = SafeHeader.hasCrlf("Bearer abc.def.ghi");
  assert(r2.present, "hasCrlf(authorization-style): FFI error");
  assert(r2.value == false, "hasCrlf(authorization-style): expected false");

  const r3 = SafeHeader.hasCrlf("");
  assert(r3.present, "hasCrlf(empty): FFI error");
  assert(r3.value == false, "hasCrlf(empty): expected false");

  // ------- hasCrlf: injection sequences ---------------------------------
  const r4 = SafeHeader.hasCrlf("value\r\nX-Admin: true");
  assert(r4.present, "hasCrlf(classic-CRLF-injection): FFI error");
  assert(r4.value == true, "hasCrlf(classic-CRLF-injection): expected true");

  const r5 = SafeHeader.hasCrlf("trailing\r\n");
  assert(r5.present, "hasCrlf(trailing-CRLF): FFI error");
  assert(r5.value == true, "hasCrlf(trailing-CRLF): expected true");

  const r6 = SafeHeader.hasCrlf("\r\nleading");
  assert(r6.present, "hasCrlf(leading-CRLF): FFI error");
  assert(r6.value == true, "hasCrlf(leading-CRLF): expected true");

  // ------- hasCrlf: boundary cases --------------------------------------
  // CR alone (no LF) is NOT a CRLF injection.
  const r7 = SafeHeader.hasCrlf("bare-CR\r");
  assert(r7.present, "hasCrlf(bare-CR): FFI error");
  assert(r7.value == false, "hasCrlf(bare-CR): expected false");

  // LF alone (no CR) is also not the CRLF sequence.
  const r8 = SafeHeader.hasCrlf("bare-LF\n");
  assert(r8.present, "hasCrlf(bare-LF): FFI error");
  assert(r8.value == false, "hasCrlf(bare-LF): expected false");

  // Reversed (LF then CR) is not CRLF.
  const r9 = SafeHeader.hasCrlf("reversed\n\r");
  assert(r9.present, "hasCrlf(reversed-LFCR): FFI error");
  assert(r9.value == false, "hasCrlf(reversed-LFCR): expected false");

  writeln("TestSafeHeader: OK");
  return 0;
}
