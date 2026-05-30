// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestFfiContract.chpl - Differential test: Chapel wrapper proc vs the
// underlying extern C ABI call.
//
// For each WIRED safety primitive we exercise the same input via both
// (a) the Chapel-idiomatic wrapper and (b) the raw extern proc returning
// the C ABI struct, and assert the results agree on status code and
// value bit-for-bit.
//
// This is the binding-tier soundness check described in the chapel
// pilot brief: it shows the wrapper preserves the C ABI contract.  It
// does NOT re-prove the Idris2 layer.

use Proven;
use CTypes;

proc checkPath(input: string, expected: bool) {
  var (ptr, len) = toCBytes(input);
  const raw = provenPathHasTraversal(ptr, len);
  assert(isOk(raw.status),
         "raw provenPathHasTraversal status != OK for input: " + input);
  assert(raw.value == expected,
         "raw provenPathHasTraversal expected " + expected:string +
         " for input: " + input);

  const wrapped = SafePath.hasTraversal(input);
  assert(wrapped.present,
         "SafePath.hasTraversal returned absent for input: " + input);
  assert(wrapped.value == expected,
         "SafePath.hasTraversal expected " + expected:string +
         " for input: " + input);

  // Differential: wrapper vs raw must agree.
  assert(wrapped.value == raw.value,
         "differential mismatch: SafePath.hasTraversal vs provenPathHasTraversal " +
         "for input: " + input);
}

proc checkHeader(input: string, expected: bool) {
  var (ptr, len) = toCBytes(input);
  const raw = provenHeaderHasCrlf(ptr, len);
  assert(isOk(raw.status),
         "raw provenHeaderHasCrlf status != OK for input: " + input);
  assert(raw.value == expected,
         "raw provenHeaderHasCrlf expected " + expected:string +
         " for input: " + input);

  const wrapped = SafeHeader.hasCrlf(input);
  assert(wrapped.present,
         "SafeHeader.hasCrlf returned absent for input: " + input);
  assert(wrapped.value == expected,
         "SafeHeader.hasCrlf expected " + expected:string +
         " for input: " + input);

  assert(wrapped.value == raw.value,
         "differential mismatch: SafeHeader.hasCrlf vs provenHeaderHasCrlf " +
         "for input: " + input);
}

proc main(): int {
  // SafePath: cover safe + traversal + boundary
  checkPath("/var/www/index.html",      false);
  checkPath("file.txt",                  false);
  checkPath("..",                        true);
  checkPath("../etc/passwd",             true);
  checkPath("/tmp/foo/../../root",       true);
  checkPath("foo..bar",                  false);

  // SafeHeader: cover safe + injection + boundary
  checkHeader("application/json",        false);
  checkHeader("Bearer abc.def",           false);
  checkHeader("value\r\nX-Admin: true",  true);
  checkHeader("trailing\r\n",            true);
  checkHeader("bare-CR\r",               false);
  checkHeader("bare-LF\n",               false);

  writeln("TestFfiContract: OK");
  return 0;
}
