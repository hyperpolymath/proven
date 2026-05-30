// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// TestLibraryInfo.chpl - Per-module test for the WIRED library-identification
// accessors (proven_version + proven_build_info).
//
// These are the cheapest possible end-to-end checks that libproven is the
// version the binding expects.  They verify the symbol table answer is
// non-empty and reasonable; chapel-symbol-audit verifies the symbols exist.

use Proven;

proc main(): int {
  const ver = libraryVersion();
  assert(ver.size > 0, "libraryVersion() returned empty string");
  // proven_version exports a semver-shaped string; reject obviously bogus
  // values without locking the test to a specific release.
  assert(ver.size < 64, "libraryVersion() returned implausibly long string");
  assert(ver.find('.') >= 0, "libraryVersion() does not look like semver: " + ver);

  const info = libraryBuildInfo();
  assert(info.size > 0, "libraryBuildInfo() returned empty string");
  assert(info.size < 256, "libraryBuildInfo() returned implausibly long string");

  writeln("TestLibraryInfo: OK (version=", ver, ")");
  return 0;
}
