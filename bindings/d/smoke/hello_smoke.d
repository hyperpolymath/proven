// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

/**
 * hello_smoke.d - End-to-end "does the binding link and run" smoke test.
 */
import std.stdio;
import std.process;
import proven;
import proven.ffi : ProvenBoolResult;

// The D package has no high-level SafeHeader wrapper yet. Exercise its C ABI
// directly using the package's actual result layout; do not duplicate policy.
extern(C) ProvenBoolResult proven_header_has_crlf(const(ubyte)* ptr, size_t len) nothrow;

int main() {
    // SafePath.hasTraversal - happy + detection paths.
    bool ok_safe = hasTraversal("/var/www/html/index.html");
    bool ok_evil = hasTraversal("../../etc/passwd");

    if (ok_safe != false) {
        writeln("FAIL: hasTraversal('/var/www/html/index.html') did not return false");
        return 1;
    }
    if (ok_evil != true) {
        writeln("FAIL: hasTraversal('../../etc/passwd') did not return true");
        return 1;
    }

    const string[] headers = ["application/json", "\r", "\n", "a\0\rZ", "\xFF\0\x80"];
    const bool[] expected = [false, true, true, true, false];
    foreach (i, value; headers) {
        auto r = proven_header_has_crlf(cast(const(ubyte)*) value.ptr, value.length);
        if (r.status != 0 || r.value != expected[i]) {
            writeln("FAIL: header C ABI byte-span case ", i);
            return 1;
        }
    }
    writeln("hello_smoke: OK (libproven ", VERSION, ")");
    return 0;
}
