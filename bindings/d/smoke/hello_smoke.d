// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

/**
 * hello_smoke.d - End-to-end "does the binding link and run" smoke test.
 */
import std.stdio;
import std.process;
import proven;

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

    writeln("hello_smoke: OK (libproven ", VERSION, ")");
    return 0;
}
