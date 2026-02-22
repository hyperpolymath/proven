# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# SafeString.io - Encoding-safe text operations for the Io language.
#
# All operations delegate to libproven via the native C addon (IoProven.c).
# Returns nil on error.
#
# Usage:
#   Proven init
#   safe := Proven escapeHtml("<script>alert(1)</script>")
#   safe println
#   Proven deinit

SafeString := Proven clone do(
    //doc SafeString category Safety
    //doc SafeString description Safe text encoding and escaping operations.

    //doc SafeString isValidUtf8(str) Check if string is valid UTF-8.
    # isValidUtf8 is provided by the native C addon (IoProven.c).

    //doc SafeString escapeSql(str) SQL-escape a string. Prefer parameterized queries.
    # escapeSql is provided by the native C addon (IoProven.c).

    //doc SafeString escapeHtml(str) HTML-escape a string (prevents XSS).
    # escapeHtml is provided by the native C addon (IoProven.c).

    //doc SafeString escapeJs(str) JavaScript-escape a string.
    # escapeJs is provided by the native C addon (IoProven.c).

    //doc SafeString pathHasTraversal(path) Check for ".." directory traversal.
    # pathHasTraversal is provided by the native C addon (IoProven.c).

    //doc SafeString sanitizeFilename(name) Remove dangerous characters from a filename.
    # sanitizeFilename is provided by the native C addon (IoProven.c).
)
