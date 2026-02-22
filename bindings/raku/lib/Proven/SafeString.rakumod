# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafeString - Safe text operations that handle encoding correctly.
#
# Thin wrapper over libproven's SafeString module.  All escaping and validation
# is performed in formally verified Idris 2 code.  This module does NOT
# reimplement any string logic.

unit module Proven::SafeString;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# Validation
# ============================================================================

#| Check if the given bytes are valid UTF-8.
#| Returns True/False, or Nil on internal error.
sub is-valid-utf8(Str:D $s --> Bool) is export {
    my ($buf, $len) = str-to-buf($s);
    my BoolResult $r = proven_string_is_valid_utf8(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return $r.value;
}

# ============================================================================
# Escaping
# ============================================================================

#| Escape a string for SQL (single-quote escaping).
#| Returns the escaped string, or Nil on error.
#| Note: Prefer parameterized queries over string escaping.
sub escape-sql(Str:D $s --> Str) is export {
    my ($buf, $len) = str-to-buf($s);
    my StringResult $r = proven_string_escape_sql(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}

#| Escape a string for HTML (prevents XSS).
#| Returns the escaped string, or Nil on error.
sub escape-html(Str:D $s --> Str) is export {
    my ($buf, $len) = str-to-buf($s);
    my StringResult $r = proven_string_escape_html(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}

#| Escape a string for JavaScript string literals.
#| Returns the escaped string, or Nil on error.
sub escape-js(Str:D $s --> Str) is export {
    my ($buf, $len) = str-to-buf($s);
    my StringResult $r = proven_string_escape_js(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}
