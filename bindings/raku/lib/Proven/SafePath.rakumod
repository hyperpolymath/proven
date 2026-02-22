# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven::SafePath - Filesystem traversal prevention.
#
# Thin wrapper over libproven's SafePath module.  All path analysis is
# performed in formally verified Idris 2 code.  This module does NOT
# reimplement any path logic.

unit module Proven::SafePath;

use NativeCall;
use Proven::LibProven;

# ============================================================================
# Traversal detection
# ============================================================================

#| Check if a path contains directory traversal sequences ("..").
#| Returns True if traversal is detected, False if safe, Nil on internal error.
sub has-traversal(Str:D $path --> Bool) is export {
    my ($buf, $len) = str-to-buf($path);
    my BoolResult $r = proven_path_has_traversal(nativecast(Pointer, $buf), $len);
    return Nil unless $r.status == 0;
    return $r.value;
}

# ============================================================================
# Filename sanitization
# ============================================================================

#| Sanitize a filename by removing dangerous characters.
#| Returns the sanitized filename, or Nil on error.
sub sanitize-filename(Str:D $name --> Str) is export {
    my ($buf, $len) = str-to-buf($name);
    my StringResult $r = proven_path_sanitize_filename(nativecast(Pointer, $buf), $len);
    return extract-string($r);
}
