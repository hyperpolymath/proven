! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven.safe-string - String escaping and validation via libproven FFI.
! All computation is performed in verified Idris 2 code via the Zig FFI
! layer. This vocabulary is a thin wrapper; it does NOT reimplement any logic.

USING: kernel sequences proven.ffi accessors ;
IN: proven.safe-string

! Helper: extract string from StringResult, returning string and t, or f.
! The caller is responsible for ensuring proven_free_string is called on
! the native pointer after copying. The Factor runtime copies the c-string
! into a managed string automatically.
! ( result -- string t ) or ( result -- f f )
: string-result>value ( result -- string/f ? )
    dup status>> PROVEN-OK = [
        value>> t
    ] [
        drop f f
    ] if ;

! Helper: extract bool from BoolResult.
! ( result -- bool t ) or ( result -- f f )
: bool-result>value ( result -- bool/f ? )
    dup status>> PROVEN-OK = [
        value>> 0 = not t
    ] [
        drop f f
    ] if ;

! Check if bytes are valid UTF-8 via libproven.
! ( string -- bool t ) or ( string -- f f )
: valid-utf8? ( string -- bool/f ? )
    dup length proven_string_is_valid_utf8 bool-result>value ;

! Escape string for SQL (single quotes) via libproven.
! Prefer parameterized queries over string escaping.
! ( string -- escaped t ) or ( string -- f f )
: escape-sql ( string -- escaped/f ? )
    dup length proven_string_escape_sql string-result>value ;

! Escape string for HTML (prevents XSS) via libproven.
! ( string -- escaped t ) or ( string -- f f )
: escape-html ( string -- escaped/f ? )
    dup length proven_string_escape_html string-result>value ;

! Escape string for JavaScript string literals via libproven.
! ( string -- escaped t ) or ( string -- f f )
: escape-js ( string -- escaped/f ? )
    dup length proven_string_escape_js string-result>value ;

! Check for directory traversal sequences ("..") via libproven.
! ( string -- bool t ) or ( string -- f f )
: has-traversal? ( string -- bool/f ? )
    dup length proven_path_has_traversal bool-result>value ;

! Sanitize a filename by removing dangerous characters via libproven.
! ( string -- sanitized t ) or ( string -- f f )
: sanitize-filename ( string -- sanitized/f ? )
    dup length proven_path_sanitize_filename string-result>value ;
