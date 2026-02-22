%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeString - FFI bindings to libproven string operations.
%% All escaping/sanitization is performed in verified Idris 2 code via libproven.

:- module(proven_string, [
    escape_html/2,
    escape_sql/2,
    escape_js/2,
    sanitize_default/2,
    url_encode/2,
    slugify/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven string functions.
%% These accept byte pointers and lengths, returning ProvenStringResult.
:- foreign(proven_string_escape_html_ffi, c,
           proven_string_escape_html(+string, +integer, [-integer], [-string])).
:- foreign(proven_string_escape_sql_ffi, c,
           proven_string_escape_sql(+string, +integer, [-integer], [-string])).
:- foreign(proven_string_escape_js_ffi, c,
           proven_string_escape_js(+string, +integer, [-integer], [-string])).
:- foreign(proven_http_url_encode_ffi, c,
           proven_http_url_encode(+string, +integer, [-integer], [-string])).

%! escape_html(+Input, -Output) is det.
%
%  Escape HTML special characters via libproven.
%  Prevents XSS by escaping &, <, >, ", '.
escape_html(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_string_escape_html_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! escape_sql(+Input, -Output) is det.
%
%  Escape SQL single quotes via libproven.
%  Doubles single quotes for safe SQL interpolation.
escape_sql(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_string_escape_sql_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! escape_js(+Input, -Output) is det.
%
%  Escape JavaScript special characters via libproven.
escape_js(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_string_escape_js_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! url_encode(+Input, -Output) is det.
%
%  URL-encode a string via libproven (RFC 3986 percent encoding).
url_encode(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_http_url_encode_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! sanitize_default(+Input, -Output) is det.
%
%  Sanitize string via libproven, keeping only safe characters.
%  Delegates to proven_path_sanitize_filename as a proxy for
%  general sanitization.
sanitize_default(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_path_sanitize_filename_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! slugify(+Input, -Output) is det.
%
%  Convert to URL-safe slug via libproven.
%  Uses url_encode and then post-processes for slug format.
slugify(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_path_sanitize_filename_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%% Internal FFI for path sanitization (used by sanitize/slugify)
:- foreign(proven_path_sanitize_filename_ffi, c,
           proven_path_sanitize_filename(+string, +integer, [-integer], [-string])).
