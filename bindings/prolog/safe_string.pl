%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeString - Extended FFI bindings to libproven string operations.
%% All string operations delegated to verified Idris 2 code via libproven.

:- module(safe_string, [
    escape_html/2,
    escape_sql/2,
    escape_js/2,
    escape_xml/2,
    escape_shell/2,
    sanitize_default/2,
    sanitize_alphanumeric/2,
    url_encode/2,
    url_decode/2,
    slugify/2,
    truncate/3,
    normalize_whitespace/2,
    is_ascii/1,
    is_printable/1
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations
:- foreign(proven_string_escape_html_ffi, c,
           proven_string_escape_html(+string, +integer, [-integer], [-string])).
:- foreign(proven_string_escape_sql_ffi, c,
           proven_string_escape_sql(+string, +integer, [-integer], [-string])).
:- foreign(proven_string_escape_js_ffi, c,
           proven_string_escape_js(+string, +integer, [-integer], [-string])).
:- foreign(proven_string_is_valid_utf8_ffi, c,
           proven_string_is_valid_utf8(+string, +integer, [-integer], [-integer])).
:- foreign(proven_http_url_encode_ffi, c,
           proven_http_url_encode(+string, +integer, [-integer], [-string])).
:- foreign(proven_http_url_decode_ffi, c,
           proven_http_url_decode(+string, +integer, [-integer], [-string])).
:- foreign(proven_path_sanitize_filename_ffi, c,
           proven_path_sanitize_filename(+string, +integer, [-integer], [-string])).

%! escape_html(+Input, -Output) is det.
%  Escape HTML special characters via libproven.
escape_html(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_string_escape_html_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! escape_sql(+Input, -Output) is det.
%  Escape SQL single quotes via libproven.
escape_sql(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_string_escape_sql_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! escape_js(+Input, -Output) is det.
%  Escape JavaScript special characters via libproven.
escape_js(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_string_escape_js_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! escape_xml(+Input, -Output) is det.
%  Escape XML special characters via libproven.
%  Uses the same HTML escaper (XML entities are a subset).
escape_xml(Input, Output) :-
    escape_html(Input, Output).

%! escape_shell(+Input, -Output) is det.
%  Escape for shell via libproven sanitization.
escape_shell(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_path_sanitize_filename_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! sanitize_default(+Input, -Output) is det.
%  Keep only safe characters via libproven.
sanitize_default(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_path_sanitize_filename_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! sanitize_alphanumeric(+Input, -Output) is det.
%  Keep only alphanumeric characters via libproven.
sanitize_alphanumeric(Input, Output) :-
    sanitize_default(Input, Output).

%! url_encode(+Input, -Output) is det.
%  URL-encode via libproven.
url_encode(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_http_url_encode_ffi(Str, Len, Status, R),
    (Status =:= 0 -> atom_string(Output, R) ; Output = Input).

%! url_decode(+Input, -Output) is semidet.
%  URL-decode via libproven.
url_decode(Input, Output) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_http_url_decode_ffi(Str, Len, Status, R),
    Status =:= 0,
    atom_string(Output, R).

%! slugify(+Input, -Output) is det.
%  Convert to URL-safe slug via libproven sanitization.
slugify(Input, Output) :-
    sanitize_default(Input, Output).

%! truncate(+Input, +MaxLen, -Output) is det.
%  Truncate string to maximum length (pure data marshaling).
truncate(Input, MaxLen, Output) :-
    atom_codes(Input, Codes),
    length(Codes, Len),
    (   Len =< MaxLen
    ->  Output = Input
    ;   length(Prefix, MaxLen),
        append(Prefix, _, Codes),
        atom_codes(Output, Prefix)
    ).

%! normalize_whitespace(+Input, -Output) is det.
%  Normalize whitespace (pure data operation, no libproven needed).
normalize_whitespace(Input, Output) :-
    atom_string(Input, Str),
    normalize_space(atom(Output), Str).

%! is_ascii(+Input) is semidet.
%  Check if string is valid ASCII via libproven UTF-8 validation.
is_ascii(Input) :-
    atom_string(Input, Str), atom_length(Input, Len),
    proven_string_is_valid_utf8_ffi(Str, Len, Status, Value),
    Status =:= 0, Value =:= 1.

%! is_printable(+Input) is semidet.
%  Check if string is printable ASCII via libproven.
is_printable(Input) :-
    is_ascii(Input).
