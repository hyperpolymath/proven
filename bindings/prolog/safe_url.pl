%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeURL - FFI bindings to libproven URL operations.
%% All URL operations are performed in verified Idris 2 code via libproven.

:- module(safe_url, [
    url_encode/2,
    url_decode/2,
    parse_url/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven URL functions.
:- foreign(proven_http_url_encode_ffi, c,
           proven_http_url_encode(+string, +integer, [-integer], [-string])).
:- foreign(proven_http_url_decode_ffi, c,
           proven_http_url_decode(+string, +integer, [-integer], [-string])).
:- foreign(proven_url_parse_ffi, c,
           proven_url_parse(+string, +integer,
                            [-integer],
                            [-string], [-integer],
                            [-string], [-integer],
                            [-integer], [-integer],
                            [-string], [-integer],
                            [-string], [-integer],
                            [-string], [-integer])).

%! url_encode(+Input, -Output) is det.
%
%  URL-encode a string via libproven (RFC 3986 percent encoding).
url_encode(Input, Output) :-
    atom_string(Input, Str),
    atom_length(Input, Len),
    proven_http_url_encode_ffi(Str, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! url_decode(+Input, -Output) is semidet.
%
%  URL-decode a percent-encoded string via libproven.
url_decode(Input, Output) :-
    atom_string(Input, Str),
    atom_length(Input, Len),
    proven_http_url_decode_ffi(Str, Len, Status, ResultStr),
    Status =:= 0,
    atom_string(Output, ResultStr).

%! parse_url(+UrlString, -Result) is det.
%
%  Parse URL into components via libproven.
%  Result is ok(url(Scheme, Host, Port, Path, Query, Fragment))
%  or error(parse_failure).
parse_url(UrlString, Result) :-
    atom_string(UrlString, Str),
    atom_length(UrlString, Len),
    proven_url_parse_ffi(Str, Len, Status,
                         Scheme, _SchemeLen,
                         Host, _HostLen,
                         Port, HasPort,
                         Path, _PathLen,
                         Query, _QueryLen,
                         Fragment, _FragmentLen),
    (   Status =:= 0
    ->  atom_string(SchemeAtom, Scheme),
        atom_string(HostAtom, Host),
        atom_string(PathAtom, Path),
        atom_string(QueryAtom, Query),
        atom_string(FragmentAtom, Fragment),
        (   HasPort =:= 1 -> PortVal = Port ; PortVal = 0 ),
        Result = ok(url(SchemeAtom, HostAtom, PortVal, PathAtom, QueryAtom, FragmentAtom))
    ;   Result = error(parse_failure)
    ).
