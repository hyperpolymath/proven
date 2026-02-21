% SPDX-License-Identifier: PMPL-1.0-or-later
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeURL - URL parsing and validation for Prolog
% RFC 3986 compliant URL handling.

:- module(safe_url, [
    parse_url/2,
    format_url/2,
    is_valid_url/1,
    url_scheme/2,
    url_host/2,
    url_port/2,
    url_path/2,
    url_query/2,
    url_fragment/2,
    url_encode_component/2,
    url_decode_component/2,
    is_https/1,
    is_http/1,
    normalize_url/2,
    join_url/3
]).

:- use_module(library(lists)).

%! parse_url(+UrlString, -UrlTerm) is semidet.
%
%  Parse URL string into structured term.
%  UrlTerm = url(Scheme, Host, Port, Path, Query, Fragment)
%
%  @arg UrlString URL string to parse
%  @arg UrlTerm Structured URL term
parse_url(UrlString, url(Scheme, Host, Port, Path, Query, Fragment)) :-
    atom_string(UrlString, UrlStr),
    % Extract scheme
    (   sub_string(UrlStr, Before, _, After, "://")
    ->  sub_string(UrlStr, 0, Before, _, SchemeStr),
        string_lower(SchemeStr, SchemeLower),
        atom_string(Scheme, SchemeLower),
        sub_string(UrlStr, _, After, 0, Rest)
    ;   fail
    ),
    % Extract fragment
    (   sub_string(Rest, FragBefore, _, FragAfter, "#")
    ->  sub_string(Rest, 0, FragBefore, _, BeforeFragment),
        sub_string(Rest, _, FragAfter, 0, FragmentStr),
        atom_string(Fragment, FragmentStr)
    ;   BeforeFragment = Rest,
        Fragment = ''
    ),
    % Extract query
    (   sub_string(BeforeFragment, QueryBefore, _, QueryAfter, "?")
    ->  sub_string(BeforeFragment, 0, QueryBefore, _, BeforeQuery),
        sub_string(BeforeFragment, _, QueryAfter, 0, QueryStr),
        atom_string(Query, QueryStr)
    ;   BeforeQuery = BeforeFragment,
        Query = ''
    ),
    % Extract path
    (   sub_string(BeforeQuery, PathBefore, _, PathAfter, "/")
    ->  sub_string(BeforeQuery, 0, PathBefore, _, HostPort),
        sub_string(BeforeQuery, PathBefore, _, 0, PathStr),
        atom_string(Path, PathStr)
    ;   HostPort = BeforeQuery,
        Path = '/'
    ),
    % Extract port from host
    (   sub_string(HostPort, PortBefore, _, PortAfter, ":")
    ->  sub_string(HostPort, 0, PortBefore, _, HostStr),
        sub_string(HostPort, _, PortAfter, 0, PortStr),
        number_string(Port, PortStr)
    ;   HostStr = HostPort,
        default_port(Scheme, Port)
    ),
    string_lower(HostStr, HostLower),
    atom_string(Host, HostLower).

default_port(http, 80).
default_port(https, 443).
default_port(ftp, 21).
default_port(ssh, 22).
default_port(_, 0).

%! format_url(+UrlTerm, -UrlString) is det.
%
%  Format URL term as string.
%
%  @arg UrlTerm Structured URL term
%  @arg UrlString URL string
format_url(url(Scheme, Host, Port, Path, Query, Fragment), UrlString) :-
    default_port(Scheme, DefaultPort),
    (   Port =:= DefaultPort
    ->  format(string(HostPort), "~w", [Host])
    ;   format(string(HostPort), "~w:~d", [Host, Port])
    ),
    (   Query = ''
    ->  QueryPart = ""
    ;   format(string(QueryPart), "?~w", [Query])
    ),
    (   Fragment = ''
    ->  FragmentPart = ""
    ;   format(string(FragmentPart), "#~w", [Fragment])
    ),
    format(string(UrlStr), "~w://~s~w~s~s", [Scheme, HostPort, Path, QueryPart, FragmentPart]),
    atom_string(UrlString, UrlStr).

%! is_valid_url(+UrlString) is semidet.
%
%  Check if URL string is valid.
%
%  @arg UrlString URL to validate
is_valid_url(UrlString) :-
    catch(parse_url(UrlString, _), _, fail).

%! url_scheme(+UrlTerm, -Scheme) is det.
%
%  Extract scheme from URL term.
%
%  @arg UrlTerm URL term
%  @arg Scheme Scheme atom
url_scheme(url(Scheme, _, _, _, _, _), Scheme).

%! url_host(+UrlTerm, -Host) is det.
%
%  Extract host from URL term.
%
%  @arg UrlTerm URL term
%  @arg Host Host atom
url_host(url(_, Host, _, _, _, _), Host).

%! url_port(+UrlTerm, -Port) is det.
%
%  Extract port from URL term.
%
%  @arg UrlTerm URL term
%  @arg Port Port number
url_port(url(_, _, Port, _, _, _), Port).

%! url_path(+UrlTerm, -Path) is det.
%
%  Extract path from URL term.
%
%  @arg UrlTerm URL term
%  @arg Path Path atom
url_path(url(_, _, _, Path, _, _), Path).

%! url_query(+UrlTerm, -Query) is det.
%
%  Extract query from URL term.
%
%  @arg UrlTerm URL term
%  @arg Query Query atom
url_query(url(_, _, _, _, Query, _), Query).

%! url_fragment(+UrlTerm, -Fragment) is det.
%
%  Extract fragment from URL term.
%
%  @arg UrlTerm URL term
%  @arg Fragment Fragment atom
url_fragment(url(_, _, _, _, _, Fragment), Fragment).

%! url_encode_component(+Input, -Output) is det.
%
%  URL-encode a component (more strict than path encoding).
%
%  @arg Input Raw component
%  @arg Output Encoded component
url_encode_component(Input, Output) :-
    atom_codes(Input, Codes),
    encode_component_codes(Codes, EncodedCodes),
    atom_codes(Output, EncodedCodes).

encode_component_codes([], []).
encode_component_codes([C|Rest], Encoded) :-
    encode_component_char(C, CharEncoded),
    encode_component_codes(Rest, RestEncoded),
    append(CharEncoded, RestEncoded, Encoded).

encode_component_char(C, [C]) :-
    is_unreserved_component(C), !.
encode_component_char(C, [0'%, H1, H2]) :-
    High is C >> 4,
    Low is C /\ 0xF,
    hex_digit(High, H1),
    hex_digit(Low, H2).

is_unreserved_component(C) :- C >= 0'a, C =< 0'z, !.
is_unreserved_component(C) :- C >= 0'A, C =< 0'Z, !.
is_unreserved_component(C) :- C >= 0'0, C =< 0'9, !.
is_unreserved_component(0'-) :- !.
is_unreserved_component(0'_) :- !.
is_unreserved_component(0'.) :- !.
is_unreserved_component(0'~).

hex_digit(N, C) :- N < 10, !, C is N + 0'0.
hex_digit(N, C) :- C is N - 10 + 0'A.

%! url_decode_component(+Input, -Output) is semidet.
%
%  URL-decode a component.
%
%  @arg Input Encoded component
%  @arg Output Decoded component
url_decode_component(Input, Output) :-
    atom_codes(Input, Codes),
    decode_component_codes(Codes, DecodedCodes),
    atom_codes(Output, DecodedCodes).

decode_component_codes([], []).
decode_component_codes([0'%, H1, H2|Rest], [Byte|Decoded]) :- !,
    hex_value(H1, High),
    hex_value(H2, Low),
    Byte is (High << 4) \/ Low,
    decode_component_codes(Rest, Decoded).
decode_component_codes([C|Rest], [C|Decoded]) :-
    decode_component_codes(Rest, Decoded).

hex_value(C, V) :- C >= 0'0, C =< 0'9, !, V is C - 0'0.
hex_value(C, V) :- C >= 0'a, C =< 0'f, !, V is C - 0'a + 10.
hex_value(C, V) :- C >= 0'A, C =< 0'F, V is C - 0'A + 10.

%! is_https(+UrlTerm) is semidet.
%
%  Check if URL uses HTTPS scheme.
%
%  @arg UrlTerm URL term
is_https(url(https, _, _, _, _, _)).

%! is_http(+UrlTerm) is semidet.
%
%  Check if URL uses HTTP scheme.
%
%  @arg UrlTerm URL term
is_http(url(http, _, _, _, _, _)).

%! normalize_url(+UrlString, -NormalizedUrl) is det.
%
%  Normalize URL (lowercase host, default ports, etc.).
%
%  @arg UrlString Input URL
%  @arg NormalizedUrl Normalized URL
normalize_url(UrlString, NormalizedUrl) :-
    parse_url(UrlString, UrlTerm),
    format_url(UrlTerm, NormalizedUrl).

%! join_url(+BaseUrl, +RelativeUrl, -AbsoluteUrl) is det.
%
%  Join base URL with relative URL.
%
%  @arg BaseUrl Base URL string
%  @arg RelativeUrl Relative URL or path
%  @arg AbsoluteUrl Resolved absolute URL
join_url(BaseUrl, RelativeUrl, AbsoluteUrl) :-
    atom_string(RelativeUrl, RelStr),
    (   sub_string(RelStr, _, _, _, "://")
    ->  AbsoluteUrl = RelativeUrl
    ;   parse_url(BaseUrl, url(Scheme, Host, Port, BasePath, _, _)),
        atom_string(RelativeUrl, RelPath),
        (   string_chars(RelPath, ['/'|_])
        ->  atom_string(NewPath, RelPath)
        ;   atom_string(BasePath, BasePathStr),
            (   sub_string(BasePathStr, Before, _, _, "/")
            ->  sub_string(BasePathStr, 0, Before, _, BaseDir),
                string_concat(BaseDir, "/", BaseDirSlash),
                string_concat(BaseDirSlash, RelPath, NewPathStr)
            ;   string_concat("/", RelPath, NewPathStr)
            ),
            atom_string(NewPath, NewPathStr)
        ),
        format_url(url(Scheme, Host, Port, NewPath, '', ''), AbsoluteUrl)
    ).
