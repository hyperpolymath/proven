% SPDX-License-Identifier: Apache-2.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeString - XSS prevention and string sanitization for Prolog
% Provides safe string escaping and transformation utilities.

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

:- use_module(library(lists)).

%! escape_html(+Input, -Output) is det.
%
%  Escape HTML special characters to prevent XSS.
%
%  @arg Input Raw input string
%  @arg Output HTML-escaped string
escape_html(Input, Output) :-
    atom_codes(Input, Codes),
    escape_html_codes(Codes, EscapedCodes),
    atom_codes(Output, EscapedCodes).

escape_html_codes([], []).
escape_html_codes([C|Rest], Escaped) :-
    escape_html_char(C, CharEscaped),
    escape_html_codes(Rest, RestEscaped),
    append(CharEscaped, RestEscaped, Escaped).

escape_html_char(0'&, "&amp;") :- !.
escape_html_char(0'<, "&lt;") :- !.
escape_html_char(0'>, "&gt;") :- !.
escape_html_char(0'", "&quot;") :- !.
escape_html_char(0'', "&#x27;") :- !.
escape_html_char(C, [C]).

%! escape_sql(+Input, -Output) is det.
%
%  Escape SQL single quotes by doubling them.
%
%  @arg Input Raw input string
%  @arg Output SQL-escaped string
escape_sql(Input, Output) :-
    atom_codes(Input, Codes),
    escape_sql_codes(Codes, EscapedCodes),
    atom_codes(Output, EscapedCodes).

escape_sql_codes([], []).
escape_sql_codes([0''|Rest], [0'', 0''|Escaped]) :- !,
    escape_sql_codes(Rest, Escaped).
escape_sql_codes([C|Rest], [C|Escaped]) :-
    escape_sql_codes(Rest, Escaped).

%! escape_js(+Input, -Output) is det.
%
%  Escape JavaScript special characters.
%
%  @arg Input Raw input string
%  @arg Output JavaScript-escaped string
escape_js(Input, Output) :-
    atom_codes(Input, Codes),
    escape_js_codes(Codes, EscapedCodes),
    atom_codes(Output, EscapedCodes).

escape_js_codes([], []).
escape_js_codes([C|Rest], Escaped) :-
    escape_js_char(C, CharEscaped),
    escape_js_codes(Rest, RestEscaped),
    append(CharEscaped, RestEscaped, Escaped).

escape_js_char(0'\\, "\\\\") :- !.
escape_js_char(0'", "\\\"") :- !.
escape_js_char(0'', "\\'") :- !.
escape_js_char(0'\n, "\\n") :- !.
escape_js_char(0'\r, "\\r") :- !.
escape_js_char(0'\t, "\\t") :- !.
escape_js_char(0'<, "\\u003C") :- !.
escape_js_char(0'>, "\\u003E") :- !.
escape_js_char(0'/, "\\/") :- !.
escape_js_char(C, [C]).

%! escape_xml(+Input, -Output) is det.
%
%  Escape XML special characters.
%
%  @arg Input Raw input string
%  @arg Output XML-escaped string
escape_xml(Input, Output) :-
    atom_codes(Input, Codes),
    escape_xml_codes(Codes, EscapedCodes),
    atom_codes(Output, EscapedCodes).

escape_xml_codes([], []).
escape_xml_codes([C|Rest], Escaped) :-
    escape_xml_char(C, CharEscaped),
    escape_xml_codes(Rest, RestEscaped),
    append(CharEscaped, RestEscaped, Escaped).

escape_xml_char(0'&, "&amp;") :- !.
escape_xml_char(0'<, "&lt;") :- !.
escape_xml_char(0'>, "&gt;") :- !.
escape_xml_char(0'", "&quot;") :- !.
escape_xml_char(0'', "&apos;") :- !.
escape_xml_char(C, [C]).

%! escape_shell(+Input, -Output) is det.
%
%  Escape shell special characters for safe command execution.
%
%  @arg Input Raw input string
%  @arg Output Shell-escaped string (single-quoted)
escape_shell(Input, Output) :-
    atom_codes(Input, Codes),
    escape_shell_codes(Codes, EscapedCodes),
    append([0''|EscapedCodes], [0''], Quoted),
    atom_codes(Output, Quoted).

escape_shell_codes([], []).
escape_shell_codes([0''|Rest], [0'', 0'\\, 0'', 0''|Escaped]) :- !,
    escape_shell_codes(Rest, Escaped).
escape_shell_codes([C|Rest], [C|Escaped]) :-
    escape_shell_codes(Rest, Escaped).

%! sanitize_default(+Input, -Output) is det.
%
%  Keep only alphanumeric, underscore, and hyphen.
%
%  @arg Input Raw input string
%  @arg Output Sanitized string
sanitize_default(Input, Output) :-
    atom_codes(Input, Codes),
    include(is_safe_char, Codes, SafeCodes),
    atom_codes(Output, SafeCodes).

is_safe_char(C) :- C >= 0'a, C =< 0'z, !.
is_safe_char(C) :- C >= 0'A, C =< 0'Z, !.
is_safe_char(C) :- C >= 0'0, C =< 0'9, !.
is_safe_char(0'_) :- !.
is_safe_char(0'-).

%! sanitize_alphanumeric(+Input, -Output) is det.
%
%  Keep only alphanumeric characters.
%
%  @arg Input Raw input string
%  @arg Output Alphanumeric-only string
sanitize_alphanumeric(Input, Output) :-
    atom_codes(Input, Codes),
    include(is_alnum_char, Codes, AlnumCodes),
    atom_codes(Output, AlnumCodes).

is_alnum_char(C) :- C >= 0'a, C =< 0'z, !.
is_alnum_char(C) :- C >= 0'A, C =< 0'Z, !.
is_alnum_char(C) :- C >= 0'0, C =< 0'9.

%! url_encode(+Input, -Output) is det.
%
%  URL-encode a string (percent encoding).
%
%  @arg Input Raw input string
%  @arg Output URL-encoded string
url_encode(Input, Output) :-
    atom_codes(Input, Codes),
    url_encode_codes(Codes, EncodedCodes),
    atom_codes(Output, EncodedCodes).

url_encode_codes([], []).
url_encode_codes([C|Rest], Encoded) :-
    url_encode_char(C, CharEncoded),
    url_encode_codes(Rest, RestEncoded),
    append(CharEncoded, RestEncoded, Encoded).

url_encode_char(C, [C]) :-
    is_unreserved(C), !.
url_encode_char(C, [0'%, H1, H2]) :-
    High is C >> 4,
    Low is C /\ 0xF,
    hex_digit(High, H1),
    hex_digit(Low, H2).

is_unreserved(C) :- C >= 0'a, C =< 0'z, !.
is_unreserved(C) :- C >= 0'A, C =< 0'Z, !.
is_unreserved(C) :- C >= 0'0, C =< 0'9, !.
is_unreserved(0'-) :- !.
is_unreserved(0'_) :- !.
is_unreserved(0'.) :- !.
is_unreserved(0'~).

hex_digit(N, C) :- N < 10, !, C is N + 0'0.
hex_digit(N, C) :- C is N - 10 + 0'A.

%! url_decode(+Input, -Output) is semidet.
%
%  URL-decode a percent-encoded string.
%
%  @arg Input URL-encoded string
%  @arg Output Decoded string
url_decode(Input, Output) :-
    atom_codes(Input, Codes),
    url_decode_codes(Codes, DecodedCodes),
    atom_codes(Output, DecodedCodes).

url_decode_codes([], []).
url_decode_codes([0'%, H1, H2|Rest], [Byte|Decoded]) :- !,
    hex_value(H1, High),
    hex_value(H2, Low),
    Byte is (High << 4) \/ Low,
    url_decode_codes(Rest, Decoded).
url_decode_codes([0'+|Rest], [0' |Decoded]) :- !,
    url_decode_codes(Rest, Decoded).
url_decode_codes([C|Rest], [C|Decoded]) :-
    url_decode_codes(Rest, Decoded).

hex_value(C, V) :- C >= 0'0, C =< 0'9, !, V is C - 0'0.
hex_value(C, V) :- C >= 0'a, C =< 0'f, !, V is C - 0'a + 10.
hex_value(C, V) :- C >= 0'A, C =< 0'F, V is C - 0'A + 10.

%! slugify(+Input, -Output) is det.
%
%  Convert to URL-safe slug (lowercase, hyphens).
%
%  @arg Input Raw input string
%  @arg Output URL-safe slug
slugify(Input, Output) :-
    atom_codes(Input, Codes),
    maplist(to_lower_code, Codes, LowerCodes),
    include(is_slug_char, LowerCodes, FilteredCodes),
    maplist(space_to_hyphen, FilteredCodes, HyphenCodes),
    collapse_hyphens(HyphenCodes, CollapsedCodes),
    strip_hyphens(CollapsedCodes, StrippedCodes),
    atom_codes(Output, StrippedCodes).

to_lower_code(C, L) :-
    (   C >= 0'A, C =< 0'Z
    ->  L is C + 32
    ;   L = C
    ).

is_slug_char(C) :- C >= 0'a, C =< 0'z, !.
is_slug_char(C) :- C >= 0'0, C =< 0'9, !.
is_slug_char(0' ) :- !.
is_slug_char(0'-).

space_to_hyphen(0' , 0'-) :- !.
space_to_hyphen(C, C).

collapse_hyphens([], []).
collapse_hyphens([0'-], [0'-]) :- !.
collapse_hyphens([0'-, 0'-|Rest], Collapsed) :- !,
    collapse_hyphens([0'-|Rest], Collapsed).
collapse_hyphens([C|Rest], [C|Collapsed]) :-
    collapse_hyphens(Rest, Collapsed).

strip_hyphens(Codes, Stripped) :-
    strip_leading_hyphens(Codes, Temp),
    reverse(Temp, Rev),
    strip_leading_hyphens(Rev, RevStripped),
    reverse(RevStripped, Stripped).

strip_leading_hyphens([0'-|Rest], Stripped) :- !,
    strip_leading_hyphens(Rest, Stripped).
strip_leading_hyphens(Codes, Codes).

%! truncate(+Input, +MaxLen, -Output) is det.
%
%  Truncate string to maximum length.
%
%  @arg Input Input string
%  @arg MaxLen Maximum length
%  @arg Output Truncated string
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
%
%  Normalize whitespace (collapse multiple spaces, trim).
%
%  @arg Input Input string with whitespace
%  @arg Output Normalized string
normalize_whitespace(Input, Output) :-
    atom_codes(Input, Codes),
    collapse_whitespace(Codes, Collapsed),
    strip_whitespace(Collapsed, Stripped),
    atom_codes(Output, Stripped).

collapse_whitespace([], []).
collapse_whitespace([C|Rest], [0' |Collapsed]) :-
    is_whitespace(C), !,
    skip_whitespace(Rest, Remaining),
    collapse_whitespace(Remaining, Collapsed).
collapse_whitespace([C|Rest], [C|Collapsed]) :-
    collapse_whitespace(Rest, Collapsed).

skip_whitespace([C|Rest], Remaining) :-
    is_whitespace(C), !,
    skip_whitespace(Rest, Remaining).
skip_whitespace(Codes, Codes).

is_whitespace(0' ).
is_whitespace(0'\t).
is_whitespace(0'\n).
is_whitespace(0'\r).

strip_whitespace(Codes, Stripped) :-
    strip_leading_whitespace(Codes, Temp),
    reverse(Temp, Rev),
    strip_leading_whitespace(Rev, RevStripped),
    reverse(RevStripped, Stripped).

strip_leading_whitespace([C|Rest], Stripped) :-
    is_whitespace(C), !,
    strip_leading_whitespace(Rest, Stripped).
strip_leading_whitespace(Codes, Codes).

%! is_ascii(+Input) is semidet.
%
%  Check if string contains only ASCII characters (0-127).
%
%  @arg Input String to check
is_ascii(Input) :-
    atom_codes(Input, Codes),
    forall(member(C, Codes), (C >= 0, C =< 127)).

%! is_printable(+Input) is semidet.
%
%  Check if string contains only printable ASCII characters (32-126).
%
%  @arg Input String to check
is_printable(Input) :-
    atom_codes(Input, Codes),
    forall(member(C, Codes), (C >= 32, C =< 126)).
