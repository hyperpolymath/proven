%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeString - XSS prevention for Erlang

-module(proven_string).

-export([
    escape_html/1,
    escape_sql/1,
    escape_js/1,
    sanitize_default/1,
    url_encode/1,
    slugify/1
]).

%% @doc Escape HTML special characters.
-spec escape_html(string()) -> string().
escape_html(Input) ->
    lists:flatmap(fun escape_html_char/1, Input).

escape_html_char($&) -> "&amp;";
escape_html_char($<) -> "&lt;";
escape_html_char($>) -> "&gt;";
escape_html_char($") -> "&quot;";
escape_html_char($') -> "&#x27;";
escape_html_char(C) -> [C].

%% @doc Escape SQL single quotes by doubling them.
-spec escape_sql(string()) -> string().
escape_sql(Input) ->
    lists:flatmap(fun escape_sql_char/1, Input).

escape_sql_char($') -> "''";
escape_sql_char(C) -> [C].

%% @doc Escape JavaScript special characters.
-spec escape_js(string()) -> string().
escape_js(Input) ->
    lists:flatmap(fun escape_js_char/1, Input).

escape_js_char($\\) -> "\\\\";
escape_js_char($") -> "\\\"";
escape_js_char($') -> "\\'";
escape_js_char($\n) -> "\\n";
escape_js_char($\r) -> "\\r";
escape_js_char($\t) -> "\\t";
escape_js_char($<) -> "\\u003C";
escape_js_char($>) -> "\\u003E";
escape_js_char($/) -> "\\/";
escape_js_char(C) -> [C].

%% @doc Sanitize string to alphanumeric, underscore, and hyphen only.
-spec sanitize_default(string()) -> string().
sanitize_default(Input) ->
    lists:filter(fun is_safe_char/1, Input).

is_safe_char(C) when C >= $a, C =< $z -> true;
is_safe_char(C) when C >= $A, C =< $Z -> true;
is_safe_char(C) when C >= $0, C =< $9 -> true;
is_safe_char($_) -> true;
is_safe_char($-) -> true;
is_safe_char(_) -> false.

%% @doc URL-encode a string.
-spec url_encode(string()) -> string().
url_encode(Input) ->
    lists:flatmap(fun url_encode_char/1, Input).

url_encode_char(C) when C >= $a, C =< $z -> [C];
url_encode_char(C) when C >= $A, C =< $Z -> [C];
url_encode_char(C) when C >= $0, C =< $9 -> [C];
url_encode_char($-) -> "-";
url_encode_char($_) -> "_";
url_encode_char($.) -> ".";
url_encode_char($~) -> "~";
url_encode_char(C) ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 16#F)].

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.

%% @doc Convert string to URL-safe slug.
-spec slugify(string()) -> string().
slugify(Input) ->
    Lower = string:lowercase(Input),
    Filtered = lists:filter(fun is_slug_char/1, Lower),
    WithHyphens = lists:map(fun(C) -> case C of $\s -> $-; _ -> C end end, Filtered),
    collapse_hyphens(WithHyphens).

is_slug_char(C) when C >= $a, C =< $z -> true;
is_slug_char(C) when C >= $0, C =< $9 -> true;
is_slug_char($\s) -> true;
is_slug_char($-) -> true;
is_slug_char(_) -> false.

collapse_hyphens(Input) ->
    collapse_hyphens(Input, false, []).

collapse_hyphens([], _PrevHyphen, Acc) ->
    lists:reverse(Acc);
collapse_hyphens([$- | Rest], true, Acc) ->
    collapse_hyphens(Rest, true, Acc);
collapse_hyphens([$- | Rest], false, Acc) ->
    collapse_hyphens(Rest, true, [$- | Acc]);
collapse_hyphens([C | Rest], _PrevHyphen, Acc) ->
    collapse_hyphens(Rest, false, [C | Acc]).
