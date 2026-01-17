% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeJSON - Safe JSON parsing and generation for Prolog
% Provides validated JSON handling with type checking.

:- module(safe_json, [
    json_parse/2,
    json_format/2,
    json_get/3,
    json_get/4,
    json_set/4,
    json_delete/3,
    json_keys/2,
    json_values/2,
    json_merge/3,
    json_type/2,
    is_valid_json/1,
    json_path_get/3,
    json_to_pairs/2,
    pairs_to_json/2
]).

:- use_module(library(lists)).

%! json_parse(+JsonString, -JsonTerm) is semidet.
%
%  Parse JSON string into Prolog term.
%  Objects become json([Key-Value, ...]) terms.
%  Arrays become lists.
%
%  @arg JsonString JSON string to parse
%  @arg JsonTerm Prolog term representation
json_parse(JsonString, JsonTerm) :-
    atom_string(JsonString, Str),
    string_codes(Str, Codes),
    phrase(json_value(JsonTerm), Codes, []).

%! json_format(+JsonTerm, -JsonString) is det.
%
%  Format Prolog term as JSON string.
%
%  @arg JsonTerm Prolog term representation
%  @arg JsonString JSON string output
json_format(JsonTerm, JsonString) :-
    format_json_value(JsonTerm, Codes),
    atom_codes(JsonString, Codes).

%! json_get(+Json, +Key, -Value) is semidet.
%
%  Get value from JSON object by key.
%
%  @arg Json JSON object term
%  @arg Key Key atom
%  @arg Value Retrieved value
json_get(json(Pairs), Key, Value) :-
    member(Key-Value, Pairs), !.

%! json_get(+Json, +Key, +Default, -Value) is det.
%
%  Get value from JSON object with default.
%
%  @arg Json JSON object term
%  @arg Key Key atom
%  @arg Default Default value if key not found
%  @arg Value Retrieved or default value
json_get(json(Pairs), Key, Default, Value) :-
    (   member(Key-V, Pairs)
    ->  Value = V
    ;   Value = Default
    ).

%! json_set(+Json, +Key, +Value, -NewJson) is det.
%
%  Set or update a value in JSON object.
%
%  @arg Json Original JSON object
%  @arg Key Key to set
%  @arg Value Value to set
%  @arg NewJson Updated JSON object
json_set(json(Pairs), Key, Value, json(NewPairs)) :-
    (   select(Key-_, Pairs, RestPairs)
    ->  NewPairs = [Key-Value|RestPairs]
    ;   NewPairs = [Key-Value|Pairs]
    ).

%! json_delete(+Json, +Key, -NewJson) is det.
%
%  Delete a key from JSON object.
%
%  @arg Json Original JSON object
%  @arg Key Key to delete
%  @arg NewJson Updated JSON object
json_delete(json(Pairs), Key, json(NewPairs)) :-
    (   select(Key-_, Pairs, NewPairs)
    ->  true
    ;   NewPairs = Pairs
    ).

%! json_keys(+Json, -Keys) is det.
%
%  Get all keys from JSON object.
%
%  @arg Json JSON object term
%  @arg Keys List of keys
json_keys(json(Pairs), Keys) :-
    pairs_keys(Pairs, Keys).

pairs_keys([], []).
pairs_keys([K-_|Rest], [K|Keys]) :-
    pairs_keys(Rest, Keys).

%! json_values(+Json, -Values) is det.
%
%  Get all values from JSON object.
%
%  @arg Json JSON object term
%  @arg Values List of values
json_values(json(Pairs), Values) :-
    pairs_values(Pairs, Values).

pairs_values([], []).
pairs_values([_-V|Rest], [V|Values]) :-
    pairs_values(Rest, Values).

%! json_merge(+Json1, +Json2, -Merged) is det.
%
%  Merge two JSON objects (Json2 overwrites Json1).
%
%  @arg Json1 First JSON object
%  @arg Json2 Second JSON object
%  @arg Merged Merged JSON object
json_merge(json(Pairs1), json(Pairs2), json(MergedPairs)) :-
    merge_pairs(Pairs1, Pairs2, MergedPairs).

merge_pairs([], Pairs2, Pairs2).
merge_pairs([K-V|Rest], Pairs2, Merged) :-
    (   member(K-_, Pairs2)
    ->  merge_pairs(Rest, Pairs2, Merged)
    ;   Merged = [K-V|RestMerged],
        merge_pairs(Rest, Pairs2, RestMerged)
    ).

%! json_type(+JsonValue, -Type) is det.
%
%  Determine the JSON type of a value.
%  Type is one of: object, array, string, number, boolean, null.
%
%  @arg JsonValue JSON value
%  @arg Type Type atom
json_type(json(_), object) :- !.
json_type(List, array) :- is_list(List), !.
json_type(true, boolean) :- !.
json_type(false, boolean) :- !.
json_type(null, null) :- !.
json_type(N, number) :- number(N), !.
json_type(S, string) :- atom(S), !.
json_type(S, string) :- string(S).

%! is_valid_json(+JsonString) is semidet.
%
%  Check if string is valid JSON.
%
%  @arg JsonString String to validate
is_valid_json(JsonString) :-
    catch(json_parse(JsonString, _), _, fail).

%! json_path_get(+Json, +Path, -Value) is semidet.
%
%  Get value using dot-notation path.
%  Path is a list of keys/indices.
%
%  @arg Json JSON term
%  @arg Path List of keys or indices
%  @arg Value Retrieved value
json_path_get(Json, [], Json).
json_path_get(json(Pairs), [Key|Rest], Value) :-
    member(Key-SubValue, Pairs),
    json_path_get(SubValue, Rest, Value).
json_path_get(List, [Index|Rest], Value) :-
    is_list(List),
    integer(Index),
    nth0(Index, List, SubValue),
    json_path_get(SubValue, Rest, Value).

%! json_to_pairs(+Json, -Pairs) is det.
%
%  Convert JSON object to key-value pairs.
%
%  @arg Json JSON object term
%  @arg Pairs List of Key-Value pairs
json_to_pairs(json(Pairs), Pairs).

%! pairs_to_json(+Pairs, -Json) is det.
%
%  Convert key-value pairs to JSON object.
%
%  @arg Pairs List of Key-Value pairs
%  @arg Json JSON object term
pairs_to_json(Pairs, json(Pairs)).

%% DCG for JSON parsing

json_value(Value) -->
    ws, json_value_inner(Value), ws.

json_value_inner(json(Pairs)) -->
    "{", ws, json_pairs(Pairs), ws, "}".
json_value_inner(Array) -->
    "[", ws, json_array(Array), ws, "]".
json_value_inner(String) -->
    "\"", json_string_chars(Chars), "\"",
    { atom_codes(String, Chars) }.
json_value_inner(Number) -->
    json_number(Number).
json_value_inner(true) --> "true".
json_value_inner(false) --> "false".
json_value_inner(null) --> "null".

json_pairs([]) --> [].
json_pairs([Pair|Pairs]) -->
    json_pair(Pair),
    json_pairs_rest(Pairs).

json_pairs_rest([]) --> [].
json_pairs_rest([Pair|Pairs]) -->
    ws, ",", ws,
    json_pair(Pair),
    json_pairs_rest(Pairs).

json_pair(Key-Value) -->
    "\"", json_string_chars(KeyChars), "\"",
    { atom_codes(Key, KeyChars) },
    ws, ":", ws,
    json_value(Value).

json_array([]) --> [].
json_array([Value|Values]) -->
    json_value(Value),
    json_array_rest(Values).

json_array_rest([]) --> [].
json_array_rest([Value|Values]) -->
    ws, ",", ws,
    json_value(Value),
    json_array_rest(Values).

json_string_chars([]) --> [].
json_string_chars([C|Chars]) -->
    json_char(C),
    json_string_chars(Chars).

json_char(C) --> [C], { C \= 0'", C \= 0'\\, C >= 0x20 }.
json_char(C) --> "\\", json_escape(C).

json_escape(0'\n) --> "n".
json_escape(0'\r) --> "r".
json_escape(0'\t) --> "t".
json_escape(0'\\) --> "\\".
json_escape(0'") --> "\"".
json_escape(0'/) --> "/".

json_number(N) -->
    json_number_chars(Chars),
    { Chars \= [], atom_codes(Atom, Chars), atom_number(Atom, N) }.

json_number_chars([C|Chars]) -->
    [C], { is_number_char(C) },
    json_number_chars(Chars).
json_number_chars([]) --> [].

is_number_char(C) :- C >= 0'0, C =< 0'9.
is_number_char(0'-).
is_number_char(0'+).
is_number_char(0'.).
is_number_char(0'e).
is_number_char(0'E).

ws --> [C], { is_ws(C) }, ws.
ws --> [].

is_ws(0' ).
is_ws(0'\t).
is_ws(0'\n).
is_ws(0'\r).

%% JSON formatting

format_json_value(json(Pairs), Codes) :-
    format_pairs(Pairs, PairsCodes),
    append([[0'{], PairsCodes, [0'}]], Codes).
format_json_value(List, Codes) :-
    is_list(List),
    format_array(List, ArrayCodes),
    append([[0'[|ArrayCodes], [0']]], Codes).
format_json_value(true, "true").
format_json_value(false, "false").
format_json_value(null, "null").
format_json_value(N, Codes) :-
    number(N),
    number_codes(N, Codes).
format_json_value(S, Codes) :-
    atom(S),
    atom_codes(S, SCodes),
    append([[0'"], SCodes, [0'"]], Codes).

format_pairs([], []).
format_pairs([K-V], Codes) :-
    atom_codes(K, KCodes),
    format_json_value(V, VCodes),
    append([[0'"], KCodes, [0'", 0':], VCodes], Codes).
format_pairs([K-V|Rest], Codes) :-
    Rest \= [],
    atom_codes(K, KCodes),
    format_json_value(V, VCodes),
    format_pairs(Rest, RestCodes),
    append([[0'"], KCodes, [0'", 0':], VCodes, [0',], RestCodes], Codes).

format_array([], []).
format_array([V], Codes) :-
    format_json_value(V, Codes).
format_array([V|Rest], Codes) :-
    Rest \= [],
    format_json_value(V, VCodes),
    format_array(Rest, RestCodes),
    append([VCodes, [0',], RestCodes], Codes).
