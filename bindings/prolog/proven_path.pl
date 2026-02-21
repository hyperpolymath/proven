%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafePath - Directory traversal prevention for Prolog

:- module(proven_path, [
    has_traversal/1,
    sanitize_filename/2,
    safe_path_join/3
]).

:- use_module(library(lists)).

%% has_traversal(+Path)
%% Succeeds if Path contains traversal patterns
has_traversal(Path) :-
    atom_string(Path, PathStr),
    string_lower(PathStr, LowerPath),
    (   sub_string(PathStr, _, _, _, "..")
    ;   sub_string(PathStr, _, _, _, "./")
    ;   sub_string(LowerPath, _, _, _, "%2e%2e")
    ;   sub_string(LowerPath, _, _, _, "%00")
    ), !.

%% sanitize_filename(+Input, -Output)
%% Replace dangerous filename characters with underscores
sanitize_filename(Input, Output) :-
    atom_codes(Input, Codes),
    maplist(sanitize_char, Codes, SanitizedCodes),
    atom_codes(Output, SanitizedCodes).

sanitize_char(0'/, 0'_) :- !.
sanitize_char(0'\\, 0'_) :- !.
sanitize_char(0':, 0'_) :- !.
sanitize_char(0'*, 0'_) :- !.
sanitize_char(0'?, 0'_) :- !.
sanitize_char(0'", 0'_) :- !.
sanitize_char(0'<, 0'_) :- !.
sanitize_char(0'>, 0'_) :- !.
sanitize_char(0'|, 0'_) :- !.
sanitize_char(C, C).

%% safe_path_join(+Base, +Filename, -Result)
%% Result is ok(Path) or error(traversal_detected)
safe_path_join(_, Filename, error(traversal_detected)) :-
    has_traversal(Filename), !.
safe_path_join(Base, Filename, ok(Path)) :-
    sanitize_filename(Filename, SafeName),
    atom_string(Base, BaseStr),
    atom_string(SafeName, SafeNameStr),
    (   string_chars(BaseStr, BaseChars),
        last(BaseChars, '/')
    ->  string_concat(BaseStr, SafeNameStr, PathStr)
    ;   string_concat(BaseStr, "/", BaseWithSlash),
        string_concat(BaseWithSlash, SafeNameStr, PathStr)
    ),
    atom_string(Path, PathStr).
