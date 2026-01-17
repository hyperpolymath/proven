% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafePath - Directory traversal prevention for Prolog
% Provides safe path manipulation and validation.

:- module(safe_path, [
    has_traversal/1,
    sanitize_filename/2,
    safe_path_join/3,
    normalize_path/2,
    path_extension/2,
    path_basename/2,
    path_dirname/2,
    is_absolute_path/1,
    is_relative_path/1,
    split_path/2,
    join_path/2
]).

:- use_module(library(lists)).

%! has_traversal(+Path) is semidet.
%
%  Succeeds if Path contains directory traversal patterns.
%  Detects: .., ./, encoded variants.
%
%  @arg Path Path string to check
has_traversal(Path) :-
    atom_string(Path, PathStr),
    string_lower(PathStr, LowerPath),
    (   sub_string(PathStr, _, _, _, "..")
    ;   sub_string(PathStr, _, _, _, "./")
    ;   sub_string(LowerPath, _, _, _, "%2e%2e")
    ;   sub_string(LowerPath, _, _, _, "%00")
    ;   sub_string(LowerPath, _, _, _, "%2f")
    ), !.

%! sanitize_filename(+Input, -Output) is det.
%
%  Replace dangerous filename characters with underscores.
%  Removes: / \ : * ? " < > |
%
%  @arg Input Raw filename
%  @arg Output Sanitized filename
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
sanitize_char(0'\0, 0'_) :- !.
sanitize_char(C, C).

%! safe_path_join(+Base, +Filename, -Result) is det.
%
%  Safely join base path with filename.
%  Result is ok(Path) or error(traversal_detected).
%
%  @arg Base Base directory path
%  @arg Filename Filename to append
%  @arg Result ok(Path) or error(traversal_detected)
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

%! normalize_path(+Input, -Output) is det.
%
%  Normalize a path by removing redundant separators and dots.
%
%  @arg Input Raw path
%  @arg Output Normalized path
normalize_path(Input, Output) :-
    atom_string(Input, PathStr),
    split_string(PathStr, "/", "", Parts),
    normalize_parts(Parts, [], NormalizedParts),
    atomics_to_string(NormalizedParts, "/", OutputStr),
    atom_string(Output, OutputStr).

normalize_parts([], Acc, Normalized) :-
    reverse(Acc, Normalized).
normalize_parts([""|Rest], Acc, Normalized) :- !,
    (   Acc = []
    ->  normalize_parts(Rest, [""], Normalized)
    ;   normalize_parts(Rest, Acc, Normalized)
    ).
normalize_parts(["."|Rest], Acc, Normalized) :- !,
    normalize_parts(Rest, Acc, Normalized).
normalize_parts([".."|Rest], [_|AccRest], Normalized) :- !,
    normalize_parts(Rest, AccRest, Normalized).
normalize_parts([".."|Rest], [], Normalized) :- !,
    normalize_parts(Rest, [], Normalized).
normalize_parts([Part|Rest], Acc, Normalized) :-
    normalize_parts(Rest, [Part|Acc], Normalized).

%! path_extension(+Path, -Extension) is det.
%
%  Extract file extension from path.
%  Extension includes the dot (e.g., ".txt").
%
%  @arg Path File path
%  @arg Extension File extension or empty atom
path_extension(Path, Extension) :-
    atom_string(Path, PathStr),
    (   sub_string(PathStr, Before, _, After, "."),
        \+ sub_string(PathStr, _, _, Less, "."),
        Less < After,
        ExtLen is After,
        ExtLen > 0,
        sub_string(PathStr, _, ExtLen1, 0, ExtStr),
        ExtLen1 is ExtLen + 1,
        sub_string(PathStr, _, ExtLen1, 0, ExtWithDot)
    ->  atom_string(Extension, ExtWithDot)
    ;   Extension = ''
    ).

%! path_basename(+Path, -Basename) is det.
%
%  Extract basename (filename with extension) from path.
%
%  @arg Path File path
%  @arg Basename Filename portion
path_basename(Path, Basename) :-
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", Parts),
    last(Parts, BasenameStr),
    atom_string(Basename, BasenameStr).

%! path_dirname(+Path, -Dirname) is det.
%
%  Extract directory portion from path.
%
%  @arg Path File path
%  @arg Dirname Directory portion
path_dirname(Path, Dirname) :-
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", Parts),
    (   Parts = [Single]
    ->  Dirname = '.'
    ;   append(DirParts, [_], Parts),
        atomics_to_string(DirParts, "/", DirnameStr),
        (   DirnameStr = ""
        ->  Dirname = '/'
        ;   atom_string(Dirname, DirnameStr)
        )
    ).

%! is_absolute_path(+Path) is semidet.
%
%  Check if path is absolute (starts with /).
%
%  @arg Path Path to check
is_absolute_path(Path) :-
    atom_string(Path, PathStr),
    string_chars(PathStr, ['/'] ++ _).

%! is_relative_path(+Path) is semidet.
%
%  Check if path is relative (does not start with /).
%
%  @arg Path Path to check
is_relative_path(Path) :-
    \+ is_absolute_path(Path).

%! split_path(+Path, -Parts) is det.
%
%  Split path into list of components.
%
%  @arg Path Path string
%  @arg Parts List of path components
split_path(Path, Parts) :-
    atom_string(Path, PathStr),
    split_string(PathStr, "/", "", StringParts),
    include(\=(""), StringParts, NonEmptyParts),
    maplist(atom_string, Parts, NonEmptyParts).

%! join_path(+Parts, -Path) is det.
%
%  Join path components into a path string.
%
%  @arg Parts List of path components
%  @arg Path Joined path
join_path(Parts, Path) :-
    maplist(atom_string, Parts, StringParts),
    atomics_to_string(StringParts, "/", PathStr),
    atom_string(Path, PathStr).
