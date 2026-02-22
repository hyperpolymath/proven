%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafePath - FFI bindings to libproven path operations.
%% All traversal detection and sanitization is performed in verified
%% Idris 2 code via libproven.

:- module(proven_path, [
    has_traversal/1,
    sanitize_filename/2,
    safe_path_join/3
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations
:- foreign(proven_path_has_traversal_ffi, c,
           proven_path_has_traversal(+string, +integer, [-integer], [-integer])).
:- foreign(proven_path_sanitize_filename_ffi, c,
           proven_path_sanitize_filename(+string, +integer, [-integer], [-string])).

%! has_traversal(+Path) is semidet.
%
%  Succeeds if Path contains directory traversal sequences.
%  Detection performed by libproven.
has_traversal(Path) :-
    atom_string(Path, PathStr),
    atom_length(Path, Len),
    proven_path_has_traversal_ffi(PathStr, Len, Status, Value),
    Status =:= 0,
    Value =:= 1.

%! sanitize_filename(+Input, -Output) is det.
%
%  Sanitize a filename by removing dangerous characters via libproven.
sanitize_filename(Input, Output) :-
    atom_string(Input, InputStr),
    atom_length(Input, Len),
    proven_path_sanitize_filename_ffi(InputStr, Len, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Output, ResultStr)
    ;   Output = Input
    ).

%! safe_path_join(+Base, +Filename, -Result) is det.
%
%  Safely join base path and filename via libproven.
%  Result is ok(Path) or error(traversal_detected).
safe_path_join(Base, Filename, Result) :-
    atom_string(Filename, FilenameStr),
    atom_length(Filename, FLen),
    proven_path_has_traversal_ffi(FilenameStr, FLen, TStatus, TValue),
    (   TStatus =:= 0, TValue =:= 1
    ->  Result = error(traversal_detected)
    ;   sanitize_filename(Filename, SafeName),
        atom_string(Base, BaseStr),
        atom_string(SafeName, SafeNameStr),
        (   string_concat(BaseStr, "/", _)
        ->  string_concat(BaseStr, SafeNameStr, PathStr)
        ;   string_concat(BaseStr, "/", BaseSlash),
            string_concat(BaseSlash, SafeNameStr, PathStr)
        ),
        atom_string(Path, PathStr),
        Result = ok(Path)
    ).
