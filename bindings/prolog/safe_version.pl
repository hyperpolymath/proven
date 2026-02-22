%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeVersion - FFI bindings to libproven version operations.
%% All semantic version operations are performed in verified Idris 2 code
%% via libproven.

:- module(safe_version, [
    parse_version/2,
    version_compare/3,
    is_valid_version/1
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven version functions.
:- foreign(proven_version_parse_ffi, c,
           proven_version_parse(+string, +integer,
                                [-integer],
                                [-integer], [-integer], [-integer],
                                [-string], [-integer])).
:- foreign(proven_version_compare_ffi, c,
           proven_version_compare(+integer, +integer, +integer, +string, +integer,
                                  +integer, +integer, +integer, +string, +integer,
                                  [-integer])).

%! parse_version(+VersionString, -Result) is det.
%
%  Parse semantic version string via libproven (e.g., "1.2.3-alpha").
%  Result is ok(version(Major, Minor, Patch, Prerelease)) or
%  error(parse_failure).
parse_version(VersionString, Result) :-
    atom_string(VersionString, Str),
    atom_length(VersionString, Len),
    proven_version_parse_ffi(Str, Len, Status,
                             Major, Minor, Patch,
                             PrereleaseStr, PrereleaseLen),
    (   Status =:= 0
    ->  (   PrereleaseLen > 0
        ->  atom_string(Prerelease, PrereleaseStr)
        ;   Prerelease = ''
        ),
        Result = ok(version(Major, Minor, Patch, Prerelease))
    ;   Result = error(parse_failure)
    ).

%! version_compare(+V1, +V2, -Order) is det.
%
%  Compare two semantic versions via libproven.
%  Order is -1 (V1 < V2), 0 (equal), or 1 (V1 > V2).
version_compare(version(Ma1, Mi1, Pa1, Pre1),
                version(Ma2, Mi2, Pa2, Pre2),
                Order) :-
    (   Pre1 = '' -> Pre1Str = "", Pre1Len = 0
    ;   atom_string(Pre1, Pre1Str), atom_length(Pre1, Pre1Len)
    ),
    (   Pre2 = '' -> Pre2Str = "", Pre2Len = 0
    ;   atom_string(Pre2, Pre2Str), atom_length(Pre2, Pre2Len)
    ),
    proven_version_compare_ffi(Ma1, Mi1, Pa1, Pre1Str, Pre1Len,
                               Ma2, Mi2, Pa2, Pre2Str, Pre2Len,
                               Order).

%! is_valid_version(+VersionString) is semidet.
%
%  Check if string is a valid semantic version via libproven.
is_valid_version(VersionString) :-
    parse_version(VersionString, ok(_)).
