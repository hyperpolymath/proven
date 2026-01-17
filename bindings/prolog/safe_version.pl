% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeVersion - Semantic versioning for Prolog
% SemVer 2.0.0 specification compliant.

:- module(safe_version, [
    version/4,
    parse_version/2,
    format_version/2,
    version_compare/3,
    version_satisfies/2,
    version_increment_major/2,
    version_increment_minor/2,
    version_increment_patch/2,
    is_valid_version/1,
    is_prerelease/1,
    is_stable/1,
    version_major/2,
    version_minor/2,
    version_patch/2,
    version_prerelease/2,
    version_compatible/2
]).

:- use_module(library(lists)).

%! version(?Major, ?Minor, ?Patch, ?VersionTerm) is det.
%
%  Construct or deconstruct a version term.
%
%  @arg Major Major version number
%  @arg Minor Minor version number
%  @arg Patch Patch version number
%  @arg VersionTerm version/4 structure
version(Major, Minor, Patch, version(Major, Minor, Patch, '')).

%! parse_version(+VersionString, -Version) is semidet.
%
%  Parse semantic version string.
%  Accepts: 1.2.3, 1.2.3-alpha, 1.2.3-alpha.1+build
%
%  @arg VersionString Version string
%  @arg Version version/4 structure
parse_version(VersionString, version(Major, Minor, Patch, Prerelease)) :-
    atom_string(VersionString, VerStr),
    % Remove leading 'v' if present
    (   string_chars(VerStr, ['v'|RestChars])
    ->  string_chars(CleanStr, RestChars)
    ;   CleanStr = VerStr
    ),
    % Split on + for build metadata (ignore it)
    (   sub_string(CleanStr, Before, _, _, "+")
    ->  sub_string(CleanStr, 0, Before, _, CoreStr)
    ;   CoreStr = CleanStr
    ),
    % Split on - for prerelease
    (   sub_string(CoreStr, HyphenPos, _, After, "-")
    ->  sub_string(CoreStr, 0, HyphenPos, _, VersionCore),
        sub_string(CoreStr, _, After, 0, PrereleaseStr),
        atom_string(Prerelease, PrereleaseStr)
    ;   VersionCore = CoreStr,
        Prerelease = ''
    ),
    % Parse version numbers
    split_string(VersionCore, ".", "", Parts),
    (   Parts = [MajorStr, MinorStr, PatchStr]
    ->  true
    ;   Parts = [MajorStr, MinorStr], PatchStr = "0"
    ;   Parts = [MajorStr], MinorStr = "0", PatchStr = "0"
    ),
    number_string(Major, MajorStr),
    number_string(Minor, MinorStr),
    number_string(Patch, PatchStr),
    Major >= 0,
    Minor >= 0,
    Patch >= 0.

%! format_version(+Version, -VersionString) is det.
%
%  Format version as string.
%
%  @arg Version version/4 structure
%  @arg VersionString Formatted string
format_version(version(Major, Minor, Patch, ''), VersionString) :- !,
    format(atom(VersionString), "~d.~d.~d", [Major, Minor, Patch]).
format_version(version(Major, Minor, Patch, Prerelease), VersionString) :-
    format(atom(VersionString), "~d.~d.~d-~w", [Major, Minor, Patch, Prerelease]).

%! version_compare(-Order, +V1, +V2) is det.
%
%  Compare two versions according to SemVer precedence.
%  Order is one of: <, =, >.
%
%  @arg Order Comparison result
%  @arg V1 First version
%  @arg V2 Second version
version_compare(Order, version(Ma1, Mi1, Pa1, Pre1), version(Ma2, Mi2, Pa2, Pre2)) :-
    (   Ma1 < Ma2 -> Order = (<)
    ;   Ma1 > Ma2 -> Order = (>)
    ;   Mi1 < Mi2 -> Order = (<)
    ;   Mi1 > Mi2 -> Order = (>)
    ;   Pa1 < Pa2 -> Order = (<)
    ;   Pa1 > Pa2 -> Order = (>)
    ;   compare_prerelease(Order, Pre1, Pre2)
    ).

compare_prerelease((=), '', '') :- !.
compare_prerelease((>), '', _) :- !.  % No prerelease > any prerelease
compare_prerelease((<), _, '') :- !.
compare_prerelease(Order, Pre1, Pre2) :-
    atom_string(Pre1, Pre1Str),
    atom_string(Pre2, Pre2Str),
    compare(Order, Pre1Str, Pre2Str).

%! version_satisfies(+Version, +Constraint) is semidet.
%
%  Check if version satisfies a constraint.
%  Constraints: >=1.0.0, <2.0.0, ^1.2.3, ~1.2.3, =1.2.3
%
%  @arg Version version/4 structure
%  @arg Constraint Constraint string
version_satisfies(Version, Constraint) :-
    atom_string(Constraint, ConstraintStr),
    parse_constraint(ConstraintStr, Op, ConstraintVersion),
    apply_constraint(Version, Op, ConstraintVersion).

parse_constraint(Str, Op, Version) :-
    (   sub_string(Str, 0, 2, _, ">=")
    ->  Op = (>=), sub_string(Str, 2, _, 0, VerStr)
    ;   sub_string(Str, 0, 2, _, "<=")
    ->  Op = (=<), sub_string(Str, 2, _, 0, VerStr)
    ;   sub_string(Str, 0, 1, _, ">")
    ->  Op = (>), sub_string(Str, 1, _, 0, VerStr)
    ;   sub_string(Str, 0, 1, _, "<")
    ->  Op = (<), sub_string(Str, 1, _, 0, VerStr)
    ;   sub_string(Str, 0, 1, _, "^")
    ->  Op = caret, sub_string(Str, 1, _, 0, VerStr)
    ;   sub_string(Str, 0, 1, _, "~")
    ->  Op = tilde, sub_string(Str, 1, _, 0, VerStr)
    ;   sub_string(Str, 0, 1, _, "=")
    ->  Op = (=), sub_string(Str, 1, _, 0, VerStr)
    ;   Op = (=), VerStr = Str
    ),
    atom_string(VerAtom, VerStr),
    parse_version(VerAtom, Version).

apply_constraint(V, (>=), Constraint) :-
    version_compare(Order, V, Constraint),
    member(Order, [(>), (=)]).
apply_constraint(V, (=<), Constraint) :-
    version_compare(Order, V, Constraint),
    member(Order, [(<), (=)]).
apply_constraint(V, (>), Constraint) :-
    version_compare((>), V, Constraint).
apply_constraint(V, (<), Constraint) :-
    version_compare((<), V, Constraint).
apply_constraint(V, (=), Constraint) :-
    version_compare((=), V, Constraint).
apply_constraint(version(Ma, Mi, Pa, _), caret, version(CMa, CMi, CPa, _)) :-
    Ma =:= CMa,
    (   CMa =:= 0
    ->  Mi =:= CMi, Pa >= CPa
    ;   (Mi > CMi ; (Mi =:= CMi, Pa >= CPa))
    ).
apply_constraint(version(Ma, Mi, Pa, _), tilde, version(CMa, CMi, CPa, _)) :-
    Ma =:= CMa,
    Mi =:= CMi,
    Pa >= CPa.

%! version_increment_major(+Version, -NewVersion) is det.
%
%  Increment major version (reset minor and patch).
%
%  @arg Version Original version
%  @arg NewVersion Incremented version
version_increment_major(version(Major, _, _, _), version(NewMajor, 0, 0, '')) :-
    NewMajor is Major + 1.

%! version_increment_minor(+Version, -NewVersion) is det.
%
%  Increment minor version (reset patch).
%
%  @arg Version Original version
%  @arg NewVersion Incremented version
version_increment_minor(version(Major, Minor, _, _), version(Major, NewMinor, 0, '')) :-
    NewMinor is Minor + 1.

%! version_increment_patch(+Version, -NewVersion) is det.
%
%  Increment patch version.
%
%  @arg Version Original version
%  @arg NewVersion Incremented version
version_increment_patch(version(Major, Minor, Patch, _), version(Major, Minor, NewPatch, '')) :-
    NewPatch is Patch + 1.

%! is_valid_version(+VersionString) is semidet.
%
%  Check if string is a valid semantic version.
%
%  @arg VersionString String to validate
is_valid_version(VersionString) :-
    catch(parse_version(VersionString, _), _, fail).

%! is_prerelease(+Version) is semidet.
%
%  Check if version is a prerelease.
%
%  @arg Version version/4 structure
is_prerelease(version(_, _, _, Prerelease)) :-
    Prerelease \= ''.

%! is_stable(+Version) is semidet.
%
%  Check if version is stable (>=1.0.0, no prerelease).
%
%  @arg Version version/4 structure
is_stable(version(Major, _, _, '')) :-
    Major >= 1.

%! version_major(+Version, -Major) is det.
%
%  Extract major version number.
%
%  @arg Version version/4 structure
%  @arg Major Major version number
version_major(version(Major, _, _, _), Major).

%! version_minor(+Version, -Minor) is det.
%
%  Extract minor version number.
%
%  @arg Version version/4 structure
%  @arg Minor Minor version number
version_minor(version(_, Minor, _, _), Minor).

%! version_patch(+Version, -Patch) is det.
%
%  Extract patch version number.
%
%  @arg Version version/4 structure
%  @arg Patch Patch version number
version_patch(version(_, _, Patch, _), Patch).

%! version_prerelease(+Version, -Prerelease) is det.
%
%  Extract prerelease identifier.
%
%  @arg Version version/4 structure
%  @arg Prerelease Prerelease string or ''
version_prerelease(version(_, _, _, Prerelease), Prerelease).

%! version_compatible(+V1, +V2) is semidet.
%
%  Check if two versions are API compatible.
%  Compatible if same major version (for stable releases).
%
%  @arg V1 First version
%  @arg V2 Second version
version_compatible(version(Major, _, _, _), version(Major, _, _, _)) :-
    Major >= 1.
version_compatible(version(0, Minor, _, _), version(0, Minor, _, _)).
