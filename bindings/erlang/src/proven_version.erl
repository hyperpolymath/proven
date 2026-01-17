%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeVersion - Semantic versioning for Erlang

-module(proven_version).

-export([
    parse/1,
    format/1,
    compare/2,
    is_valid/1,
    major/1,
    minor/1,
    patch/1,
    prerelease/1,
    build_metadata/1,
    increment_major/1,
    increment_minor/1,
    increment_patch/1,
    satisfies/2,
    is_compatible/2,
    is_stable/1
]).

-export_type([semver/0]).

%% Semantic version record
-record(semver, {
    major :: non_neg_integer(),
    minor :: non_neg_integer(),
    patch :: non_neg_integer(),
    prerelease :: [string()] | undefined,
    build :: string() | undefined
}).

-type semver() :: #semver{}.

%% @doc Parse a semantic version string.
-spec parse(string() | binary()) -> {ok, semver()} | {error, invalid_format}.
parse(Version) when is_binary(Version) ->
    parse(binary_to_list(Version));
parse(Version) when is_list(Version) ->
    %% Remove leading 'v' or 'V' if present
    Trimmed = case Version of
        [$v | Rest] -> Rest;
        [$V | Rest] -> Rest;
        _ -> Version
    end,
    %% Split off build metadata
    {MainPart, Build} = case string:split(Trimmed, "+") of
        [M, B] -> {M, B};
        [M] -> {M, undefined}
    end,
    %% Split off prerelease
    {VersionPart, Prerelease} = case string:split(MainPart, "-") of
        [V, P] -> {V, parse_prerelease(P)};
        [V] -> {V, undefined}
    end,
    %% Parse major.minor.patch
    case string:tokens(VersionPart, ".") of
        [MajorStr, MinorStr, PatchStr] ->
            try
                Major = list_to_integer(MajorStr),
                Minor = list_to_integer(MinorStr),
                Patch = list_to_integer(PatchStr),
                case Major >= 0 andalso Minor >= 0 andalso Patch >= 0 of
                    true ->
                        {ok, #semver{
                            major = Major,
                            minor = Minor,
                            patch = Patch,
                            prerelease = Prerelease,
                            build = Build
                        }};
                    false ->
                        {error, invalid_format}
                end
            catch
                _:_ -> {error, invalid_format}
            end;
        [MajorStr, MinorStr] ->
            try
                Major = list_to_integer(MajorStr),
                Minor = list_to_integer(MinorStr),
                {ok, #semver{major = Major, minor = Minor, patch = 0,
                             prerelease = Prerelease, build = Build}}
            catch
                _:_ -> {error, invalid_format}
            end;
        [MajorStr] ->
            try
                Major = list_to_integer(MajorStr),
                {ok, #semver{major = Major, minor = 0, patch = 0,
                             prerelease = Prerelease, build = Build}}
            catch
                _:_ -> {error, invalid_format}
            end;
        _ ->
            {error, invalid_format}
    end.

parse_prerelease(Prerelease) ->
    string:tokens(Prerelease, ".").

%% @doc Format a semantic version as a string.
-spec format(semver()) -> string().
format(#semver{major = Major, minor = Minor, patch = Patch,
               prerelease = Prerelease, build = Build}) ->
    Base = io_lib:format("~B.~B.~B", [Major, Minor, Patch]),
    WithPre = case Prerelease of
        undefined -> Base;
        [] -> Base;
        Parts -> Base ++ "-" ++ string:join(Parts, ".")
    end,
    case Build of
        undefined -> lists:flatten(WithPre);
        _ -> lists:flatten(WithPre ++ "+" ++ Build)
    end.

%% @doc Compare two semantic versions.
%% Returns lt, eq, or gt.
-spec compare(semver(), semver()) -> lt | eq | gt.
compare(#semver{major = M1}, #semver{major = M2}) when M1 < M2 -> lt;
compare(#semver{major = M1}, #semver{major = M2}) when M1 > M2 -> gt;
compare(#semver{minor = M1}, #semver{minor = M2}) when M1 < M2 -> lt;
compare(#semver{minor = M1}, #semver{minor = M2}) when M1 > M2 -> gt;
compare(#semver{patch = P1}, #semver{patch = P2}) when P1 < P2 -> lt;
compare(#semver{patch = P1}, #semver{patch = P2}) when P1 > P2 -> gt;
compare(#semver{prerelease = undefined}, #semver{prerelease = undefined}) -> eq;
compare(#semver{prerelease = undefined}, #semver{prerelease = _}) -> gt;
compare(#semver{prerelease = _}, #semver{prerelease = undefined}) -> lt;
compare(#semver{prerelease = Pre1}, #semver{prerelease = Pre2}) ->
    compare_prerelease(Pre1, Pre2).

compare_prerelease([], []) -> eq;
compare_prerelease([], _) -> lt;
compare_prerelease(_, []) -> gt;
compare_prerelease([H1 | T1], [H2 | T2]) ->
    case compare_prerelease_part(H1, H2) of
        eq -> compare_prerelease(T1, T2);
        Result -> Result
    end.

compare_prerelease_part(P1, P2) ->
    %% Numeric parts have lower precedence than string parts
    case {catch list_to_integer(P1), catch list_to_integer(P2)} of
        {N1, N2} when is_integer(N1), is_integer(N2) ->
            if N1 < N2 -> lt;
               N1 > N2 -> gt;
               true -> eq
            end;
        {N1, _} when is_integer(N1) -> lt;
        {_, N2} when is_integer(N2) -> gt;
        _ ->
            if P1 < P2 -> lt;
               P1 > P2 -> gt;
               true -> eq
            end
    end.

%% @doc Check if a version string is valid.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Version) ->
    case parse(Version) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @doc Get the major version number.
-spec major(semver()) -> non_neg_integer().
major(#semver{major = Major}) -> Major.

%% @doc Get the minor version number.
-spec minor(semver()) -> non_neg_integer().
minor(#semver{minor = Minor}) -> Minor.

%% @doc Get the patch version number.
-spec patch(semver()) -> non_neg_integer().
patch(#semver{patch = Patch}) -> Patch.

%% @doc Get the prerelease identifiers.
-spec prerelease(semver()) -> [string()] | undefined.
prerelease(#semver{prerelease = Prerelease}) -> Prerelease.

%% @doc Get the build metadata.
-spec build_metadata(semver()) -> string() | undefined.
build_metadata(#semver{build = Build}) -> Build.

%% @doc Increment the major version.
-spec increment_major(semver()) -> semver().
increment_major(V = #semver{major = Major}) ->
    V#semver{major = Major + 1, minor = 0, patch = 0,
             prerelease = undefined, build = undefined}.

%% @doc Increment the minor version.
-spec increment_minor(semver()) -> semver().
increment_minor(V = #semver{minor = Minor}) ->
    V#semver{minor = Minor + 1, patch = 0,
             prerelease = undefined, build = undefined}.

%% @doc Increment the patch version.
-spec increment_patch(semver()) -> semver().
increment_patch(V = #semver{patch = Patch}) ->
    V#semver{patch = Patch + 1, prerelease = undefined, build = undefined}.

%% @doc Check if a version satisfies a version constraint.
%% Supports: =, >=, >, <=, <, ^, ~
-spec satisfies(semver(), string()) -> boolean().
satisfies(Version, Constraint) ->
    case parse_constraint(Constraint) of
        {ok, Op, RequiredVersion} ->
            check_constraint(Version, Op, RequiredVersion);
        {error, _} ->
            false
    end.

parse_constraint(Constraint) ->
    Trimmed = string:trim(Constraint),
    {Op, VersionStr} = case Trimmed of
        [$^  | Rest] -> {caret, Rest};
        [$~  | Rest] -> {tilde, Rest};
        [$>, $= | Rest] -> {gte, string:trim(Rest)};
        [$<, $= | Rest] -> {lte, string:trim(Rest)};
        [$> | Rest] -> {gt, string:trim(Rest)};
        [$< | Rest] -> {lt, string:trim(Rest)};
        [$= | Rest] -> {eq, string:trim(Rest)};
        _ -> {eq, Trimmed}
    end,
    case parse(VersionStr) of
        {ok, V} -> {ok, Op, V};
        Error -> Error
    end.

check_constraint(Version, eq, Required) ->
    compare(Version, Required) == eq;
check_constraint(Version, gt, Required) ->
    compare(Version, Required) == gt;
check_constraint(Version, gte, Required) ->
    compare(Version, Required) /= lt;
check_constraint(Version, lt, Required) ->
    compare(Version, Required) == lt;
check_constraint(Version, lte, Required) ->
    compare(Version, Required) /= gt;
check_constraint(Version, caret, Required) ->
    %% ^x.y.z allows changes that do not modify the leftmost non-zero digit
    case compare(Version, Required) of
        lt -> false;
        _ ->
            case Required#semver.major of
                0 ->
                    Version#semver.major == 0 andalso
                    Version#semver.minor == Required#semver.minor;
                M ->
                    Version#semver.major == M
            end
    end;
check_constraint(Version, tilde, Required) ->
    %% ~x.y.z allows patch-level changes
    compare(Version, Required) /= lt andalso
    Version#semver.major == Required#semver.major andalso
    Version#semver.minor == Required#semver.minor.

%% @doc Check if two versions are compatible (same major version).
-spec is_compatible(semver(), semver()) -> boolean().
is_compatible(#semver{major = M}, #semver{major = M}) -> true;
is_compatible(_, _) -> false.

%% @doc Check if a version is stable (no prerelease, major > 0).
-spec is_stable(semver()) -> boolean().
is_stable(#semver{major = Major, prerelease = Prerelease}) ->
    Major > 0 andalso Prerelease == undefined.
