%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafePath - Directory traversal prevention for Erlang

-module(proven_path).

-export([
    has_traversal/1,
    sanitize_filename/1,
    safe_path_join/2
]).

%% @doc Check if path contains traversal patterns.
-spec has_traversal(string()) -> boolean().
has_traversal(Path) ->
    LowerPath = string:lowercase(Path),
    contains(Path, "..") orelse
    contains(Path, "./") orelse
    contains(LowerPath, "%2e%2e") orelse
    contains(LowerPath, "%00").

%% @doc Check if string contains substring.
-spec contains(string(), string()) -> boolean().
contains(String, SubString) ->
    case string:find(String, SubString) of
        nomatch -> false;
        _ -> true
    end.

%% @doc Sanitize filename by replacing dangerous characters.
-spec sanitize_filename(string()) -> string().
sanitize_filename(Input) ->
    lists:map(fun sanitize_char/1, Input).

sanitize_char($/) -> $_;
sanitize_char($\\) -> $_;
sanitize_char($:) -> $_;
sanitize_char($*) -> $_;
sanitize_char($?) -> $_;
sanitize_char($") -> $_;
sanitize_char($<) -> $_;
sanitize_char($>) -> $_;
sanitize_char($|) -> $_;
sanitize_char(C) -> C.

%% @doc Safely join base path with filename.
%% Returns {ok, Path} or {error, Reason}.
-spec safe_path_join(string(), string()) -> {ok, string()} | {error, traversal_detected}.
safe_path_join(Base, Filename) ->
    case has_traversal(Filename) of
        true ->
            {error, traversal_detected};
        false ->
            SafeName = sanitize_filename(Filename),
            Joined = case lists:last(Base) of
                $/ -> Base ++ SafeName;
                _ -> Base ++ "/" ++ SafeName
            end,
            {ok, Joined}
    end.
