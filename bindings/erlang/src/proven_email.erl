%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeEmail - Email validation for Erlang

-module(proven_email).

-export([
    is_valid/1,
    parse/1,
    is_disposable/1,
    normalize/1
]).

%% Disposable email domains
-define(DISPOSABLE_DOMAINS, [
    "tempmail.com", "throwaway.com", "mailinator.com", "guerrillamail.com",
    "10minutemail.com", "trashmail.com", "fakeinbox.com", "tempinbox.com"
]).

%% @doc Check if email address is valid.
-spec is_valid(string()) -> boolean().
is_valid(Email) ->
    case parse(Email) of
        {ok, _LocalPart, _Domain} -> true;
        {error, _} -> false
    end.

%% @doc Parse email into local part and domain.
%% Returns {ok, LocalPart, Domain} or {error, Reason}.
-spec parse(string()) -> {ok, string(), string()} | {error, atom()}.
parse(Email) ->
    case string:split(Email, "@") of
        [LocalPart, Domain] when LocalPart =/= "", Domain =/= "" ->
            case valid_local_part(LocalPart) of
                true ->
                    case valid_domain(Domain) of
                        true -> {ok, LocalPart, Domain};
                        false -> {error, invalid_domain}
                    end;
                false ->
                    {error, invalid_local_part}
            end;
        [_] ->
            {error, no_at_symbol};
        _ ->
            {error, multiple_at_symbols}
    end.

%% @doc Validate local part of email.
-spec valid_local_part(string()) -> boolean().
valid_local_part(Local) when length(Local) == 0; length(Local) > 64 ->
    false;
valid_local_part(Local) ->
    [First | _] = Local,
    Last = lists:last(Local),
    First =/= $. andalso Last =/= $. andalso
    not contains(Local, "..") andalso
    lists:all(fun is_local_char/1, Local).

is_local_char(C) when C >= $a, C =< $z -> true;
is_local_char(C) when C >= $A, C =< $Z -> true;
is_local_char(C) when C >= $0, C =< $9 -> true;
is_local_char($.) -> true;
is_local_char($_) -> true;
is_local_char($-) -> true;
is_local_char($+) -> true;
is_local_char($=) -> true;
is_local_char(_) -> false.

%% @doc Validate domain of email.
-spec valid_domain(string()) -> boolean().
valid_domain(Domain) when length(Domain) == 0; length(Domain) > 253 ->
    false;
valid_domain(Domain) ->
    [First | _] = Domain,
    Last = lists:last(Domain),
    has_dot(Domain) andalso
    First =/= $. andalso Last =/= $. andalso
    First =/= $- andalso Last =/= $- andalso
    not contains(Domain, "..") andalso
    not contains(Domain, ".-") andalso
    not contains(Domain, "-.") andalso
    lists:all(fun is_domain_char/1, Domain).

has_dot(String) ->
    lists:member($., String).

is_domain_char(C) when C >= $a, C =< $z -> true;
is_domain_char(C) when C >= $A, C =< $Z -> true;
is_domain_char(C) when C >= $0, C =< $9 -> true;
is_domain_char($.) -> true;
is_domain_char($-) -> true;
is_domain_char(_) -> false.

%% @doc Check if string contains substring.
-spec contains(string(), string()) -> boolean().
contains(String, SubString) ->
    case string:find(String, SubString) of
        nomatch -> false;
        _ -> true
    end.

%% @doc Check if email is from a disposable domain.
-spec is_disposable(string()) -> boolean().
is_disposable(Email) ->
    case parse(Email) of
        {ok, _LocalPart, Domain} ->
            LowerDomain = string:lowercase(Domain),
            lists:member(LowerDomain, ?DISPOSABLE_DOMAINS);
        {error, _} ->
            false
    end.

%% @doc Normalize email address (lowercase domain).
-spec normalize(string()) -> {ok, string()} | {error, atom()}.
normalize(Email) ->
    case parse(Email) of
        {ok, LocalPart, Domain} ->
            {ok, LocalPart ++ "@" ++ string:lowercase(Domain)};
        {error, Reason} ->
            {error, Reason}
    end.
