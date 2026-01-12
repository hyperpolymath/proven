%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeNetwork - Network validation for Erlang

-module(proven_network).

-export([
    parse_ipv4/1,
    format_ipv4/1,
    is_loopback/1,
    is_private/1,
    is_reserved/1,
    is_public/1,
    classify/1,
    is_valid_port/1,
    is_privileged_port/1
]).

%% IP classification types
-type ip_class() :: invalid | loopback | private | reserved | public.
-type ipv4() :: {ipv4, integer(), integer(), integer(), integer()} | invalid.

%% @doc Parse IPv4 address string.
-spec parse_ipv4(string()) -> ipv4().
parse_ipv4(IpString) ->
    case string:tokens(IpString, ".") of
        [A, B, C, D] ->
            try
                {Oa, _} = string:to_integer(A),
                {Ob, _} = string:to_integer(B),
                {Oc, _} = string:to_integer(C),
                {Od, _} = string:to_integer(D),
                case valid_octets([Oa, Ob, Oc, Od]) of
                    true -> {ipv4, Oa, Ob, Oc, Od};
                    false -> invalid
                end
            catch
                _:_ -> invalid
            end;
        _ ->
            invalid
    end.

valid_octets(Octets) ->
    lists:all(fun(O) -> O >= 0 andalso O =< 255 end, Octets).

%% @doc Format IPv4 address as dotted-decimal string.
-spec format_ipv4(ipv4()) -> string() | error.
format_ipv4({ipv4, A, B, C, D}) ->
    io_lib:format("~B.~B.~B.~B", [A, B, C, D]);
format_ipv4(invalid) ->
    error.

%% @doc Check if IPv4 is loopback (127.x.x.x).
-spec is_loopback(ipv4()) -> boolean().
is_loopback({ipv4, 127, _, _, _}) -> true;
is_loopback(_) -> false.

%% @doc Check if IPv4 is private address.
-spec is_private(ipv4()) -> boolean().
is_private({ipv4, 10, _, _, _}) -> true;
is_private({ipv4, 172, B, _, _}) when B >= 16, B =< 31 -> true;
is_private({ipv4, 192, 168, _, _}) -> true;
is_private(_) -> false.

%% @doc Check if IPv4 is reserved address.
-spec is_reserved(ipv4()) -> boolean().
is_reserved({ipv4, 0, _, _, _}) -> true;
is_reserved({ipv4, 100, B, _, _}) when B >= 64, B =< 127 -> true;
is_reserved({ipv4, 169, 254, _, _}) -> true;
is_reserved({ipv4, 192, 0, 0, _}) -> true;
is_reserved({ipv4, 192, 0, 2, _}) -> true;
is_reserved({ipv4, 198, 51, 100, _}) -> true;
is_reserved({ipv4, 203, 0, 113, _}) -> true;
is_reserved({ipv4, A, _, _, _}) when A >= 224, A =< 239 -> true;
is_reserved({ipv4, A, _, _, _}) when A >= 240 -> true;
is_reserved({ipv4, 255, 255, 255, 255}) -> true;
is_reserved(_) -> false.

%% @doc Check if IPv4 is public address.
-spec is_public(ipv4()) -> boolean().
is_public(invalid) -> false;
is_public(Ip) ->
    not is_loopback(Ip) andalso
    not is_private(Ip) andalso
    not is_reserved(Ip).

%% @doc Classify IPv4 address.
-spec classify(ipv4()) -> ip_class().
classify(invalid) -> invalid;
classify(Ip) ->
    case is_loopback(Ip) of
        true -> loopback;
        false ->
            case is_private(Ip) of
                true -> private;
                false ->
                    case is_reserved(Ip) of
                        true -> reserved;
                        false -> public
                    end
            end
    end.

%% @doc Check if port number is valid (1-65535).
-spec is_valid_port(integer()) -> boolean().
is_valid_port(Port) when is_integer(Port), Port >= 1, Port =< 65535 -> true;
is_valid_port(_) -> false.

%% @doc Check if port is privileged (1-1023).
-spec is_privileged_port(integer()) -> boolean().
is_privileged_port(Port) when is_integer(Port), Port >= 1, Port =< 1023 -> true;
is_privileged_port(_) -> false.
