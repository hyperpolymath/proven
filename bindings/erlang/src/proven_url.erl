%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeURL - URL parsing and validation for Erlang

-module(proven_url).

-export([
    parse/1,
    is_valid/1,
    get_scheme/1,
    get_host/1,
    get_port/1,
    get_path/1,
    get_query/1,
    get_fragment/1,
    build/1,
    join/2,
    encode_query/1,
    decode_query/1
]).

-export_type([parsed_url/0]).

%% Parsed URL record
-record(parsed_url, {
    scheme :: string(),
    username :: string() | undefined,
    password :: string() | undefined,
    host :: string(),
    port :: integer() | undefined,
    path :: string(),
    query :: string() | undefined,
    fragment :: string() | undefined
}).

-type parsed_url() :: #parsed_url{}.

%% @doc Parse a URL string into components.
-spec parse(string() | binary()) -> {ok, parsed_url()} | {error, atom()}.
parse(Url) when is_binary(Url) ->
    parse(binary_to_list(Url));
parse(Url) when is_list(Url) ->
    Trimmed = string:trim(Url),
    case Trimmed of
        [] ->
            {error, empty_url};
        _ ->
            parse_scheme(Trimmed)
    end.

parse_scheme(Url) ->
    case string:split(Url, "://") of
        [_] ->
            {error, missing_scheme};
        [Scheme, Rest] ->
            parse_authority(string:lowercase(Scheme), Rest)
    end.

parse_authority(Scheme, Rest) ->
    %% Extract fragment
    {Rest1, Fragment} = case string:split(Rest, "#", trailing) of
        [R, F] -> {R, F};
        [R] -> {R, undefined}
    end,
    %% Extract query
    {Rest2, Query} = case string:split(Rest1, "?") of
        [R2, Q] -> {R2, Q};
        [R2] -> {R2, undefined}
    end,
    %% Extract path
    {Authority, Path} = case string:chr(Rest2, $/) of
        0 -> {Rest2, "/"};
        Pos -> {string:substr(Rest2, 1, Pos - 1), string:substr(Rest2, Pos)}
    end,
    %% Extract userinfo
    {Username, Password, HostPort} = case string:split(Authority, "@", trailing) of
        [H] -> {undefined, undefined, H};
        [UserInfo, H] ->
            case string:split(UserInfo, ":") of
                [U, P] -> {U, P, H};
                [U] -> {U, undefined, H}
            end
    end,
    %% Extract port
    {Host, Port} = parse_host_port(HostPort),
    {ok, #parsed_url{
        scheme = Scheme,
        username = Username,
        password = Password,
        host = Host,
        port = Port,
        path = Path,
        query = Query,
        fragment = Fragment
    }}.

parse_host_port(HostPort) ->
    case string:rchr(HostPort, $:) of
        0 ->
            {HostPort, undefined};
        Pos ->
            Host = string:substr(HostPort, 1, Pos - 1),
            PortStr = string:substr(HostPort, Pos + 1),
            case string:to_integer(PortStr) of
                {Port, []} when Port >= 1, Port =< 65535 ->
                    {Host, Port};
                _ ->
                    {HostPort, undefined}
            end
    end.

%% @doc Check if a URL string is valid.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Url) ->
    case parse(Url) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @doc Get the scheme from a parsed URL.
-spec get_scheme(parsed_url()) -> string().
get_scheme(#parsed_url{scheme = Scheme}) -> Scheme.

%% @doc Get the host from a parsed URL.
-spec get_host(parsed_url()) -> string().
get_host(#parsed_url{host = Host}) -> Host.

%% @doc Get the port from a parsed URL.
-spec get_port(parsed_url()) -> integer() | undefined.
get_port(#parsed_url{port = Port}) -> Port.

%% @doc Get the path from a parsed URL.
-spec get_path(parsed_url()) -> string().
get_path(#parsed_url{path = Path}) -> Path.

%% @doc Get the query string from a parsed URL.
-spec get_query(parsed_url()) -> string() | undefined.
get_query(#parsed_url{query = Query}) -> Query.

%% @doc Get the fragment from a parsed URL.
-spec get_fragment(parsed_url()) -> string() | undefined.
get_fragment(#parsed_url{fragment = Fragment}) -> Fragment.

%% @doc Build a URL string from a parsed URL record.
-spec build(parsed_url()) -> string().
build(#parsed_url{scheme = Scheme, username = Username, password = Password,
                  host = Host, port = Port, path = Path,
                  query = Query, fragment = Fragment}) ->
    Base = Scheme ++ "://",
    WithAuth = case Username of
        undefined -> Base;
        _ ->
            case Password of
                undefined -> Base ++ Username ++ "@";
                _ -> Base ++ Username ++ ":" ++ Password ++ "@"
            end
    end,
    WithHost = WithAuth ++ Host,
    WithPort = case Port of
        undefined -> WithHost;
        _ -> WithHost ++ ":" ++ integer_to_list(Port)
    end,
    WithPath = WithPort ++ Path,
    WithQuery = case Query of
        undefined -> WithPath;
        _ -> WithPath ++ "?" ++ Query
    end,
    case Fragment of
        undefined -> WithQuery;
        _ -> WithQuery ++ "#" ++ Fragment
    end.

%% @doc Join a base URL with a relative path.
-spec join(parsed_url(), string()) -> parsed_url().
join(BaseUrl = #parsed_url{path = BasePath}, RelativePath) ->
    NewPath = case RelativePath of
        [$/ | _] -> RelativePath;
        _ ->
            %% Remove filename from base path
            case string:rchr(BasePath, $/) of
                0 -> "/" ++ RelativePath;
                Pos -> string:substr(BasePath, 1, Pos) ++ RelativePath
            end
    end,
    BaseUrl#parsed_url{path = NewPath, query = undefined, fragment = undefined}.

%% @doc Encode a list of key-value pairs as a query string.
-spec encode_query([{string(), string()}]) -> string().
encode_query(Pairs) ->
    Encoded = [url_encode_pair(K, V) || {K, V} <- Pairs],
    string:join(Encoded, "&").

url_encode_pair(Key, Value) ->
    url_encode(Key) ++ "=" ++ url_encode(Value).

url_encode(String) ->
    lists:flatmap(fun url_encode_char/1, String).

url_encode_char(C) when C >= $a, C =< $z -> [C];
url_encode_char(C) when C >= $A, C =< $Z -> [C];
url_encode_char(C) when C >= $0, C =< $9 -> [C];
url_encode_char($-) -> "-";
url_encode_char($_) -> "_";
url_encode_char($.) -> ".";
url_encode_char($~) -> "~";
url_encode_char(C) ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 16#F)].

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.

%% @doc Decode a query string into key-value pairs.
-spec decode_query(string()) -> [{string(), string()}].
decode_query(QueryString) ->
    Pairs = string:tokens(QueryString, "&"),
    [decode_pair(Pair) || Pair <- Pairs].

decode_pair(Pair) ->
    case string:split(Pair, "=") of
        [Key, Value] -> {url_decode(Key), url_decode(Value)};
        [Key] -> {url_decode(Key), ""}
    end.

url_decode(String) ->
    url_decode(String, []).

url_decode([], Acc) ->
    lists:reverse(Acc);
url_decode([$%, H1, H2 | Rest], Acc) ->
    case {hex_value(H1), hex_value(H2)} of
        {{ok, V1}, {ok, V2}} ->
            url_decode(Rest, [(V1 bsl 4) bor V2 | Acc]);
        _ ->
            url_decode(Rest, [H2, H1, $% | Acc])
    end;
url_decode([$+ | Rest], Acc) ->
    url_decode(Rest, [$\s | Acc]);
url_decode([C | Rest], Acc) ->
    url_decode(Rest, [C | Acc]).

hex_value(C) when C >= $0, C =< $9 -> {ok, C - $0};
hex_value(C) when C >= $a, C =< $f -> {ok, C - $a + 10};
hex_value(C) when C >= $A, C =< $F -> {ok, C - $A + 10};
hex_value(_) -> error.
