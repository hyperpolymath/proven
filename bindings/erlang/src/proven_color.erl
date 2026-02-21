%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeColor - Color manipulation for Erlang

-module(proven_color).

-export([
    rgb/3,
    rgba/4,
    from_hex/1,
    to_hex/1,
    to_hex_alpha/1,
    to_rgb/1,
    to_hsl/1,
    from_hsl/3,
    lighten/2,
    darken/2,
    saturate/2,
    desaturate/2,
    invert/1,
    grayscale/1,
    mix/3,
    contrast_ratio/2,
    is_accessible_aa/2,
    is_accessible_aaa/2,
    complementary/1,
    format_css/1
]).

-export_type([color/0]).

%% Color record
-record(color, {
    r :: 0..255,
    g :: 0..255,
    b :: 0..255,
    a :: float()
}).

-type color() :: #color{}.

%% @doc Create an RGB color.
-spec rgb(integer(), integer(), integer()) -> {ok, color()} | {error, out_of_range}.
rgb(R, G, B) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
    {ok, #color{r = R, g = G, b = B, a = 1.0}};
rgb(_, _, _) ->
    {error, out_of_range}.

%% @doc Create an RGBA color.
-spec rgba(integer(), integer(), integer(), float()) -> {ok, color()} | {error, out_of_range}.
rgba(R, G, B, A) when R >= 0, R =< 255, G >= 0, G =< 255,
                      B >= 0, B =< 255, A >= 0.0, A =< 1.0 ->
    {ok, #color{r = R, g = G, b = B, a = A}};
rgba(_, _, _, _) ->
    {error, out_of_range}.

%% @doc Parse a hex color string.
-spec from_hex(string() | binary()) -> {ok, color()} | {error, invalid_format}.
from_hex(Hex) when is_binary(Hex) ->
    from_hex(binary_to_list(Hex));
from_hex([$# | Rest]) ->
    from_hex(Rest);
from_hex(Hex) when length(Hex) == 3 ->
    [R, G, B] = Hex,
    from_hex([R, R, G, G, B, B]);
from_hex(Hex) when length(Hex) == 6 ->
    try
        R = list_to_integer(string:substr(Hex, 1, 2), 16),
        G = list_to_integer(string:substr(Hex, 3, 2), 16),
        B = list_to_integer(string:substr(Hex, 5, 2), 16),
        {ok, #color{r = R, g = G, b = B, a = 1.0}}
    catch
        _:_ -> {error, invalid_format}
    end;
from_hex(Hex) when length(Hex) == 8 ->
    try
        R = list_to_integer(string:substr(Hex, 1, 2), 16),
        G = list_to_integer(string:substr(Hex, 3, 2), 16),
        B = list_to_integer(string:substr(Hex, 5, 2), 16),
        A = list_to_integer(string:substr(Hex, 7, 2), 16) / 255.0,
        {ok, #color{r = R, g = G, b = B, a = A}}
    catch
        _:_ -> {error, invalid_format}
    end;
from_hex(_) ->
    {error, invalid_format}.

%% @doc Convert color to hex string.
-spec to_hex(color()) -> string().
to_hex(#color{r = R, g = G, b = B}) ->
    io_lib:format("#~2.16.0B~2.16.0B~2.16.0B", [R, G, B]).

%% @doc Convert color to hex string with alpha.
-spec to_hex_alpha(color()) -> string().
to_hex_alpha(#color{r = R, g = G, b = B, a = A}) ->
    Alpha = round(A * 255),
    io_lib:format("#~2.16.0B~2.16.0B~2.16.0B~2.16.0B", [R, G, B, Alpha]).

%% @doc Get RGB values as a tuple.
-spec to_rgb(color()) -> {integer(), integer(), integer()}.
to_rgb(#color{r = R, g = G, b = B}) ->
    {R, G, B}.

%% @doc Convert RGB to HSL.
-spec to_hsl(color()) -> {float(), float(), float()}.
to_hsl(#color{r = R, g = G, b = B}) ->
    R1 = R / 255.0,
    G1 = G / 255.0,
    B1 = B / 255.0,
    Max = lists:max([R1, G1, B1]),
    Min = lists:min([R1, G1, B1]),
    L = (Max + Min) / 2,
    case Max == Min of
        true ->
            {0.0, 0.0, L};
        false ->
            D = Max - Min,
            S = if
                L > 0.5 -> D / (2.0 - Max - Min);
                true -> D / (Max + Min)
            end,
            H = if
                Max == R1 ->
                    ((G1 - B1) / D) + (if G1 < B1 -> 6.0; true -> 0.0 end);
                Max == G1 ->
                    ((B1 - R1) / D) + 2.0;
                true ->
                    ((R1 - G1) / D) + 4.0
            end,
            {H / 6.0 * 360.0, S * 100.0, L * 100.0}
    end.

%% @doc Create a color from HSL values.
-spec from_hsl(float(), float(), float()) -> {ok, color()} | {error, out_of_range}.
from_hsl(H, S, L) when H >= 0, H =< 360, S >= 0, S =< 100, L >= 0, L =< 100 ->
    H1 = H / 360.0,
    S1 = S / 100.0,
    L1 = L / 100.0,
    case S1 == 0.0 of
        true ->
            V = round(L1 * 255),
            {ok, #color{r = V, g = V, b = V, a = 1.0}};
        false ->
            Q = if
                L1 < 0.5 -> L1 * (1.0 + S1);
                true -> L1 + S1 - L1 * S1
            end,
            P = 2.0 * L1 - Q,
            R = round(hue_to_rgb(P, Q, H1 + 1.0/3.0) * 255),
            G = round(hue_to_rgb(P, Q, H1) * 255),
            B = round(hue_to_rgb(P, Q, H1 - 1.0/3.0) * 255),
            {ok, #color{r = R, g = G, b = B, a = 1.0}}
    end;
from_hsl(_, _, _) ->
    {error, out_of_range}.

hue_to_rgb(P, Q, T0) ->
    T = if
        T0 < 0.0 -> T0 + 1.0;
        T0 > 1.0 -> T0 - 1.0;
        true -> T0
    end,
    if
        T < 1.0/6.0 -> P + (Q - P) * 6.0 * T;
        T < 1.0/2.0 -> Q;
        T < 2.0/3.0 -> P + (Q - P) * (2.0/3.0 - T) * 6.0;
        true -> P
    end.

%% @doc Lighten a color by a percentage.
-spec lighten(color(), float()) -> color().
lighten(Color, Percent) when Percent >= 0, Percent =< 100 ->
    {H, S, L} = to_hsl(Color),
    NewL = min(100.0, L + Percent),
    {ok, NewColor} = from_hsl(H, S, NewL),
    NewColor#color{a = Color#color.a}.

%% @doc Darken a color by a percentage.
-spec darken(color(), float()) -> color().
darken(Color, Percent) when Percent >= 0, Percent =< 100 ->
    {H, S, L} = to_hsl(Color),
    NewL = max(0.0, L - Percent),
    {ok, NewColor} = from_hsl(H, S, NewL),
    NewColor#color{a = Color#color.a}.

%% @doc Increase saturation by a percentage.
-spec saturate(color(), float()) -> color().
saturate(Color, Percent) when Percent >= 0, Percent =< 100 ->
    {H, S, L} = to_hsl(Color),
    NewS = min(100.0, S + Percent),
    {ok, NewColor} = from_hsl(H, NewS, L),
    NewColor#color{a = Color#color.a}.

%% @doc Decrease saturation by a percentage.
-spec desaturate(color(), float()) -> color().
desaturate(Color, Percent) when Percent >= 0, Percent =< 100 ->
    {H, S, L} = to_hsl(Color),
    NewS = max(0.0, S - Percent),
    {ok, NewColor} = from_hsl(H, NewS, L),
    NewColor#color{a = Color#color.a}.

%% @doc Invert a color.
-spec invert(color()) -> color().
invert(#color{r = R, g = G, b = B, a = A}) ->
    #color{r = 255 - R, g = 255 - G, b = 255 - B, a = A}.

%% @doc Convert to grayscale.
-spec grayscale(color()) -> color().
grayscale(#color{r = R, g = G, b = B, a = A}) ->
    %% Use luminosity method
    Gray = round(0.299 * R + 0.587 * G + 0.114 * B),
    #color{r = Gray, g = Gray, b = Gray, a = A}.

%% @doc Mix two colors with a weight (0.0 = all color1, 1.0 = all color2).
-spec mix(color(), color(), float()) -> color().
mix(C1, C2, Weight) when Weight >= 0.0, Weight =< 1.0 ->
    W = 1.0 - Weight,
    R = round(C1#color.r * W + C2#color.r * Weight),
    G = round(C1#color.g * W + C2#color.g * Weight),
    B = round(C1#color.b * W + C2#color.b * Weight),
    A = C1#color.a * W + C2#color.a * Weight,
    #color{r = R, g = G, b = B, a = A}.

%% @doc Calculate contrast ratio between two colors (WCAG).
-spec contrast_ratio(color(), color()) -> float().
contrast_ratio(C1, C2) ->
    L1 = relative_luminance(C1),
    L2 = relative_luminance(C2),
    {Lighter, Darker} = if L1 > L2 -> {L1, L2}; true -> {L2, L1} end,
    (Lighter + 0.05) / (Darker + 0.05).

relative_luminance(#color{r = R, g = G, b = B}) ->
    Rl = linearize(R / 255.0),
    Gl = linearize(G / 255.0),
    Bl = linearize(B / 255.0),
    0.2126 * Rl + 0.7152 * Gl + 0.0722 * Bl.

linearize(C) when C =< 0.03928 ->
    C / 12.92;
linearize(C) ->
    math:pow((C + 0.055) / 1.055, 2.4).

%% @doc Check if colors meet WCAG AA contrast (4.5:1 for text).
-spec is_accessible_aa(color(), color()) -> boolean().
is_accessible_aa(C1, C2) ->
    contrast_ratio(C1, C2) >= 4.5.

%% @doc Check if colors meet WCAG AAA contrast (7:1 for text).
-spec is_accessible_aaa(color(), color()) -> boolean().
is_accessible_aaa(C1, C2) ->
    contrast_ratio(C1, C2) >= 7.0.

%% @doc Get the complementary color.
-spec complementary(color()) -> color().
complementary(Color) ->
    {H, S, L} = to_hsl(Color),
    NewH = if H >= 180.0 -> H - 180.0; true -> H + 180.0 end,
    {ok, NewColor} = from_hsl(NewH, S, L),
    NewColor#color{a = Color#color.a}.

%% @doc Format as CSS color string.
-spec format_css(color()) -> string().
format_css(#color{r = R, g = G, b = B, a = 1.0}) ->
    io_lib:format("rgb(~B, ~B, ~B)", [R, G, B]);
format_css(#color{r = R, g = G, b = B, a = A}) ->
    io_lib:format("rgba(~B, ~B, ~B, ~.2f)", [R, G, B, A]).
