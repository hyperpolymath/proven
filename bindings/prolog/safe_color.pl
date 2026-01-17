% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeColor - Color manipulation for Prolog
% Supports RGB, HSL, HSV, and named colors.

:- module(safe_color, [
    rgb/4,
    rgba/5,
    hsl/4,
    hsv/4,
    parse_hex_color/2,
    format_hex_color/2,
    rgb_to_hsl/2,
    hsl_to_rgb/2,
    rgb_to_hsv/2,
    hsv_to_rgb/2,
    color_blend/4,
    color_lighten/3,
    color_darken/3,
    color_saturate/3,
    color_desaturate/3,
    color_invert/2,
    color_grayscale/2,
    color_contrast/3,
    named_color/2,
    is_valid_color/1,
    color_luminance/2
]).

:- use_module(library(lists)).

%! rgb(?R, ?G, ?B, ?Color) is det.
%
%  Construct or deconstruct RGB color.
%  R, G, B are 0-255.
%
%  @arg R Red component (0-255)
%  @arg G Green component (0-255)
%  @arg B Blue component (0-255)
%  @arg Color rgb/3 structure
rgb(R, G, B, rgb(R, G, B)) :-
    R >= 0, R =< 255,
    G >= 0, G =< 255,
    B >= 0, B =< 255.

%! rgba(?R, ?G, ?B, ?A, ?Color) is det.
%
%  Construct or deconstruct RGBA color.
%  A is 0.0-1.0.
%
%  @arg R Red component (0-255)
%  @arg G Green component (0-255)
%  @arg B Blue component (0-255)
%  @arg A Alpha component (0.0-1.0)
%  @arg Color rgba/4 structure
rgba(R, G, B, A, rgba(R, G, B, A)) :-
    R >= 0, R =< 255,
    G >= 0, G =< 255,
    B >= 0, B =< 255,
    A >= 0.0, A =< 1.0.

%! hsl(?H, ?S, ?L, ?Color) is det.
%
%  Construct or deconstruct HSL color.
%  H is 0-360, S and L are 0.0-1.0.
%
%  @arg H Hue (0-360)
%  @arg S Saturation (0.0-1.0)
%  @arg L Lightness (0.0-1.0)
%  @arg Color hsl/3 structure
hsl(H, S, L, hsl(H, S, L)) :-
    H >= 0, H =< 360,
    S >= 0.0, S =< 1.0,
    L >= 0.0, L =< 1.0.

%! hsv(?H, ?S, ?V, ?Color) is det.
%
%  Construct or deconstruct HSV color.
%  H is 0-360, S and V are 0.0-1.0.
%
%  @arg H Hue (0-360)
%  @arg S Saturation (0.0-1.0)
%  @arg V Value (0.0-1.0)
%  @arg Color hsv/3 structure
hsv(H, S, V, hsv(H, S, V)) :-
    H >= 0, H =< 360,
    S >= 0.0, S =< 1.0,
    V >= 0.0, V =< 1.0.

%! parse_hex_color(+HexString, -Color) is semidet.
%
%  Parse hex color string (#RGB, #RRGGBB, #RRGGBBAA).
%
%  @arg HexString Hex color string
%  @arg Color rgb/3 or rgba/4 structure
parse_hex_color(HexString, Color) :-
    atom_string(HexString, Str),
    string_chars(Str, Chars),
    (   Chars = ['#'|HexChars]
    ->  true
    ;   HexChars = Chars
    ),
    parse_hex_chars(HexChars, Color).

parse_hex_chars([R1, R2, G1, G2, B1, B2, A1, A2], rgba(R, G, B, A)) :- !,
    hex_pair_value([R1, R2], R),
    hex_pair_value([G1, G2], G),
    hex_pair_value([B1, B2], B),
    hex_pair_value([A1, A2], AInt),
    A is AInt / 255.0.
parse_hex_chars([R1, R2, G1, G2, B1, B2], rgb(R, G, B)) :- !,
    hex_pair_value([R1, R2], R),
    hex_pair_value([G1, G2], G),
    hex_pair_value([B1, B2], B).
parse_hex_chars([R, G, B], rgb(RV, GV, BV)) :-
    hex_char_value(R, RH),
    hex_char_value(G, GH),
    hex_char_value(B, BH),
    RV is RH * 16 + RH,
    GV is GH * 16 + GH,
    BV is BH * 16 + BH.

hex_pair_value([H1, H2], Value) :-
    hex_char_value(H1, High),
    hex_char_value(H2, Low),
    Value is High * 16 + Low.

hex_char_value(C, V) :-
    char_code(C, Code),
    (   Code >= 0'0, Code =< 0'9 -> V is Code - 0'0
    ;   Code >= 0'a, Code =< 0'f -> V is Code - 0'a + 10
    ;   Code >= 0'A, Code =< 0'F -> V is Code - 0'A + 10
    ).

%! format_hex_color(+Color, -HexString) is det.
%
%  Format color as hex string.
%
%  @arg Color rgb/3 or rgba/4 structure
%  @arg HexString Hex color string
format_hex_color(rgb(R, G, B), HexString) :-
    format(atom(HexString), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+", [R, G, B]).
format_hex_color(rgba(R, G, B, A), HexString) :-
    AInt is round(A * 255),
    format(atom(HexString), "#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+", [R, G, B, AInt]).

%! rgb_to_hsl(+RGB, -HSL) is det.
%
%  Convert RGB to HSL color space.
%
%  @arg RGB rgb/3 structure
%  @arg HSL hsl/3 structure
rgb_to_hsl(rgb(R, G, B), hsl(H, S, L)) :-
    Rn is R / 255.0,
    Gn is G / 255.0,
    Bn is B / 255.0,
    Max is max(Rn, max(Gn, Bn)),
    Min is min(Rn, min(Gn, Bn)),
    L is (Max + Min) / 2,
    Delta is Max - Min,
    (   Delta =:= 0
    ->  H = 0, S = 0
    ;   (   L < 0.5
        ->  S is Delta / (Max + Min)
        ;   S is Delta / (2 - Max - Min)
        ),
        (   Max =:= Rn
        ->  H0 is ((Gn - Bn) / Delta) + (Gn < Bn -> 6 ; 0)
        ;   Max =:= Gn
        ->  H0 is ((Bn - Rn) / Delta) + 2
        ;   H0 is ((Rn - Gn) / Delta) + 4
        ),
        H is H0 * 60
    ).

%! hsl_to_rgb(+HSL, -RGB) is det.
%
%  Convert HSL to RGB color space.
%
%  @arg HSL hsl/3 structure
%  @arg RGB rgb/3 structure
hsl_to_rgb(hsl(H, S, L), rgb(R, G, B)) :-
    (   S =:= 0
    ->  R is round(L * 255),
        G is round(L * 255),
        B is round(L * 255)
    ;   (   L < 0.5
        ->  Q is L * (1 + S)
        ;   Q is L + S - L * S
        ),
        P is 2 * L - Q,
        Hk is H / 360,
        hue_to_rgb(P, Q, Hk + 1/3, Rn),
        hue_to_rgb(P, Q, Hk, Gn),
        hue_to_rgb(P, Q, Hk - 1/3, Bn),
        R is round(Rn * 255),
        G is round(Gn * 255),
        B is round(Bn * 255)
    ).

hue_to_rgb(P, Q, T0, Value) :-
    (   T0 < 0 -> T is T0 + 1 ; T0 > 1 -> T is T0 - 1 ; T = T0 ),
    (   T < 1/6 -> Value is P + (Q - P) * 6 * T
    ;   T < 1/2 -> Value is Q
    ;   T < 2/3 -> Value is P + (Q - P) * (2/3 - T) * 6
    ;   Value is P
    ).

%! rgb_to_hsv(+RGB, -HSV) is det.
%
%  Convert RGB to HSV color space.
%
%  @arg RGB rgb/3 structure
%  @arg HSV hsv/3 structure
rgb_to_hsv(rgb(R, G, B), hsv(H, S, V)) :-
    Rn is R / 255.0,
    Gn is G / 255.0,
    Bn is B / 255.0,
    Max is max(Rn, max(Gn, Bn)),
    Min is min(Rn, min(Gn, Bn)),
    V is Max,
    Delta is Max - Min,
    (   Max =:= 0
    ->  S = 0, H = 0
    ;   S is Delta / Max,
        (   Delta =:= 0
        ->  H = 0
        ;   Max =:= Rn
        ->  H0 is ((Gn - Bn) / Delta) mod 6,
            H is H0 * 60
        ;   Max =:= Gn
        ->  H is (((Bn - Rn) / Delta) + 2) * 60
        ;   H is (((Rn - Gn) / Delta) + 4) * 60
        )
    ).

%! hsv_to_rgb(+HSV, -RGB) is det.
%
%  Convert HSV to RGB color space.
%
%  @arg HSV hsv/3 structure
%  @arg RGB rgb/3 structure
hsv_to_rgb(hsv(H, S, V), rgb(R, G, B)) :-
    C is V * S,
    X is C * (1 - abs((H / 60) mod 2 - 1)),
    M is V - C,
    hsv_rgb_sector(H, C, X, Rn, Gn, Bn),
    R is round((Rn + M) * 255),
    G is round((Gn + M) * 255),
    B is round((Bn + M) * 255).

hsv_rgb_sector(H, C, X, C, X, 0) :- H >= 0, H < 60, !.
hsv_rgb_sector(H, C, X, X, C, 0) :- H >= 60, H < 120, !.
hsv_rgb_sector(H, C, X, 0, C, X) :- H >= 120, H < 180, !.
hsv_rgb_sector(H, C, X, 0, X, C) :- H >= 180, H < 240, !.
hsv_rgb_sector(H, C, X, X, 0, C) :- H >= 240, H < 300, !.
hsv_rgb_sector(_, C, X, C, 0, X).

%! color_blend(+Color1, +Color2, +Factor, -Result) is det.
%
%  Blend two colors. Factor 0=Color1, 1=Color2.
%
%  @arg Color1 First color (rgb/3)
%  @arg Color2 Second color (rgb/3)
%  @arg Factor Blend factor (0.0-1.0)
%  @arg Result Blended color (rgb/3)
color_blend(rgb(R1, G1, B1), rgb(R2, G2, B2), Factor, rgb(R, G, B)) :-
    R is round(R1 + (R2 - R1) * Factor),
    G is round(G1 + (G2 - G1) * Factor),
    B is round(B1 + (B2 - B1) * Factor).

%! color_lighten(+Color, +Amount, -Result) is det.
%
%  Lighten a color by amount (0.0-1.0).
%
%  @arg Color Original color (rgb/3)
%  @arg Amount Lighten amount
%  @arg Result Lightened color
color_lighten(rgb(R, G, B), Amount, rgb(NR, NG, NB)) :-
    NR is min(255, round(R + (255 - R) * Amount)),
    NG is min(255, round(G + (255 - G) * Amount)),
    NB is min(255, round(B + (255 - B) * Amount)).

%! color_darken(+Color, +Amount, -Result) is det.
%
%  Darken a color by amount (0.0-1.0).
%
%  @arg Color Original color (rgb/3)
%  @arg Amount Darken amount
%  @arg Result Darkened color
color_darken(rgb(R, G, B), Amount, rgb(NR, NG, NB)) :-
    NR is max(0, round(R * (1 - Amount))),
    NG is max(0, round(G * (1 - Amount))),
    NB is max(0, round(B * (1 - Amount))).

%! color_saturate(+Color, +Amount, -Result) is det.
%
%  Increase saturation by amount.
%
%  @arg Color Original color (rgb/3)
%  @arg Amount Saturation increase (0.0-1.0)
%  @arg Result Saturated color
color_saturate(RGB, Amount, Result) :-
    rgb_to_hsl(RGB, hsl(H, S, L)),
    NS is min(1.0, S + Amount),
    hsl_to_rgb(hsl(H, NS, L), Result).

%! color_desaturate(+Color, +Amount, -Result) is det.
%
%  Decrease saturation by amount.
%
%  @arg Color Original color (rgb/3)
%  @arg Amount Saturation decrease (0.0-1.0)
%  @arg Result Desaturated color
color_desaturate(RGB, Amount, Result) :-
    rgb_to_hsl(RGB, hsl(H, S, L)),
    NS is max(0.0, S - Amount),
    hsl_to_rgb(hsl(H, NS, L), Result).

%! color_invert(+Color, -Result) is det.
%
%  Invert a color.
%
%  @arg Color Original color (rgb/3)
%  @arg Result Inverted color
color_invert(rgb(R, G, B), rgb(NR, NG, NB)) :-
    NR is 255 - R,
    NG is 255 - G,
    NB is 255 - B.

%! color_grayscale(+Color, -Result) is det.
%
%  Convert color to grayscale.
%
%  @arg Color Original color (rgb/3)
%  @arg Result Grayscale color
color_grayscale(rgb(R, G, B), rgb(Gray, Gray, Gray)) :-
    Gray is round(0.299 * R + 0.587 * G + 0.114 * B).

%! color_contrast(+Color1, +Color2, -Ratio) is det.
%
%  Calculate contrast ratio between two colors (WCAG).
%
%  @arg Color1 First color (rgb/3)
%  @arg Color2 Second color (rgb/3)
%  @arg Ratio Contrast ratio (1-21)
color_contrast(C1, C2, Ratio) :-
    color_luminance(C1, L1),
    color_luminance(C2, L2),
    Lighter is max(L1, L2),
    Darker is min(L1, L2),
    Ratio is (Lighter + 0.05) / (Darker + 0.05).

%! color_luminance(+Color, -Luminance) is det.
%
%  Calculate relative luminance of color.
%
%  @arg Color rgb/3 structure
%  @arg Luminance Relative luminance (0-1)
color_luminance(rgb(R, G, B), Luminance) :-
    Rn is R / 255.0,
    Gn is G / 255.0,
    Bn is B / 255.0,
    linearize(Rn, RL),
    linearize(Gn, GL),
    linearize(Bn, BL),
    Luminance is 0.2126 * RL + 0.7152 * GL + 0.0722 * BL.

linearize(C, L) :-
    (   C =< 0.03928
    ->  L is C / 12.92
    ;   L is ((C + 0.055) / 1.055) ** 2.4
    ).

%! named_color(?Name, ?Color) is nondet.
%
%  CSS named colors.
%
%  @arg Name Color name atom
%  @arg Color rgb/3 structure
named_color(black, rgb(0, 0, 0)).
named_color(white, rgb(255, 255, 255)).
named_color(red, rgb(255, 0, 0)).
named_color(green, rgb(0, 128, 0)).
named_color(blue, rgb(0, 0, 255)).
named_color(yellow, rgb(255, 255, 0)).
named_color(cyan, rgb(0, 255, 255)).
named_color(magenta, rgb(255, 0, 255)).
named_color(orange, rgb(255, 165, 0)).
named_color(purple, rgb(128, 0, 128)).
named_color(pink, rgb(255, 192, 203)).
named_color(gray, rgb(128, 128, 128)).
named_color(grey, rgb(128, 128, 128)).
named_color(silver, rgb(192, 192, 192)).
named_color(maroon, rgb(128, 0, 0)).
named_color(olive, rgb(128, 128, 0)).
named_color(lime, rgb(0, 255, 0)).
named_color(aqua, rgb(0, 255, 255)).
named_color(teal, rgb(0, 128, 128)).
named_color(navy, rgb(0, 0, 128)).
named_color(fuchsia, rgb(255, 0, 255)).

%! is_valid_color(+Color) is semidet.
%
%  Check if color term is valid.
%
%  @arg Color Color term to validate
is_valid_color(rgb(R, G, B)) :-
    integer(R), R >= 0, R =< 255,
    integer(G), G >= 0, G =< 255,
    integer(B), B >= 0, B =< 255.
is_valid_color(rgba(R, G, B, A)) :-
    is_valid_color(rgb(R, G, B)),
    number(A), A >= 0, A =< 1.
is_valid_color(hsl(H, S, L)) :-
    number(H), H >= 0, H =< 360,
    number(S), S >= 0, S =< 1,
    number(L), L >= 0, L =< 1.
is_valid_color(hsv(H, S, V)) :-
    number(H), H >= 0, H =< 360,
    number(S), S >= 0, S =< 1,
    number(V), V >= 0, V =< 1.
