%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeColor - FFI bindings to libproven color operations.
%% All color operations are performed in verified Idris 2 code via libproven.

:- module(safe_color, [
    parse_hex_color/2,
    rgb_to_hsl/4,
    color_to_hex/4
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven color functions.
:- foreign(proven_color_parse_hex_ffi, c,
           proven_color_parse_hex(+string, +integer,
                                  [-integer], [-integer], [-integer], [-integer])).
:- foreign(proven_color_rgb_to_hsl_ffi, c,
           proven_color_rgb_to_hsl(+integer, +integer, +integer,
                                   [-float], [-float], [-float])).
:- foreign(proven_color_to_hex_ffi, c,
           proven_color_to_hex(+integer, +integer, +integer,
                               [-integer], [-string])).

%! parse_hex_color(+HexString, -Color) is det.
%
%  Parse hex color string (#RRGGBB or #RGB) via libproven.
%  Color is ok(rgb(R, G, B)) or error(parse_failure).
parse_hex_color(HexString, Color) :-
    atom_string(HexString, Str),
    atom_length(HexString, Len),
    proven_color_parse_hex_ffi(Str, Len, Status, R, G, B),
    (   Status =:= 0
    ->  Color = ok(rgb(R, G, B))
    ;   Color = error(parse_failure)
    ).

%! rgb_to_hsl(+R, +G, +B, -HSL) is det.
%
%  Convert RGB to HSL color space via libproven.
%  HSL is hsl(H, S, L) where H is 0-360, S and L are 0.0-1.0.
rgb_to_hsl(R, G, B, hsl(H, S, L)) :-
    proven_color_rgb_to_hsl_ffi(R, G, B, H, S, L).

%! color_to_hex(+R, +G, +B, -HexString) is det.
%
%  Format RGB as hex string ("#rrggbb") via libproven.
color_to_hex(R, G, B, HexString) :-
    proven_color_to_hex_ffi(R, G, B, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(HexString, ResultStr)
    ;   HexString = ''
    ).
