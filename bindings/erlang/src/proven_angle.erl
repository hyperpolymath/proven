%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeAngle - Thin NIF wrapper for angle conversion operations.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang -- only data marshaling.

-module(proven_angle).

-export([
    deg_to_rad/1,
    rad_to_deg/1,
    normalize_degrees/1,
    normalize_radians/1
]).

%% @doc Convert degrees to radians.
%% Delegates to proven:angle_deg_to_rad/1 NIF.
-spec deg_to_rad(number()) -> float().
deg_to_rad(Degrees) when is_number(Degrees) ->
    proven:angle_deg_to_rad(Degrees).

%% @doc Convert radians to degrees.
%% Delegates to proven:angle_rad_to_deg/1 NIF.
-spec rad_to_deg(number()) -> float().
rad_to_deg(Radians) when is_number(Radians) ->
    proven:angle_rad_to_deg(Radians).

%% @doc Normalize an angle in degrees to [0, 360).
%% Delegates to proven:angle_normalize_degrees/1 NIF.
-spec normalize_degrees(number()) -> float().
normalize_degrees(Degrees) when is_number(Degrees) ->
    proven:angle_normalize_degrees(Degrees).

%% @doc Normalize an angle in radians to [0, 2*pi).
%% Delegates to proven:angle_normalize_radians/1 NIF.
-spec normalize_radians(number()) -> float().
normalize_radians(Radians) when is_number(Radians) ->
    proven:angle_normalize_radians(Radians).
