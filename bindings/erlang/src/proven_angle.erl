%% SPDX-License-Identifier: PMPL-1.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeAngle - Angle conversion and operations for Erlang

-module(proven_angle).

-export([
    degrees/1,
    radians/1,
    gradians/1,
    turns/1,
    to_degrees/1,
    to_radians/1,
    to_gradians/1,
    to_turns/1,
    normalize/1,
    normalize_signed/1,
    sin/1,
    cos/1,
    tan/1,
    asin/1,
    acos/1,
    atan/1,
    atan2/2,
    add/2,
    subtract/2,
    multiply/2,
    is_acute/1,
    is_right/1,
    is_obtuse/1,
    is_straight/1,
    is_reflex/1,
    complementary/1,
    supplementary/1
]).

-export_type([angle/0]).

%% Constants
-define(PI, 3.141592653589793).
-define(DEG_TO_RAD, ?PI / 180.0).
-define(RAD_TO_DEG, 180.0 / ?PI).
-define(GRAD_TO_DEG, 0.9).
-define(DEG_TO_GRAD, 1.0 / 0.9).

%% Angle record (internally stored as radians)
-record(angle, {
    radians :: float()
}).

-type angle() :: #angle{}.

%% @doc Create an angle from degrees.
-spec degrees(number()) -> angle().
degrees(Deg) when is_number(Deg) ->
    #angle{radians = Deg * ?DEG_TO_RAD}.

%% @doc Create an angle from radians.
-spec radians(number()) -> angle().
radians(Rad) when is_number(Rad) ->
    #angle{radians = float(Rad)}.

%% @doc Create an angle from gradians.
-spec gradians(number()) -> angle().
gradians(Grad) when is_number(Grad) ->
    #angle{radians = Grad * ?GRAD_TO_DEG * ?DEG_TO_RAD}.

%% @doc Create an angle from turns (full rotations).
-spec turns(number()) -> angle().
turns(Turn) when is_number(Turn) ->
    #angle{radians = Turn * 2.0 * ?PI}.

%% @doc Convert angle to degrees.
-spec to_degrees(angle()) -> float().
to_degrees(#angle{radians = Rad}) ->
    Rad * ?RAD_TO_DEG.

%% @doc Convert angle to radians.
-spec to_radians(angle()) -> float().
to_radians(#angle{radians = Rad}) ->
    Rad.

%% @doc Convert angle to gradians.
-spec to_gradians(angle()) -> float().
to_gradians(Angle) ->
    to_degrees(Angle) * ?DEG_TO_GRAD.

%% @doc Convert angle to turns.
-spec to_turns(angle()) -> float().
to_turns(#angle{radians = Rad}) ->
    Rad / (2.0 * ?PI).

%% @doc Normalize angle to [0, 360) degrees.
-spec normalize(angle()) -> angle().
normalize(#angle{radians = Rad}) ->
    TwoPi = 2.0 * ?PI,
    Normalized = Rad - TwoPi * floor(Rad / TwoPi),
    #angle{radians = Normalized}.

%% @doc Normalize angle to [-180, 180) degrees.
-spec normalize_signed(angle()) -> angle().
normalize_signed(Angle) ->
    Normalized = normalize(Angle),
    Deg = to_degrees(Normalized),
    case Deg >= 180.0 of
        true -> degrees(Deg - 360.0);
        false -> Normalized
    end.

%% @doc Calculate sine of angle.
-spec sin(angle()) -> float().
sin(#angle{radians = Rad}) ->
    math:sin(Rad).

%% @doc Calculate cosine of angle.
-spec cos(angle()) -> float().
cos(#angle{radians = Rad}) ->
    math:cos(Rad).

%% @doc Calculate tangent of angle.
-spec tan(angle()) -> float().
tan(#angle{radians = Rad}) ->
    math:tan(Rad).

%% @doc Calculate arcsine (returns angle).
-spec asin(float()) -> {ok, angle()} | {error, out_of_range}.
asin(X) when X >= -1.0, X =< 1.0 ->
    {ok, #angle{radians = math:asin(X)}};
asin(_) ->
    {error, out_of_range}.

%% @doc Calculate arccosine (returns angle).
-spec acos(float()) -> {ok, angle()} | {error, out_of_range}.
acos(X) when X >= -1.0, X =< 1.0 ->
    {ok, #angle{radians = math:acos(X)}};
acos(_) ->
    {error, out_of_range}.

%% @doc Calculate arctangent (returns angle).
-spec atan(float()) -> angle().
atan(X) ->
    #angle{radians = math:atan(X)}.

%% @doc Calculate two-argument arctangent (returns angle).
-spec atan2(float(), float()) -> angle().
atan2(Y, X) ->
    #angle{radians = math:atan2(Y, X)}.

%% @doc Add two angles.
-spec add(angle(), angle()) -> angle().
add(#angle{radians = R1}, #angle{radians = R2}) ->
    #angle{radians = R1 + R2}.

%% @doc Subtract second angle from first.
-spec subtract(angle(), angle()) -> angle().
subtract(#angle{radians = R1}, #angle{radians = R2}) ->
    #angle{radians = R1 - R2}.

%% @doc Multiply angle by a scalar.
-spec multiply(angle(), number()) -> angle().
multiply(#angle{radians = R}, Scalar) when is_number(Scalar) ->
    #angle{radians = R * Scalar}.

%% @doc Check if angle is acute (0, 90).
-spec is_acute(angle()) -> boolean().
is_acute(Angle) ->
    Deg = to_degrees(normalize(Angle)),
    Deg > 0.0 andalso Deg < 90.0.

%% @doc Check if angle is a right angle (90 degrees).
-spec is_right(angle()) -> boolean().
is_right(Angle) ->
    Deg = to_degrees(normalize(Angle)),
    abs(Deg - 90.0) < 0.0001.

%% @doc Check if angle is obtuse (90, 180).
-spec is_obtuse(angle()) -> boolean().
is_obtuse(Angle) ->
    Deg = to_degrees(normalize(Angle)),
    Deg > 90.0 andalso Deg < 180.0.

%% @doc Check if angle is a straight angle (180 degrees).
-spec is_straight(angle()) -> boolean().
is_straight(Angle) ->
    Deg = to_degrees(normalize(Angle)),
    abs(Deg - 180.0) < 0.0001.

%% @doc Check if angle is reflex (180, 360).
-spec is_reflex(angle()) -> boolean().
is_reflex(Angle) ->
    Deg = to_degrees(normalize(Angle)),
    Deg > 180.0 andalso Deg < 360.0.

%% @doc Get complementary angle (90 - angle).
-spec complementary(angle()) -> angle().
complementary(Angle) ->
    degrees(90.0 - to_degrees(normalize(Angle))).

%% @doc Get supplementary angle (180 - angle).
-spec supplementary(angle()) -> angle().
supplementary(Angle) ->
    degrees(180.0 - to_degrees(normalize(Angle))).
