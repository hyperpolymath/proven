%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeCrypto - Extended FFI bindings to libproven crypto operations.
%% All crypto operations are performed in verified Idris 2 code via libproven.

:- module(safe_crypto, [
    constant_time_equals/2,
    bytes_to_hex/2,
    generate_token/2,
    random_int/3
]).

:- use_module(proven_crypto, [
    constant_time_equals/2 as pc_constant_time_equals,
    bytes_to_hex/2 as pc_bytes_to_hex,
    generate_token/2 as pc_generate_token,
    random_int/3 as pc_random_int
]).

%! constant_time_equals(+A, +B) is semidet.
%
%  Constant-time byte comparison via libproven (timing-attack safe).
constant_time_equals(A, B) :-
    pc_constant_time_equals(A, B).

%! bytes_to_hex(+Bytes, -Hex) is det.
%
%  Convert list of bytes to hexadecimal string via libproven.
bytes_to_hex(Bytes, Hex) :-
    pc_bytes_to_hex(Bytes, Hex).

%! generate_token(+Length, -Token) is det.
%
%  Generate a random hexadecimal token via libproven.
generate_token(Length, Token) :-
    pc_generate_token(Length, Token).

%! random_int(+Min, +Max, -Result) is det.
%
%  Generate a random integer in range [Min, Max] via libproven.
random_int(Min, Max, Result) :-
    pc_random_int(Min, Max, Result).
