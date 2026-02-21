%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeCrypto - Cryptographic utilities for Prolog

:- module(proven_crypto, [
    constant_time_equals/2,
    simple_hash/2,
    bytes_to_hex/2,
    generate_token/2,
    random_int/3
]).

:- use_module(library(lists)).

%% constant_time_equals(+A, +B)
%% Succeeds if A and B are equal (constant-time comparison)
%% Prevents timing attacks by always comparing all bytes
constant_time_equals(A, B) :-
    atom_codes(A, CodesA),
    atom_codes(B, CodesB),
    length(CodesA, Len),
    length(CodesB, Len),
    constant_time_compare(CodesA, CodesB, 0, Diff),
    Diff =:= 0.

constant_time_compare([], [], Diff, Diff).
constant_time_compare([A|RestA], [B|RestB], Acc, Diff) :-
    NewAcc is Acc \/ (A xor B),
    constant_time_compare(RestA, RestB, NewAcc, Diff).

%% simple_hash(+Input, -Hash)
%% Compute FNV-1a hash of Input
simple_hash(Input, Hash) :-
    atom_codes(Input, Codes),
    fnv1a(Codes, 16'811c9dc5, Hash).

fnv1a([], Hash, Result) :-
    Result is Hash /\ 16'FFFFFFFF.
fnv1a([C|Rest], Hash, Result) :-
    H1 is (Hash xor C) * 16'01000193,
    H2 is H1 /\ 16'FFFFFFFF,
    fnv1a(Rest, H2, Result).

%% bytes_to_hex(+Bytes, -Hex)
%% Convert list of bytes to hexadecimal string
bytes_to_hex(Bytes, Hex) :-
    maplist(byte_to_hex, Bytes, HexPairs),
    append(HexPairs, HexCodes),
    atom_codes(Hex, HexCodes).

byte_to_hex(Byte, [H1, H2]) :-
    High is Byte >> 4,
    Low is Byte /\ 16'F,
    hex_digit(High, H1),
    hex_digit(Low, H2).

hex_digit(N, C) :- N < 10, !, C is N + 0'0.
hex_digit(N, C) :- C is N - 10 + 0'a.

%% generate_token(+Length, -Token)
%% Generate a random hexadecimal token
generate_token(Length, Token) :-
    HexChars = "0123456789abcdef",
    length(Codes, Length),
    maplist(random_hex_char(HexChars), Codes),
    atom_codes(Token, Codes).

random_hex_char(HexChars, Char) :-
    random_between(0, 15, Index),
    nth0(Index, HexChars, Char).

%% random_int(+Min, +Max, -Result)
%% Generate a random integer in range [Min, Max]
random_int(Min, Max, Result) :-
    Min >= Max,
    !,
    Result = Min.
random_int(Min, Max, Result) :-
    random_between(Min, Max, Result).
