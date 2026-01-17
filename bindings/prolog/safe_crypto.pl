% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeCrypto - Cryptographic utilities for Prolog
% Provides secure comparison, hashing, and random generation.

:- module(safe_crypto, [
    constant_time_equals/2,
    simple_hash/2,
    fnv1a_hash/2,
    djb2_hash/2,
    bytes_to_hex/2,
    hex_to_bytes/2,
    generate_token/2,
    random_int/3,
    random_bytes/2,
    secure_compare/3,
    xor_bytes/3
]).

:- use_module(library(lists)).

%! constant_time_equals(+A, +B) is semidet.
%
%  Constant-time string comparison (timing-attack resistant).
%  Compares all bytes regardless of early mismatches.
%
%  @arg A First string/atom
%  @arg B Second string/atom
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

%! simple_hash(+Input, -Hash) is det.
%
%  Compute FNV-1a hash (alias for fnv1a_hash).
%
%  @arg Input Input string
%  @arg Hash 32-bit hash value
simple_hash(Input, Hash) :-
    fnv1a_hash(Input, Hash).

%! fnv1a_hash(+Input, -Hash) is det.
%
%  Compute FNV-1a 32-bit hash.
%
%  @arg Input Input string
%  @arg Hash 32-bit hash value
fnv1a_hash(Input, Hash) :-
    atom_codes(Input, Codes),
    fnv1a(Codes, 16'811c9dc5, Hash).

fnv1a([], Hash, Result) :-
    Result is Hash /\ 16'FFFFFFFF.
fnv1a([C|Rest], Hash, Result) :-
    H1 is (Hash xor C) * 16'01000193,
    H2 is H1 /\ 16'FFFFFFFF,
    fnv1a(Rest, H2, Result).

%! djb2_hash(+Input, -Hash) is det.
%
%  Compute DJB2 hash.
%
%  @arg Input Input string
%  @arg Hash 32-bit hash value
djb2_hash(Input, Hash) :-
    atom_codes(Input, Codes),
    djb2(Codes, 5381, Hash).

djb2([], Hash, Result) :-
    Result is Hash /\ 16'FFFFFFFF.
djb2([C|Rest], Hash, Result) :-
    H1 is ((Hash << 5) + Hash) + C,
    H2 is H1 /\ 16'FFFFFFFF,
    djb2(Rest, H2, Result).

%! bytes_to_hex(+Bytes, -Hex) is det.
%
%  Convert list of bytes to hexadecimal string.
%
%  @arg Bytes List of bytes (0-255)
%  @arg Hex Hexadecimal string
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

%! hex_to_bytes(+Hex, -Bytes) is semidet.
%
%  Convert hexadecimal string to list of bytes.
%
%  @arg Hex Hexadecimal string
%  @arg Bytes List of bytes
hex_to_bytes(Hex, Bytes) :-
    atom_codes(Hex, Codes),
    length(Codes, Len),
    Len mod 2 =:= 0,
    hex_codes_to_bytes(Codes, Bytes).

hex_codes_to_bytes([], []).
hex_codes_to_bytes([H1, H2|Rest], [Byte|Bytes]) :-
    hex_value(H1, High),
    hex_value(H2, Low),
    Byte is (High << 4) \/ Low,
    hex_codes_to_bytes(Rest, Bytes).

hex_value(C, V) :- C >= 0'0, C =< 0'9, !, V is C - 0'0.
hex_value(C, V) :- C >= 0'a, C =< 0'f, !, V is C - 0'a + 10.
hex_value(C, V) :- C >= 0'A, C =< 0'F, V is C - 0'A + 10.

%! generate_token(+Length, -Token) is det.
%
%  Generate a random hexadecimal token.
%
%  @arg Length Length of token in characters
%  @arg Token Random hex token
generate_token(Length, Token) :-
    HexChars = "0123456789abcdef",
    length(Codes, Length),
    maplist(random_hex_char(HexChars), Codes),
    atom_codes(Token, Codes).

random_hex_char(HexChars, Char) :-
    random_between(0, 15, Index),
    nth0(Index, HexChars, Char).

%! random_int(+Min, +Max, -Result) is det.
%
%  Generate a random integer in range [Min, Max].
%
%  @arg Min Minimum value (inclusive)
%  @arg Max Maximum value (inclusive)
%  @arg Result Random integer
random_int(Min, Max, Result) :-
    Min >= Max,
    !,
    Result = Min.
random_int(Min, Max, Result) :-
    random_between(Min, Max, Result).

%! random_bytes(+Count, -Bytes) is det.
%
%  Generate list of random bytes.
%
%  @arg Count Number of bytes to generate
%  @arg Bytes List of random bytes (0-255)
random_bytes(Count, Bytes) :-
    length(Bytes, Count),
    maplist(random_byte, Bytes).

random_byte(Byte) :-
    random_between(0, 255, Byte).

%! secure_compare(+A, +B, -Result) is det.
%
%  Secure comparison returning a result term.
%  Result is equal or not_equal.
%
%  @arg A First value
%  @arg B Second value
%  @arg Result equal or not_equal
secure_compare(A, B, Result) :-
    (   constant_time_equals(A, B)
    ->  Result = equal
    ;   Result = not_equal
    ).

%! xor_bytes(+BytesA, +BytesB, -Result) is det.
%
%  XOR two byte lists together.
%
%  @arg BytesA First byte list
%  @arg BytesB Second byte list
%  @arg Result XOR result
xor_bytes([], [], []).
xor_bytes([A|RestA], [B|RestB], [R|RestR]) :-
    R is A xor B,
    xor_bytes(RestA, RestB, RestR).
