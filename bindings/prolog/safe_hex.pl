%% SPDX-License-Identifier: AGPL-3.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeHex - Hexadecimal encoding/decoding for Prolog
%% With constant-time comparison for security.

:- module(safe_hex, [
    hex_encode/2,
    hex_decode/2,
    hex_encode_upper/2,
    constant_time_equal/2,
    is_hex_string/1,
    is_valid_hex_bytes/1,
    hex_format_spaced/2,
    hex_format_colons/2,
    int_to_hex/2,
    int_to_hex/3,
    hex_to_int/2
]).

:- use_module(library(lists)).

%! hex_encode(+Bytes, -HexString) is det.
%
%  Encode a list of bytes to lowercase hexadecimal string.
%
%  @arg Bytes List of integers (0-255)
%  @arg HexString Lowercase hex string
hex_encode(Bytes, HexString) :-
    maplist(byte_to_hex_chars, Bytes, CharLists),
    append(CharLists, AllChars),
    atom_chars(HexString, AllChars).

%! hex_encode_upper(+Bytes, -HexString) is det.
%
%  Encode a list of bytes to uppercase hexadecimal string.
%
%  @arg Bytes List of integers (0-255)
%  @arg HexString Uppercase hex string
hex_encode_upper(Bytes, HexString) :-
    maplist(byte_to_hex_chars_upper, Bytes, CharLists),
    append(CharLists, AllChars),
    atom_chars(HexString, AllChars).

%! hex_decode(+HexString, -Bytes) is semidet.
%
%  Decode a hexadecimal string to a list of bytes.
%  Fails if the string has odd length or invalid characters.
%
%  @arg HexString Hex string (case-insensitive)
%  @arg Bytes List of integers (0-255)
hex_decode(HexString, Bytes) :-
    atom_string(HexString, HexStr),
    string_length(HexStr, Len),
    Len mod 2 =:= 0,
    string_chars(HexStr, Chars),
    hex_chars_to_bytes(Chars, Bytes).

%! constant_time_equal(+A, +B) is semidet.
%
%  Constant-time comparison of two strings/atoms.
%  Prevents timing attacks by always comparing all bytes.
%  Both arguments must be the same length for success.
%
%  @arg A First string/atom to compare
%  @arg B Second string/atom to compare
constant_time_equal(A, B) :-
    atom_codes(A, CodesA),
    atom_codes(B, CodesB),
    length(CodesA, Len),
    length(CodesB, Len),
    constant_time_compare_codes(CodesA, CodesB, 0, Diff),
    Diff =:= 0.

%% Accumulate XOR differences without short-circuiting
constant_time_compare_codes([], [], Diff, Diff).
constant_time_compare_codes([A|RestA], [B|RestB], Acc, Diff) :-
    NewAcc is Acc \/ (A xor B),
    constant_time_compare_codes(RestA, RestB, NewAcc, Diff).

%! is_hex_string(+String) is semidet.
%
%  Check if string contains only valid hexadecimal characters.
%
%  @arg String String to validate
is_hex_string(String) :-
    atom_string(String, Str),
    string_chars(Str, Chars),
    forall(member(C, Chars), is_hex_char(C)).

%! is_valid_hex_bytes(+String) is semidet.
%
%  Check if string is valid hex with even length (represents bytes).
%
%  @arg String String to validate
is_valid_hex_bytes(String) :-
    atom_string(String, Str),
    string_length(Str, Len),
    Len mod 2 =:= 0,
    is_hex_string(String).

%! hex_format_spaced(+HexString, -SpacedHex) is det.
%
%  Format hex string with spaces between byte pairs.
%  Example: "aabbcc" -> "aa bb cc"
%
%  @arg HexString Input hex string
%  @arg SpacedHex Space-separated hex string
hex_format_spaced(HexString, SpacedHex) :-
    atom_string(HexString, HexStr),
    string_length(HexStr, Len),
    ( Len mod 2 =:= 0 ->
        string_chars(HexStr, Chars),
        group_pairs(Chars, Pairs),
        atomics_to_string(Pairs, " ", SpacedHex)
    ;
        SpacedHex = error
    ).

%! hex_format_colons(+HexString, -ColonHex) is det.
%
%  Format hex string with colons between byte pairs.
%  Example: "aabbcc" -> "aa:bb:cc"
%
%  @arg HexString Input hex string
%  @arg ColonHex Colon-separated hex string
hex_format_colons(HexString, ColonHex) :-
    atom_string(HexString, HexStr),
    string_length(HexStr, Len),
    ( Len mod 2 =:= 0 ->
        string_chars(HexStr, Chars),
        group_pairs(Chars, Pairs),
        atomics_to_string(Pairs, ":", ColonHex)
    ;
        ColonHex = error
    ).

%! int_to_hex(+Integer, -HexString) is det.
%
%  Convert non-negative integer to lowercase hex string.
%
%  @arg Integer Non-negative integer
%  @arg HexString Hexadecimal string representation
int_to_hex(Integer, HexString) :-
    format(atom(HexString), "~16r", [Integer]).

%! int_to_hex(+Integer, +MinWidth, -HexString) is det.
%
%  Convert integer to hex string with minimum width (zero-padded).
%
%  @arg Integer Non-negative integer
%  @arg MinWidth Minimum width (will be zero-padded)
%  @arg HexString Hexadecimal string representation
int_to_hex(Integer, MinWidth, HexString) :-
    format(atom(Raw), "~16r", [Integer]),
    atom_length(Raw, Len),
    ( Len >= MinWidth ->
        HexString = Raw
    ;
        PadLen is MinWidth - Len,
        length(ZeroChars, PadLen),
        maplist(=('0'), ZeroChars),
        atom_chars(Zeros, ZeroChars),
        atom_concat(Zeros, Raw, HexString)
    ).

%! hex_to_int(+HexString, -Integer) is semidet.
%
%  Parse hex string to integer.
%
%  @arg HexString Hexadecimal string
%  @arg Integer Parsed integer value
hex_to_int(HexString, Integer) :-
    atom_string(HexString, HexStr),
    string_lower(HexStr, LowerStr),
    atom_string(LowerAtom, LowerStr),
    atom_codes(LowerAtom, Codes),
    forall(member(C, Codes), is_hex_code(C)),
    read_term_from_atom(LowerAtom, Term, [syntax_errors(quiet)]),
    ( integer(Term) ->
        Integer = Term
    ;
        atom_concat('0x', LowerAtom, HexAtom),
        term_to_atom(Integer, HexAtom)
    ).

%% Helper predicates

%! is_hex_char(+Char) is semidet.
%
%  Check if character is valid hexadecimal.
is_hex_char(C) :-
    char_code(C, Code),
    is_hex_code(Code).

%! is_hex_code(+Code) is semidet.
%
%  Check if character code is valid hexadecimal.
is_hex_code(Code) :-
    ( Code >= 0'0, Code =< 0'9 -> true
    ; Code >= 0'a, Code =< 0'f -> true
    ; Code >= 0'A, Code =< 0'F -> true
    ).

%! nibble_to_hex_char(+Nibble, -Char) is det.
%
%  Convert 4-bit value to lowercase hex character.
nibble_to_hex_char(N, C) :-
    ( N < 10 ->
        Code is N + 0'0
    ;
        Code is N - 10 + 0'a
    ),
    char_code(C, Code).

%! nibble_to_hex_char_upper(+Nibble, -Char) is det.
%
%  Convert 4-bit value to uppercase hex character.
nibble_to_hex_char_upper(N, C) :-
    ( N < 10 ->
        Code is N + 0'0
    ;
        Code is N - 10 + 0'A
    ),
    char_code(C, Code).

%! byte_to_hex_chars(+Byte, -Chars) is det.
%
%  Convert byte to two lowercase hex characters.
byte_to_hex_chars(Byte, [H, L]) :-
    High is (Byte >> 4) /\ 0xF,
    Low is Byte /\ 0xF,
    nibble_to_hex_char(High, H),
    nibble_to_hex_char(Low, L).

%! byte_to_hex_chars_upper(+Byte, -Chars) is det.
%
%  Convert byte to two uppercase hex characters.
byte_to_hex_chars_upper(Byte, [H, L]) :-
    High is (Byte >> 4) /\ 0xF,
    Low is Byte /\ 0xF,
    nibble_to_hex_char_upper(High, H),
    nibble_to_hex_char_upper(Low, L).

%! hex_char_to_nibble(+Char, -Nibble) is det.
%
%  Convert hex character to 4-bit value.
hex_char_to_nibble(C, N) :-
    char_code(C, Code),
    ( Code >= 0'0, Code =< 0'9 -> N is Code - 0'0
    ; Code >= 0'a, Code =< 0'f -> N is Code - 0'a + 10
    ; Code >= 0'A, Code =< 0'F -> N is Code - 0'A + 10
    ).

%! hex_chars_to_bytes(+HexChars, -Bytes) is det.
%
%  Convert list of hex characters to byte list.
hex_chars_to_bytes([], []).
hex_chars_to_bytes([H1, H2 | Rest], [Byte | Bytes]) :-
    hex_char_to_nibble(H1, High),
    hex_char_to_nibble(H2, Low),
    Byte is (High << 4) \/ Low,
    hex_chars_to_bytes(Rest, Bytes).

%! group_pairs(+List, -Pairs) is det.
%
%  Group list elements into pairs as atoms.
group_pairs([], []).
group_pairs([A, B | Rest], [Pair | Pairs]) :-
    atom_chars(Pair, [A, B]),
    group_pairs(Rest, Pairs).
