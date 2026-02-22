%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeCrypto - FFI bindings to libproven cryptographic operations.
%% All crypto operations are performed in verified Idris 2 code via libproven.

:- module(proven_crypto, [
    constant_time_equals/2,
    bytes_to_hex/2,
    generate_token/2,
    random_int/3
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations
:- foreign(proven_crypto_constant_time_eq_ffi, c,
           proven_crypto_constant_time_eq(+string, +integer,
                                          +string, +integer,
                                          [-integer], [-integer])).
:- foreign(proven_hex_encode_ffi, c,
           proven_hex_encode(+string, +integer, +integer, [-integer], [-string])).
:- foreign(proven_crypto_random_bytes_ffi, c,
           proven_crypto_random_bytes(+integer, [-string], [-integer])).

%! constant_time_equals(+A, +B) is semidet.
%
%  Constant-time byte comparison via libproven (timing-attack safe).
constant_time_equals(A, B) :-
    atom_string(A, AStr),
    atom_string(B, BStr),
    atom_length(A, LenA),
    atom_length(B, LenB),
    proven_crypto_constant_time_eq_ffi(AStr, LenA, BStr, LenB, Status, Value),
    Status =:= 0,
    Value =:= 1.

%! bytes_to_hex(+Bytes, -Hex) is det.
%
%  Convert list of bytes to hexadecimal string via libproven.
bytes_to_hex(Bytes, Hex) :-
    length(Bytes, Len),
    atom_codes(ByteAtom, Bytes),
    atom_string(ByteAtom, ByteStr),
    proven_hex_encode_ffi(ByteStr, Len, 0, Status, HexStr),
    (   Status =:= 0
    ->  atom_string(Hex, HexStr)
    ;   Hex = ''
    ).

%! generate_token(+Length, -Token) is det.
%
%  Generate a random hexadecimal token via libproven.
%  Uses cryptographically secure random bytes from libproven.
generate_token(Length, Token) :-
    ByteLen is (Length + 1) // 2,
    proven_crypto_random_bytes_ffi(ByteLen, BytesStr, Status),
    (   Status =:= 0
    ->  proven_hex_encode_ffi(BytesStr, ByteLen, 0, HStatus, HexStr),
        (   HStatus =:= 0
        ->  atom_string(FullToken, HexStr),
            atom_length(FullToken, FullLen),
            (   FullLen > Length
            ->  sub_atom(FullToken, 0, Length, _, Token)
            ;   Token = FullToken
            )
        ;   Token = ''
        )
    ;   Token = ''
    ).

%! random_int(+Min, +Max, -Result) is det.
%
%  Generate a random integer in range [Min, Max] via libproven.
%  Uses proven_crypto_random_bytes for the entropy source.
random_int(Min, Max, Result) :-
    (   Min >= Max
    ->  Result = Min
    ;   Range is Max - Min + 1,
        proven_crypto_random_bytes_ffi(8, BytesStr, Status),
        (   Status =:= 0
        ->  atom_codes(BytesStr, Codes),
            codes_to_int(Codes, 0, RawInt),
            AbsInt is abs(RawInt),
            Result is Min + (AbsInt mod Range)
        ;   Result = Min
        )
    ).

%% Helper: convert byte codes to an integer
codes_to_int([], Acc, Acc).
codes_to_int([C|Rest], Acc, Result) :-
    NewAcc is (Acc << 8) \/ C,
    codes_to_int(Rest, NewAcc, Result).
