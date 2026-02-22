%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeHex - FFI bindings to libproven hexadecimal operations.
%% All hex encoding/decoding is performed in verified Idris 2 code via libproven.

:- module(safe_hex, [
    hex_encode/2,
    hex_encode_upper/2,
    hex_decode/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven hex functions.
:- foreign(proven_hex_encode_ffi, c,
           proven_hex_encode(+string, +integer, +integer, [-integer], [-string])).
:- foreign(proven_hex_decode_ffi, c,
           proven_hex_decode(+string, +integer, [-integer], [-string], [-integer])).

%! hex_encode(+Bytes, -HexString) is det.
%
%  Encode bytes to lowercase hexadecimal string via libproven.
hex_encode(Bytes, HexString) :-
    length(Bytes, Len),
    atom_codes(ByteAtom, Bytes),
    atom_string(ByteAtom, ByteStr),
    proven_hex_encode_ffi(ByteStr, Len, 0, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(HexString, ResultStr)
    ;   HexString = ''
    ).

%! hex_encode_upper(+Bytes, -HexString) is det.
%
%  Encode bytes to uppercase hexadecimal string via libproven.
hex_encode_upper(Bytes, HexString) :-
    length(Bytes, Len),
    atom_codes(ByteAtom, Bytes),
    atom_string(ByteAtom, ByteStr),
    proven_hex_encode_ffi(ByteStr, Len, 1, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(HexString, ResultStr)
    ;   HexString = ''
    ).

%! hex_decode(+HexString, -Bytes) is semidet.
%
%  Decode hexadecimal string to bytes via libproven.
%  Fails if string has invalid hex characters.
hex_decode(HexString, Bytes) :-
    atom_string(HexString, Str),
    atom_length(HexString, Len),
    proven_hex_decode_ffi(Str, Len, Status, ResultStr, ResultLen),
    Status =:= 0,
    atom_codes(ResultAtom, _),
    atom_string(ResultAtom, ResultStr),
    atom_codes(ResultAtom, Bytes),
    length(Bytes, ResultLen).
