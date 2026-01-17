% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeUUID - UUID validation and manipulation for Prolog
% Following RFC 4122 specification.

:- module(safe_uuid, [
    uuid/2,
    parse_uuid/2,
    format_uuid/2,
    is_valid_uuid/1,
    uuid_version/2,
    uuid_variant/2,
    uuid_nil/1,
    uuid_is_nil/1,
    uuid_to_urn/2,
    uuid_bytes/2
]).

:- use_module(library(lists)).

%% Namespace UUIDs (RFC 4122)
namespace_dns([0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
               0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]).
namespace_url([0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
               0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8]).

%! uuid(?Atom, ?Bytes) is semidet.
%
%  Bidirectional conversion between UUID atom and byte list.
%  Uuid is the canonical 36-character string representation.
%  Bytes is a list of 16 integers (0-255).
%
%  @arg Atom UUID in canonical string format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
%  @arg Bytes List of 16 bytes representing the UUID
uuid(Atom, Bytes) :-
    nonvar(Atom),
    !,
    parse_uuid(Atom, Bytes).
uuid(Atom, Bytes) :-
    nonvar(Bytes),
    !,
    format_uuid(Bytes, Atom).

%! parse_uuid(+UuidString, -Bytes) is semidet.
%
%  Parse a UUID string into a list of 16 bytes.
%  Accepts canonical format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
%
%  @arg UuidString UUID string (atom or string)
%  @arg Bytes List of 16 bytes
parse_uuid(UuidString, Bytes) :-
    atom_string(UuidString, UuidStr),
    string_length(UuidStr, 36),
    string_chars(UuidStr, Chars),
    Chars = [C0,C1,C2,C3,C4,C5,C6,C7,'-',
             C8,C9,C10,C11,'-',
             C12,C13,C14,C15,'-',
             C16,C17,C18,C19,'-',
             C20,C21,C22,C23,C24,C25,C26,C27,C28,C29,C30,C31],
    HexChars = [C0,C1,C2,C3,C4,C5,C6,C7,
                C8,C9,C10,C11,
                C12,C13,C14,C15,
                C16,C17,C18,C19,
                C20,C21,C22,C23,C24,C25,C26,C27,C28,C29,C30,C31],
    maplist(is_hex_char, HexChars),
    hex_chars_to_bytes(HexChars, Bytes),
    length(Bytes, 16).

%! format_uuid(+Bytes, -UuidAtom) is det.
%
%  Format a list of 16 bytes as a UUID string.
%
%  @arg Bytes List of 16 bytes
%  @arg UuidAtom UUID in canonical format
format_uuid(Bytes, UuidAtom) :-
    length(Bytes, 16),
    Bytes = [B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15],
    format(atom(UuidAtom),
           "~|~`0t~16r~2+-~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+-~|~`0t~16r~2+~|~`0t~16r~2+-~|~`0t~16r~2+~|~`0t~16r~2+-~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+",
           [B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15]).

%! is_valid_uuid(+UuidString) is semidet.
%
%  Check if UuidString is a valid UUID.
%
%  @arg UuidString UUID string to validate
is_valid_uuid(UuidString) :-
    catch(parse_uuid(UuidString, _), _, fail).

%! uuid_version(+Bytes, -Version) is det.
%
%  Extract the version from UUID bytes.
%  Version is one of: v1, v2, v3, v4, v5, nil, unknown.
%
%  @arg Bytes List of 16 UUID bytes
%  @arg Version Version atom
uuid_version(Bytes, Version) :-
    nth0(6, Bytes, B6),
    VersionNum is (B6 >> 4) /\ 0x0F,
    version_atom(VersionNum, Version).

version_atom(1, v1) :- !.
version_atom(2, v2) :- !.
version_atom(3, v3) :- !.
version_atom(4, v4) :- !.
version_atom(5, v5) :- !.
version_atom(0, nil) :- !.
version_atom(_, unknown).

%! uuid_variant(+Bytes, -Variant) is det.
%
%  Extract the variant from UUID bytes.
%  Variant is one of: ncs, rfc4122, microsoft, future.
%
%  @arg Bytes List of 16 UUID bytes
%  @arg Variant Variant atom
uuid_variant(Bytes, Variant) :-
    nth0(8, Bytes, B8),
    ( B8 >> 7 =:= 0 -> Variant = ncs
    ; B8 >> 6 =:= 0b10 -> Variant = rfc4122
    ; B8 >> 5 =:= 0b110 -> Variant = microsoft
    ; Variant = future
    ).

%! uuid_nil(-Bytes) is det.
%
%  Unify Bytes with the nil UUID (all zeros).
%
%  @arg Bytes The nil UUID as 16 bytes
uuid_nil([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).

%! uuid_is_nil(+Bytes) is semidet.
%
%  Check if UUID bytes represent the nil UUID.
%
%  @arg Bytes List of 16 UUID bytes
uuid_is_nil(Bytes) :-
    length(Bytes, 16),
    forall(member(B, Bytes), B =:= 0).

%! uuid_to_urn(+UuidString, -Urn) is det.
%
%  Convert UUID string to URN format.
%
%  @arg UuidString UUID in canonical format
%  @arg Urn UUID in URN format (urn:uuid:...)
uuid_to_urn(UuidString, Urn) :-
    atom_string(UuidString, UuidStr),
    string_lower(UuidStr, LowerStr),
    format(atom(Urn), "urn:uuid:~s", [LowerStr]).

%! uuid_bytes(+UuidString, -Bytes) is det.
%
%  Extract bytes from UUID string (alias for parse_uuid).
%
%  @arg UuidString UUID in canonical format
%  @arg Bytes List of 16 bytes
uuid_bytes(UuidString, Bytes) :-
    parse_uuid(UuidString, Bytes).

%% Helper predicates

%! is_hex_char(+Char) is semidet.
%
%  Check if character is valid hexadecimal.
is_hex_char(C) :-
    char_code(C, Code),
    ( Code >= 0'0, Code =< 0'9 -> true
    ; Code >= 0'a, Code =< 0'f -> true
    ; Code >= 0'A, Code =< 0'F -> true
    ).

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
