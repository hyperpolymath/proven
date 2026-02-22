%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeUUID - FFI bindings to libproven UUID operations.
%% All UUID operations are performed in verified Idris 2 code via libproven.

:- module(safe_uuid, [
    uuid_v4/1,
    uuid_parse/2,
    uuid_to_string/2,
    uuid_is_nil/1,
    uuid_version/2
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven UUID functions.
%% ProvenUUID is 16 bytes; we pass as a string buffer.
:- foreign(proven_uuid_v4_ffi, c,
           proven_uuid_v4([-integer], [-string])).
:- foreign(proven_uuid_to_string_ffi, c,
           proven_uuid_to_string(+string, [-integer], [-string])).
:- foreign(proven_uuid_parse_ffi, c,
           proven_uuid_parse(+string, +integer, [-integer], [-string])).
:- foreign(proven_uuid_is_nil_ffi, c,
           proven_uuid_is_nil(+string, [-integer])).
:- foreign(proven_uuid_version_ffi, c,
           proven_uuid_version(+string, [-integer])).

%! uuid_v4(-UuidString) is det.
%
%  Generate a UUID v4 (random) via libproven.
%  Returns the canonical 36-character string representation.
uuid_v4(UuidString) :-
    proven_uuid_v4_ffi(Status, UuidBytes),
    (   Status =:= 0
    ->  proven_uuid_to_string_ffi(UuidBytes, StrStatus, ResultStr),
        (   StrStatus =:= 0
        ->  atom_string(UuidString, ResultStr)
        ;   UuidString = ''
        )
    ;   UuidString = ''
    ).

%! uuid_parse(+UuidString, -UuidBytes) is semidet.
%
%  Parse UUID from string via libproven.
%  UuidBytes is the internal 16-byte representation.
uuid_parse(UuidString, UuidBytes) :-
    atom_string(UuidString, Str),
    atom_length(UuidString, Len),
    proven_uuid_parse_ffi(Str, Len, Status, UuidBytes),
    Status =:= 0.

%! uuid_to_string(+UuidBytes, -UuidString) is det.
%
%  Format UUID bytes as canonical string via libproven.
uuid_to_string(UuidBytes, UuidString) :-
    proven_uuid_to_string_ffi(UuidBytes, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(UuidString, ResultStr)
    ;   UuidString = ''
    ).

%! uuid_is_nil(+UuidBytes) is semidet.
%
%  Check if UUID is nil (all zeros) via libproven.
uuid_is_nil(UuidBytes) :-
    proven_uuid_is_nil_ffi(UuidBytes, Value),
    Value =:= 1.

%! uuid_version(+UuidBytes, -Version) is det.
%
%  Get UUID version via libproven.
uuid_version(UuidBytes, Version) :-
    proven_uuid_version_ffi(UuidBytes, Version).
