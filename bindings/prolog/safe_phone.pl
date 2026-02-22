%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafePhone - FFI bindings to libproven phone number operations.
%% All phone number parsing is performed in verified Idris 2 code via libproven.

:- module(safe_phone, [
    parse_phone/2,
    format_phone_e164/3,
    is_valid_phone/1
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven phone functions.
:- foreign(proven_phone_parse_ffi, c,
           proven_phone_parse(+string, +integer,
                              [-integer], [-integer], [-integer], [-integer])).
:- foreign(proven_phone_format_e164_ffi, c,
           proven_phone_format_e164(+integer, +integer,
                                    [-integer], [-string])).

%! parse_phone(+Input, -Result) is det.
%
%  Parse phone number to E.164 format via libproven.
%  Result is ok(phone(CountryCode, NationalNumber)) or error(parse_failure).
parse_phone(Input, Result) :-
    atom_string(Input, Str),
    atom_length(Input, Len),
    proven_phone_parse_ffi(Str, Len, Status, CountryCode, NationalNumber, IsValid),
    (   Status =:= 0, IsValid =:= 1
    ->  Result = ok(phone(CountryCode, NationalNumber))
    ;   Result = error(parse_failure)
    ).

%! format_phone_e164(+CountryCode, +NationalNumber, -Formatted) is det.
%
%  Format phone number as E.164 string via libproven.
format_phone_e164(CountryCode, NationalNumber, Formatted) :-
    proven_phone_format_e164_ffi(CountryCode, NationalNumber, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Formatted, ResultStr)
    ;   Formatted = ''
    ).

%! is_valid_phone(+Input) is semidet.
%
%  Check if input is a valid phone number via libproven.
is_valid_phone(Input) :-
    parse_phone(Input, ok(_)).
