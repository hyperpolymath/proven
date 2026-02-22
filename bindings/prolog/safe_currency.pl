%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeCurrency - FFI bindings to libproven currency operations.
%% All currency parsing and formatting is performed in verified Idris 2 code
%% via libproven.

:- module(safe_currency, [
    parse_currency/2,
    format_currency/4
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven currency functions.
:- foreign(proven_currency_parse_ffi, c,
           proven_currency_parse(+string, +integer,
                                 [-integer], [-integer],
                                 [-string], [-integer])).
:- foreign(proven_currency_format_ffi, c,
           proven_currency_format(+integer, +string, +integer,
                                  [-integer], [-string])).

%! parse_currency(+Input, -Result) is det.
%
%  Parse currency amount string via libproven (e.g., "USD 123.45").
%  Result is ok(currency(AmountMinor, Code, DecimalPlaces)) or error(parse_failure).
parse_currency(Input, Result) :-
    atom_string(Input, Str),
    atom_length(Input, Len),
    proven_currency_parse_ffi(Str, Len, Status, AmountMinor, Code, DecimalPlaces),
    (   Status =:= 0
    ->  atom_string(CodeAtom, Code),
        Result = ok(currency(AmountMinor, CodeAtom, DecimalPlaces))
    ;   Result = error(parse_failure)
    ).

%! format_currency(+AmountMinor, +Code, +DecimalPlaces, -Formatted) is det.
%
%  Format currency amount via libproven.
%  AmountMinor is the amount in minor units (cents, etc.).
format_currency(AmountMinor, Code, DecimalPlaces, Formatted) :-
    atom_string(Code, CodeStr),
    proven_currency_format_ffi(AmountMinor, CodeStr, DecimalPlaces, Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(Formatted, ResultStr)
    ;   Formatted = ''
    ).
