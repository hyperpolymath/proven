%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven SafeDatetime - FFI bindings to libproven datetime operations.
%% All datetime operations are performed in verified Idris 2 code via libproven.

:- module(safe_datetime, [
    parse_iso8601/2,
    format_iso8601/2,
    is_leap_year/1,
    days_in_month/3
]).

:- use_foreign_library(foreign(libproven)).

%% FFI declarations for libproven datetime functions.
:- foreign(proven_datetime_parse_ffi, c,
           proven_datetime_parse(+string, +integer,
                                 [-integer],
                                 [-integer], [-integer], [-integer],
                                 [-integer], [-integer], [-integer],
                                 [-integer], [-integer])).
:- foreign(proven_datetime_format_iso8601_ffi, c,
           proven_datetime_format_iso8601(+integer, +integer, +integer,
                                          +integer, +integer, +integer,
                                          +integer, +integer,
                                          [-integer], [-string])).
:- foreign(proven_datetime_is_leap_year_ffi, c,
           proven_datetime_is_leap_year(+integer, [-integer])).
:- foreign(proven_datetime_days_in_month_ffi, c,
           proven_datetime_days_in_month(+integer, +integer, [-integer])).

%! parse_iso8601(+IsoString, -Result) is det.
%
%  Parse ISO 8601 datetime string via libproven.
%  Result is ok(datetime(Year, Month, Day, Hour, Minute, Second)) or
%  error(parse_failure).
parse_iso8601(IsoString, Result) :-
    atom_string(IsoString, Str),
    atom_length(IsoString, Len),
    proven_datetime_parse_ffi(Str, Len, Status,
                              Year, Month, Day,
                              Hour, Minute, Second,
                              _Nanosecond, _TzOffsetMinutes),
    (   Status =:= 0
    ->  Result = ok(datetime(Year, Month, Day, Hour, Minute, Second))
    ;   Result = error(parse_failure)
    ).

%! format_iso8601(+Datetime, -IsoString) is det.
%
%  Format datetime as ISO 8601 string via libproven.
format_iso8601(datetime(Year, Month, Day, Hour, Minute, Second), IsoString) :-
    proven_datetime_format_iso8601_ffi(Year, Month, Day,
                                       Hour, Minute, Second,
                                       0, 0,
                                       Status, ResultStr),
    (   Status =:= 0
    ->  atom_string(IsoString, ResultStr)
    ;   IsoString = ''
    ).

%! is_leap_year(+Year) is semidet.
%
%  Check if year is a leap year via libproven.
is_leap_year(Year) :-
    proven_datetime_is_leap_year_ffi(Year, Value),
    Value =:= 1.

%! days_in_month(+Year, +Month, -Days) is det.
%
%  Get number of days in a month via libproven.
%  Returns 0 if month is invalid.
days_in_month(Year, Month, Days) :-
    proven_datetime_days_in_month_ffi(Year, Month, Days).
