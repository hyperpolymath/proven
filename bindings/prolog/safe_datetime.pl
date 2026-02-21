% SPDX-License-Identifier: PMPL-1.0-or-later
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeDatetime - Safe date/time handling for Prolog
% ISO 8601 compliant datetime operations.

:- module(safe_datetime, [
    datetime/7,
    datetime_now/1,
    parse_iso8601/2,
    format_iso8601/2,
    datetime_add_days/3,
    datetime_add_hours/3,
    datetime_add_minutes/3,
    datetime_add_seconds/3,
    datetime_diff_seconds/3,
    datetime_compare/3,
    is_valid_datetime/1,
    is_leap_year/1,
    days_in_month/3,
    day_of_week/2,
    unix_timestamp/2,
    timestamp_to_datetime/2,
    format_datetime/3
]).

:- use_module(library(lists)).

%! datetime(?Year, ?Month, ?Day, ?Hour, ?Minute, ?Second, ?Dt) is det.
%
%  Construct or deconstruct a datetime term.
%
%  @arg Year Year (e.g., 2025)
%  @arg Month Month (1-12)
%  @arg Day Day (1-31)
%  @arg Hour Hour (0-23)
%  @arg Minute Minute (0-59)
%  @arg Second Second (0-59)
%  @arg Dt datetime/6 structure
datetime(Year, Month, Day, Hour, Minute, Second,
         datetime(Year, Month, Day, Hour, Minute, Second)).

%! datetime_now(-Dt) is det.
%
%  Get current datetime.
%
%  @arg Dt Current datetime term
datetime_now(datetime(Year, Month, Day, Hour, Minute, Second)) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, date(Year, Month, Day, Hour, Minute, SecFloat, _, _, _), local),
    Second is floor(SecFloat).

%! parse_iso8601(+IsoString, -Dt) is semidet.
%
%  Parse ISO 8601 datetime string.
%  Accepts: YYYY-MM-DDTHH:MM:SS, YYYY-MM-DD, etc.
%
%  @arg IsoString ISO 8601 string
%  @arg Dt datetime/6 structure
parse_iso8601(IsoString, datetime(Year, Month, Day, Hour, Minute, Second)) :-
    atom_string(IsoString, Str),
    (   sub_string(Str, _, _, _, "T")
    ->  split_string(Str, "T", "", [DateStr, TimeStr]),
        parse_date(DateStr, Year, Month, Day),
        parse_time(TimeStr, Hour, Minute, Second)
    ;   parse_date(Str, Year, Month, Day),
        Hour = 0, Minute = 0, Second = 0
    ).

parse_date(DateStr, Year, Month, Day) :-
    split_string(DateStr, "-", "", Parts),
    Parts = [YearStr, MonthStr, DayStr],
    number_string(Year, YearStr),
    number_string(Month, MonthStr),
    number_string(Day, DayStr),
    Month >= 1, Month =< 12,
    days_in_month(Year, Month, MaxDay),
    Day >= 1, Day =< MaxDay.

parse_time(TimeStr, Hour, Minute, Second) :-
    (   sub_string(TimeStr, _, _, _, "Z")
    ->  sub_string(TimeStr, 0, _, 1, TimeStrClean)
    ;   TimeStrClean = TimeStr
    ),
    split_string(TimeStrClean, ":", "", Parts),
    (   Parts = [HourStr, MinuteStr, SecondStr]
    ->  true
    ;   Parts = [HourStr, MinuteStr], SecondStr = "0"
    ),
    number_string(Hour, HourStr),
    number_string(Minute, MinuteStr),
    number_string(Second, SecondStr),
    Hour >= 0, Hour =< 23,
    Minute >= 0, Minute =< 59,
    Second >= 0, Second =< 59.

%! format_iso8601(+Dt, -IsoString) is det.
%
%  Format datetime as ISO 8601 string.
%
%  @arg Dt datetime/6 structure
%  @arg IsoString ISO 8601 string
format_iso8601(datetime(Year, Month, Day, Hour, Minute, Second), IsoString) :-
    format(atom(IsoString), "~d-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+",
           [Year, Month, Day, Hour, Minute, Second]).

%! datetime_add_days(+Dt, +Days, -NewDt) is det.
%
%  Add days to datetime.
%
%  @arg Dt Original datetime
%  @arg Days Days to add (can be negative)
%  @arg NewDt Resulting datetime
datetime_add_days(Dt, Days, NewDt) :-
    datetime_add_seconds(Dt, Days * 86400, NewDt).

%! datetime_add_hours(+Dt, +Hours, -NewDt) is det.
%
%  Add hours to datetime.
%
%  @arg Dt Original datetime
%  @arg Hours Hours to add
%  @arg NewDt Resulting datetime
datetime_add_hours(Dt, Hours, NewDt) :-
    datetime_add_seconds(Dt, Hours * 3600, NewDt).

%! datetime_add_minutes(+Dt, +Minutes, -NewDt) is det.
%
%  Add minutes to datetime.
%
%  @arg Dt Original datetime
%  @arg Minutes Minutes to add
%  @arg NewDt Resulting datetime
datetime_add_minutes(Dt, Minutes, NewDt) :-
    datetime_add_seconds(Dt, Minutes * 60, NewDt).

%! datetime_add_seconds(+Dt, +Seconds, -NewDt) is det.
%
%  Add seconds to datetime.
%
%  @arg Dt Original datetime
%  @arg Seconds Seconds to add
%  @arg NewDt Resulting datetime
datetime_add_seconds(datetime(Y, Mo, D, H, Mi, S), Seconds,
                     datetime(NY, NMo, ND, NH, NMi, NS)) :-
    unix_timestamp(datetime(Y, Mo, D, H, Mi, S), Ts),
    NewTs is Ts + Seconds,
    timestamp_to_datetime(NewTs, datetime(NY, NMo, ND, NH, NMi, NS)).

%! datetime_diff_seconds(+Dt1, +Dt2, -Diff) is det.
%
%  Calculate difference in seconds between two datetimes.
%
%  @arg Dt1 First datetime
%  @arg Dt2 Second datetime
%  @arg Diff Difference in seconds (Dt2 - Dt1)
datetime_diff_seconds(Dt1, Dt2, Diff) :-
    unix_timestamp(Dt1, Ts1),
    unix_timestamp(Dt2, Ts2),
    Diff is Ts2 - Ts1.

%! datetime_compare(-Order, +Dt1, +Dt2) is det.
%
%  Compare two datetimes.
%  Order is one of: <, =, >.
%
%  @arg Order Comparison result
%  @arg Dt1 First datetime
%  @arg Dt2 Second datetime
datetime_compare(Order, Dt1, Dt2) :-
    unix_timestamp(Dt1, Ts1),
    unix_timestamp(Dt2, Ts2),
    compare(Order, Ts1, Ts2).

%! is_valid_datetime(+Dt) is semidet.
%
%  Validate a datetime term.
%
%  @arg Dt datetime/6 structure
is_valid_datetime(datetime(Year, Month, Day, Hour, Minute, Second)) :-
    integer(Year),
    Month >= 1, Month =< 12,
    days_in_month(Year, Month, MaxDay),
    Day >= 1, Day =< MaxDay,
    Hour >= 0, Hour =< 23,
    Minute >= 0, Minute =< 59,
    Second >= 0, Second =< 59.

%! is_leap_year(+Year) is semidet.
%
%  Check if year is a leap year.
%
%  @arg Year Year to check
is_leap_year(Year) :-
    (   Year mod 400 =:= 0
    ->  true
    ;   Year mod 100 =\= 0,
        Year mod 4 =:= 0
    ).

%! days_in_month(+Year, +Month, -Days) is det.
%
%  Get number of days in a month.
%
%  @arg Year Year (for leap year calculation)
%  @arg Month Month (1-12)
%  @arg Days Number of days
days_in_month(_, 1, 31).
days_in_month(Year, 2, Days) :-
    (   is_leap_year(Year) -> Days = 29 ; Days = 28 ).
days_in_month(_, 3, 31).
days_in_month(_, 4, 30).
days_in_month(_, 5, 31).
days_in_month(_, 6, 30).
days_in_month(_, 7, 31).
days_in_month(_, 8, 31).
days_in_month(_, 9, 30).
days_in_month(_, 10, 31).
days_in_month(_, 11, 30).
days_in_month(_, 12, 31).

%! day_of_week(+Dt, -DayOfWeek) is det.
%
%  Get day of week (0=Sunday, 6=Saturday).
%
%  @arg Dt datetime/6 structure
%  @arg DayOfWeek Day of week (0-6)
day_of_week(datetime(Year, Month, Day, _, _, _), DayOfWeek) :-
    % Zeller's congruence
    (   Month < 3
    ->  M is Month + 12, Y is Year - 1
    ;   M is Month, Y is Year
    ),
    K is Y mod 100,
    J is Y // 100,
    H is (Day + ((13 * (M + 1)) // 5) + K + (K // 4) + (J // 4) - 2 * J) mod 7,
    DayOfWeek is (H + 6) mod 7.

%! unix_timestamp(+Dt, -Timestamp) is det.
%
%  Convert datetime to Unix timestamp.
%
%  @arg Dt datetime/6 structure
%  @arg Timestamp Unix timestamp (seconds since 1970-01-01)
unix_timestamp(datetime(Year, Month, Day, Hour, Minute, Second), Timestamp) :-
    date_time_stamp(date(Year, Month, Day, Hour, Minute, Second, 0, -, -), Timestamp).

%! timestamp_to_datetime(+Timestamp, -Dt) is det.
%
%  Convert Unix timestamp to datetime.
%
%  @arg Timestamp Unix timestamp
%  @arg Dt datetime/6 structure
timestamp_to_datetime(Timestamp, datetime(Year, Month, Day, Hour, Minute, Second)) :-
    stamp_date_time(Timestamp, date(Year, Month, Day, Hour, Minute, SecFloat, _, _, _), 'UTC'),
    Second is floor(SecFloat).

%! format_datetime(+Dt, +Format, -Formatted) is det.
%
%  Format datetime with custom format string.
%  Supports: %Y (year), %m (month), %d (day), %H (hour), %M (minute), %S (second)
%
%  @arg Dt datetime/6 structure
%  @arg Format Format string
%  @arg Formatted Formatted string
format_datetime(datetime(Year, Month, Day, Hour, Minute, Second), Format, Formatted) :-
    atom_string(Format, FormatStr),
    format_tokens(FormatStr, Year, Month, Day, Hour, Minute, Second, FormattedStr),
    atom_string(Formatted, FormattedStr).

format_tokens(Format, Y, Mo, D, H, Mi, S, Result) :-
    (   sub_string(Format, Before, 2, After, "%Y")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(YStr), "~d", [Y]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, YStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   sub_string(Format, Before, 2, After, "%m")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(MoStr), "~|~`0t~d~2+", [Mo]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, MoStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   sub_string(Format, Before, 2, After, "%d")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(DStr), "~|~`0t~d~2+", [D]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, DStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   sub_string(Format, Before, 2, After, "%H")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(HStr), "~|~`0t~d~2+", [H]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, HStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   sub_string(Format, Before, 2, After, "%M")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(MiStr), "~|~`0t~d~2+", [Mi]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, MiStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   sub_string(Format, Before, 2, After, "%S")
    ->  sub_string(Format, 0, Before, _, Prefix),
        sub_string(Format, _, After, 0, Suffix),
        format(string(SStr), "~|~`0t~d~2+", [S]),
        format_tokens(Suffix, Y, Mo, D, H, Mi, S, SuffixResult),
        string_concat(Prefix, SStr, Temp),
        string_concat(Temp, SuffixResult, Result)
    ;   Result = Format
    ).
