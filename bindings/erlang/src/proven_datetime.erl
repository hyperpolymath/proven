%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeDateTime - Date and time validation for Erlang

-module(proven_datetime).

-export([
    is_leap_year/1,
    days_in_month/2,
    validate_date/3,
    validate_time/3,
    validate_time/4,
    parse_iso_date/1,
    parse_iso_time/1,
    parse_iso_datetime/1,
    format_iso_date/1,
    format_iso_time/1,
    format_iso_datetime/2,
    day_of_week/3,
    days_between/2,
    add_days/2,
    now_utc/0
]).

-export_type([date/0, time/0]).

%% Date record
-record(date, {
    year :: integer(),
    month :: 1..12,
    day :: 1..31
}).

%% Time record
-record(time, {
    hour :: 0..23,
    minute :: 0..59,
    second :: 0..59,
    nanos :: non_neg_integer()
}).

-type date() :: #date{}.
-type time() :: #time{}.

%% @doc Check if a year is a leap year.
-spec is_leap_year(integer()) -> boolean().
is_leap_year(Year) ->
    (Year rem 4 == 0 andalso Year rem 100 /= 0) orelse (Year rem 400 == 0).

%% @doc Get the number of days in a month.
-spec days_in_month(integer(), 1..12) -> 28..31 | undefined.
days_in_month(_, Month) when Month < 1; Month > 12 ->
    undefined;
days_in_month(Year, 2) ->
    case is_leap_year(Year) of
        true -> 29;
        false -> 28
    end;
days_in_month(_, Month) when Month == 4; Month == 6; Month == 9; Month == 11 ->
    30;
days_in_month(_, _) ->
    31.

%% @doc Validate a date.
-spec validate_date(integer(), integer(), integer()) -> {ok, date()} | {error, atom()}.
validate_date(Year, Month, Day) when Month < 1; Month > 12 ->
    {error, invalid_month};
validate_date(Year, Month, Day) ->
    MaxDay = days_in_month(Year, Month),
    case Day >= 1 andalso Day =< MaxDay of
        true -> {ok, #date{year = Year, month = Month, day = Day}};
        false -> {error, invalid_day}
    end.

%% @doc Validate a time with seconds.
-spec validate_time(integer(), integer(), integer()) -> {ok, time()} | {error, atom()}.
validate_time(Hour, Minute, Second) ->
    validate_time(Hour, Minute, Second, 0).

%% @doc Validate a time with nanoseconds.
-spec validate_time(integer(), integer(), integer(), integer()) -> {ok, time()} | {error, atom()}.
validate_time(Hour, _, _, _) when Hour < 0; Hour > 23 ->
    {error, invalid_hour};
validate_time(_, Minute, _, _) when Minute < 0; Minute > 59 ->
    {error, invalid_minute};
validate_time(_, _, Second, _) when Second < 0; Second > 59 ->
    {error, invalid_second};
validate_time(_, _, _, Nanos) when Nanos < 0 ->
    {error, invalid_nanos};
validate_time(Hour, Minute, Second, Nanos) ->
    {ok, #time{hour = Hour, minute = Minute, second = Second, nanos = Nanos}}.

%% @doc Parse an ISO 8601 date (YYYY-MM-DD).
-spec parse_iso_date(string() | binary()) -> {ok, date()} | {error, atom()}.
parse_iso_date(Input) when is_binary(Input) ->
    parse_iso_date(binary_to_list(Input));
parse_iso_date(Input) when is_list(Input) ->
    case string:tokens(Input, "-") of
        [YearStr, MonthStr, DayStr] ->
            try
                Year = list_to_integer(YearStr),
                Month = list_to_integer(MonthStr),
                Day = list_to_integer(DayStr),
                validate_date(Year, Month, Day)
            catch
                _:_ -> {error, invalid_format}
            end;
        _ ->
            {error, invalid_format}
    end.

%% @doc Parse an ISO 8601 time (HH:MM:SS or HH:MM:SS.sss).
-spec parse_iso_time(string() | binary()) -> {ok, time()} | {error, atom()}.
parse_iso_time(Input) when is_binary(Input) ->
    parse_iso_time(binary_to_list(Input));
parse_iso_time(Input) when is_list(Input) ->
    case string:tokens(Input, ":") of
        [HourStr, MinuteStr] ->
            parse_time_parts(HourStr, MinuteStr, "0");
        [HourStr, MinuteStr, SecondStr] ->
            parse_time_parts(HourStr, MinuteStr, SecondStr);
        _ ->
            {error, invalid_format}
    end.

parse_time_parts(HourStr, MinuteStr, SecondStr) ->
    try
        Hour = list_to_integer(HourStr),
        Minute = list_to_integer(MinuteStr),
        {Second, Nanos} = parse_seconds(SecondStr),
        validate_time(Hour, Minute, Second, Nanos)
    catch
        _:_ -> {error, invalid_format}
    end.

parse_seconds(SecondStr) ->
    case string:tokens(SecondStr, ".") of
        [IntPart] ->
            {list_to_integer(IntPart), 0};
        [IntPart, FracPart] ->
            Second = list_to_integer(IntPart),
            Padded = FracPart ++ lists:duplicate(9 - length(FracPart), $0),
            Nanos = list_to_integer(string:substr(Padded, 1, 9)),
            {Second, Nanos};
        _ ->
            {list_to_integer(SecondStr), 0}
    end.

%% @doc Parse an ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS).
-spec parse_iso_datetime(string() | binary()) -> {ok, date(), time()} | {error, atom()}.
parse_iso_datetime(Input) when is_binary(Input) ->
    parse_iso_datetime(binary_to_list(Input));
parse_iso_datetime(Input) when is_list(Input) ->
    %% Remove timezone suffix if present
    CleanInput = strip_timezone(Input),
    case string:tokens(CleanInput, "T") of
        [DateStr, TimeStr] ->
            case parse_iso_date(DateStr) of
                {ok, Date} ->
                    case parse_iso_time(TimeStr) of
                        {ok, Time} -> {ok, Date, Time};
                        Error -> Error
                    end;
                Error -> Error
            end;
        _ ->
            {error, invalid_format}
    end.

strip_timezone(Input) ->
    %% Strip Z or +HH:MM or -HH:MM suffix
    case lists:last(Input) of
        $Z -> lists:droplast(Input);
        _ ->
            case string:rchr(Input, $+) of
                0 ->
                    case string:rchr(Input, $-) of
                        Pos when Pos > 10 -> string:substr(Input, 1, Pos - 1);
                        _ -> Input
                    end;
                Pos when Pos > 10 -> string:substr(Input, 1, Pos - 1);
                _ -> Input
            end
    end.

%% @doc Format a date as ISO 8601.
-spec format_iso_date(date()) -> string().
format_iso_date(#date{year = Year, month = Month, day = Day}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Year, Month, Day]).

%% @doc Format a time as ISO 8601.
-spec format_iso_time(time()) -> string().
format_iso_time(#time{hour = Hour, minute = Minute, second = Second}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Minute, Second]).

%% @doc Format a date and time as ISO 8601.
-spec format_iso_datetime(date(), time()) -> string().
format_iso_datetime(Date, Time) ->
    format_iso_date(Date) ++ "T" ++ format_iso_time(Time).

%% @doc Calculate the day of week (0 = Monday, 6 = Sunday).
%% Uses Zeller's congruence algorithm.
-spec day_of_week(integer(), integer(), integer()) -> 0..6.
day_of_week(Year, Month, Day) ->
    %% Adjust for January and February
    {Y, M} = case Month < 3 of
        true -> {Year - 1, Month + 12};
        false -> {Year, Month}
    end,
    K = Y rem 100,
    J = Y div 100,
    H = (Day + ((13 * (M + 1)) div 5) + K + (K div 4) + (J div 4) - (2 * J)) rem 7,
    %% Convert from Zeller (0=Saturday) to ISO (0=Monday)
    ((H + 5) rem 7).

%% @doc Calculate days between two dates.
-spec days_between(date(), date()) -> integer().
days_between(Date1, Date2) ->
    JD1 = to_julian_day(Date1),
    JD2 = to_julian_day(Date2),
    JD2 - JD1.

to_julian_day(#date{year = Y, month = M, day = D}) ->
    A = (14 - M) div 12,
    Y2 = Y + 4800 - A,
    M2 = M + 12 * A - 3,
    D + ((153 * M2 + 2) div 5) + 365 * Y2 + (Y2 div 4) - (Y2 div 100) + (Y2 div 400) - 32045.

%% @doc Add days to a date.
-spec add_days(date(), integer()) -> date().
add_days(Date, Days) ->
    JD = to_julian_day(Date) + Days,
    from_julian_day(JD).

from_julian_day(JD) ->
    A = JD + 32044,
    B = (4 * A + 3) div 146097,
    C = A - (146097 * B div 4),
    D = (4 * C + 3) div 1461,
    E = C - (1461 * D div 4),
    M = (5 * E + 2) div 153,
    Day = E - ((153 * M + 2) div 5) + 1,
    Month = M + 3 - 12 * (M div 10),
    Year = 100 * B + D - 4800 + (M div 10),
    #date{year = Year, month = Month, day = Day}.

%% @doc Get the current UTC time as date and time records.
-spec now_utc() -> {date(), time()}.
now_utc() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Date = #date{year = Year, month = Month, day = Day},
    Time = #time{hour = Hour, minute = Minute, second = Second, nanos = 0},
    {Date, Time}.
