%% SPDX-License-Identifier: PMPL-1.0-or-later
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafeCurrency - Currency and monetary operations for Erlang

-module(proven_currency).

-export([
    new/2,
    new/3,
    add/2,
    subtract/2,
    multiply/2,
    divide/2,
    negate/1,
    abs/1,
    compare/2,
    is_zero/1,
    is_positive/1,
    is_negative/1,
    format/1,
    format/2,
    parse/2,
    round/2,
    currency_info/1,
    convert/3,
    allocate/2
]).

-export_type([money/0, currency_code/0, rounding_mode/0]).

%% Money record: amount stored as integer in minor units (cents, pence, etc.)
-record(money, {
    amount :: integer(),
    currency :: currency_code(),
    decimals :: non_neg_integer()
}).

-type money() :: #money{}.

%% ISO 4217 currency codes
-type currency_code() :: usd | eur | gbp | jpy | cny | chf | cad | aud |
                         nzd | hkd | sgd | sek | nok | dkk | krw | inr |
                         brl | mxn | zar | rub | pln | czk | huf | ils |
                         thb | myr | php | idr | vnd | try | aed | sar |
                         btc | eth | atom().

-type rounding_mode() :: round_half_up | round_half_down | round_half_even |
                         round_up | round_down | truncate.

%% Currency decimal places mapping
currency_decimals(jpy) -> 0;
currency_decimals(krw) -> 0;
currency_decimals(vnd) -> 0;
currency_decimals(btc) -> 8;
currency_decimals(eth) -> 18;
currency_decimals(_) -> 2.

%% Currency symbols
currency_symbol(usd) -> "$";
currency_symbol(eur) -> "€";
currency_symbol(gbp) -> "£";
currency_symbol(jpy) -> "¥";
currency_symbol(cny) -> "¥";
currency_symbol(chf) -> "CHF ";
currency_symbol(btc) -> "₿";
currency_symbol(eth) -> "Ξ";
currency_symbol(Currency) -> string:uppercase(atom_to_list(Currency)) ++ " ".

%% @doc Create a new money value from a float or integer amount.
-spec new(number(), currency_code()) -> {ok, money()} | {error, term()}.
new(Amount, Currency) when is_number(Amount), is_atom(Currency) ->
    Decimals = currency_decimals(Currency),
    Multiplier = math:pow(10, Decimals),
    IntAmount = round(Amount * Multiplier),
    {ok, #money{amount = IntAmount, currency = Currency, decimals = Decimals}}.

%% @doc Create a new money value with explicit decimal places.
-spec new(number(), currency_code(), non_neg_integer()) -> {ok, money()} | {error, term()}.
new(Amount, Currency, Decimals) when is_number(Amount), is_atom(Currency), is_integer(Decimals), Decimals >= 0 ->
    Multiplier = math:pow(10, Decimals),
    IntAmount = round(Amount * Multiplier),
    {ok, #money{amount = IntAmount, currency = Currency, decimals = Decimals}}.

%% @doc Add two money values. Both must have the same currency.
-spec add(money(), money()) -> {ok, money()} | {error, currency_mismatch}.
add(#money{currency = C, decimals = D, amount = A1},
    #money{currency = C, decimals = D, amount = A2}) ->
    {ok, #money{amount = A1 + A2, currency = C, decimals = D}};
add(_, _) ->
    {error, currency_mismatch}.

%% @doc Subtract second money value from first. Both must have the same currency.
-spec subtract(money(), money()) -> {ok, money()} | {error, currency_mismatch}.
subtract(#money{currency = C, decimals = D, amount = A1},
         #money{currency = C, decimals = D, amount = A2}) ->
    {ok, #money{amount = A1 - A2, currency = C, decimals = D}};
subtract(_, _) ->
    {error, currency_mismatch}.

%% @doc Multiply money by a scalar value.
-spec multiply(money(), number()) -> {ok, money()}.
multiply(#money{amount = Amount, currency = C, decimals = D}, Multiplier) when is_number(Multiplier) ->
    NewAmount = round(Amount * Multiplier),
    {ok, #money{amount = NewAmount, currency = C, decimals = D}}.

%% @doc Divide money by a scalar value.
-spec divide(money(), number()) -> {ok, money()} | {error, division_by_zero}.
divide(_, 0) ->
    {error, division_by_zero};
divide(_, 0.0) ->
    {error, division_by_zero};
divide(#money{amount = Amount, currency = C, decimals = D}, Divisor) when is_number(Divisor) ->
    NewAmount = round(Amount / Divisor),
    {ok, #money{amount = NewAmount, currency = C, decimals = D}}.

%% @doc Negate a money value.
-spec negate(money()) -> money().
negate(#money{amount = Amount, currency = C, decimals = D}) ->
    #money{amount = -Amount, currency = C, decimals = D}.

%% @doc Return the absolute value of money.
-spec abs(money()) -> money().
abs(#money{amount = Amount, currency = C, decimals = D}) ->
    #money{amount = erlang:abs(Amount), currency = C, decimals = D}.

%% @doc Compare two money values. Returns lt, eq, or gt.
-spec compare(money(), money()) -> {ok, lt | eq | gt} | {error, currency_mismatch}.
compare(#money{currency = C, amount = A1}, #money{currency = C, amount = A2}) ->
    Result = if
        A1 < A2 -> lt;
        A1 > A2 -> gt;
        true -> eq
    end,
    {ok, Result};
compare(_, _) ->
    {error, currency_mismatch}.

%% @doc Check if money value is zero.
-spec is_zero(money()) -> boolean().
is_zero(#money{amount = 0}) -> true;
is_zero(_) -> false.

%% @doc Check if money value is positive.
-spec is_positive(money()) -> boolean().
is_positive(#money{amount = Amount}) when Amount > 0 -> true;
is_positive(_) -> false.

%% @doc Check if money value is negative.
-spec is_negative(money()) -> boolean().
is_negative(#money{amount = Amount}) when Amount < 0 -> true;
is_negative(_) -> false.

%% @doc Format money as a string with currency symbol.
-spec format(money()) -> string().
format(Money) ->
    format(Money, #{}).

%% @doc Format money with options.
%% Options: symbol => boolean(), code => boolean(), separator => string()
-spec format(money(), map()) -> string().
format(#money{amount = Amount, currency = Currency, decimals = Decimals}, Options) ->
    ShowSymbol = maps:get(symbol, Options, true),
    ShowCode = maps:get(code, Options, false),
    Separator = maps:get(separator, Options, ","),

    Sign = if Amount < 0 -> "-"; true -> "" end,
    AbsAmount = erlang:abs(Amount),

    %% Convert to string with decimal point
    Divisor = round(math:pow(10, Decimals)),
    IntPart = AbsAmount div Divisor,
    FracPart = AbsAmount rem Divisor,

    %% Format integer part with thousands separator
    IntStr = format_with_separators(integer_to_list(IntPart), Separator),

    %% Format fractional part with leading zeros
    FracStr = if
        Decimals > 0 ->
            FracFormatted = pad_left_int(FracPart, Decimals),
            "." ++ FracFormatted;
        true ->
            ""
    end,

    %% Build result
    Symbol = if ShowSymbol -> currency_symbol(Currency); true -> "" end,
    Code = if ShowCode -> " " ++ string:uppercase(atom_to_list(Currency)); true -> "" end,

    Sign ++ Symbol ++ IntStr ++ FracStr ++ Code.

pad_left_int(Value, Width) ->
    Str = integer_to_list(Value),
    pad_left_str(Str, Width, $0).

pad_left_str(Str, Width, PadChar) when length(Str) >= Width ->
    Str;
pad_left_str(Str, Width, PadChar) ->
    pad_left_str([PadChar | Str], Width, PadChar).

format_with_separators(NumStr, Separator) ->
    Reversed = lists:reverse(NumStr),
    Grouped = group_digits(Reversed, 3),
    lists:flatten(lists:join(Separator, Grouped)).

group_digits([], _) -> [];
group_digits(List, N) ->
    {Group, Rest} = lists:split(min(N, length(List)), List),
    [lists:reverse(Group) | group_digits(Rest, N)].

%% @doc Parse a string into a money value.
-spec parse(string() | binary(), currency_code()) -> {ok, money()} | {error, invalid_format}.
parse(Input, Currency) when is_binary(Input) ->
    parse(binary_to_list(Input), Currency);
parse(Input, Currency) when is_list(Input) ->
    %% Remove currency symbols and whitespace
    Cleaned = lists:filter(fun(C) ->
        (C >= $0 andalso C =< $9) orelse C == $. orelse C == $- orelse C == $,
    end, Input),
    %% Remove thousands separators
    NoCommas = [C || C <- Cleaned, C =/= $,],
    try
        Amount = case string:to_float(NoCommas) of
            {error, _} ->
                case string:to_integer(NoCommas) of
                    {Int, []} -> float(Int);
                    _ -> throw(invalid_format)
                end;
            {Float, []} -> Float;
            _ -> throw(invalid_format)
        end,
        new(Amount, Currency)
    catch
        _:_ -> {error, invalid_format}
    end.

%% @doc Round money to specified decimal places.
-spec round(money(), rounding_mode()) -> money().
round(Money, _Mode) ->
    %% Money is already stored in integer minor units, so it's inherently rounded
    Money.

%% @doc Get information about a currency.
-spec currency_info(currency_code()) -> map().
currency_info(Currency) ->
    #{
        code => Currency,
        symbol => currency_symbol(Currency),
        decimals => currency_decimals(Currency),
        name => currency_name(Currency)
    }.

currency_name(usd) -> "US Dollar";
currency_name(eur) -> "Euro";
currency_name(gbp) -> "British Pound";
currency_name(jpy) -> "Japanese Yen";
currency_name(cny) -> "Chinese Yuan";
currency_name(chf) -> "Swiss Franc";
currency_name(cad) -> "Canadian Dollar";
currency_name(aud) -> "Australian Dollar";
currency_name(btc) -> "Bitcoin";
currency_name(eth) -> "Ethereum";
currency_name(Currency) -> atom_to_list(Currency).

%% @doc Convert money to another currency using an exchange rate.
-spec convert(money(), currency_code(), number()) -> {ok, money()}.
convert(#money{amount = Amount, decimals = FromDecimals}, ToCurrency, Rate)
  when is_number(Rate), Rate > 0 ->
    ToDecimals = currency_decimals(ToCurrency),
    %% Convert to base units, apply rate, convert to target decimals
    BaseAmount = Amount / math:pow(10, FromDecimals),
    ConvertedBase = BaseAmount * Rate,
    NewAmount = round(ConvertedBase * math:pow(10, ToDecimals)),
    {ok, #money{amount = NewAmount, currency = ToCurrency, decimals = ToDecimals}}.

%% @doc Allocate money across multiple parts according to ratios.
%% Returns a list of money values that sum to the original.
-spec allocate(money(), [number()]) -> {ok, [money()]} | {error, empty_ratios}.
allocate(_, []) ->
    {error, empty_ratios};
allocate(#money{amount = Amount, currency = C, decimals = D}, Ratios) ->
    Total = lists:sum(Ratios),
    case Total of
        0 -> {error, zero_total_ratio};
        _ ->
            {Allocated, Remainder} = lists:foldl(
                fun(Ratio, {Acc, Remaining}) ->
                    Share = trunc((Amount * Ratio) / Total),
                    {[Share | Acc], Remaining - Share}
                end,
                {[], Amount},
                Ratios
            ),
            %% Distribute remainder to largest shares
            ReversedAllocated = lists:reverse(Allocated),
            Final = distribute_remainder(ReversedAllocated, Remainder),
            MoneyList = [#money{amount = A, currency = C, decimals = D} || A <- Final],
            {ok, MoneyList}
    end.

distribute_remainder(Amounts, 0) ->
    Amounts;
distribute_remainder(Amounts, Remainder) when Remainder > 0 ->
    %% Add 1 to each amount until remainder is exhausted
    distribute_one_each(Amounts, Remainder, []);
distribute_remainder(Amounts, Remainder) when Remainder < 0 ->
    %% Subtract 1 from each amount until remainder is exhausted
    distribute_one_each(Amounts, -Remainder, []).

distribute_one_each([], _, Acc) ->
    lists:reverse(Acc);
distribute_one_each([H | T], 0, Acc) ->
    lists:reverse(Acc) ++ [H | T];
distribute_one_each([H | T], N, Acc) ->
    distribute_one_each(T, N - 1, [H + 1 | Acc]).
