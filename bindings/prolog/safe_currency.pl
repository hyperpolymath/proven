% SPDX-License-Identifier: Apache-2.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafeCurrency - Type-safe currency operations for Prolog
% Following ISO 4217 currency codes.

:- module(safe_currency, [
    currency_code/2,
    currency_decimals/2,
    currency_symbol/2,
    currency_name/2,
    money/3,
    money_zero/2,
    money_from_major/3,
    money_from_minor/3,
    money_add/3,
    money_sub/3,
    money_mul/3,
    money_div/3,
    money_abs/2,
    money_negate/2,
    money_is_zero/1,
    money_is_positive/1,
    money_is_negative/1,
    money_compare/3,
    format_money/2,
    parse_currency_code/2,
    is_valid_currency_code/1
]).

:- use_module(library(lists)).

%! currency_code(?Code, ?NumericCode) is nondet.
%
%  ISO 4217 currency codes with numeric identifiers.
%  Can be used to enumerate or look up codes.
%
%  @arg Code Three-letter currency code atom
%  @arg NumericCode ISO 4217 numeric code
currency_code(usd, 840).
currency_code(eur, 978).
currency_code(gbp, 826).
currency_code(jpy, 392).
currency_code(chf, 756).
currency_code(cad, 124).
currency_code(aud, 036).
currency_code(nzd, 554).
currency_code(cny, 156).
currency_code(inr, 356).
currency_code(brl, 986).
currency_code(mxn, 484).
currency_code(krw, 410).
currency_code(sgd, 702).
currency_code(hkd, 344).
currency_code(sek, 752).
currency_code(nok, 578).
currency_code(dkk, 208).
currency_code(pln, 985).
currency_code(rub, 643).
currency_code(zar, 710).
currency_code(try, 949).
currency_code(thb, 764).
currency_code(myr, 458).
currency_code(idr, 360).
currency_code(php, 608).
currency_code(vnd, 704).
currency_code(aed, 784).
currency_code(sar, 682).
currency_code(ils, 376).
currency_code(czk, 203).
currency_code(huf, 348).
currency_code(ron, 946).
currency_code(bgn, 975).
currency_code(hrk, 191).
currency_code(isk, 352).
currency_code(clp, 152).
currency_code(cop, 170).
currency_code(pen, 604).
currency_code(ars, 032).
%% Cryptocurrencies (non-ISO)
currency_code(btc, 0).
currency_code(eth, 0).

%! currency_decimals(?Code, ?Decimals) is nondet.
%
%  Number of decimal places for each currency.
%  Zero-decimal currencies: JPY, KRW, VND.
%  Cryptocurrencies use 8 decimals.
%
%  @arg Code Currency code atom
%  @arg Decimals Number of decimal places
currency_decimals(jpy, 0).
currency_decimals(krw, 0).
currency_decimals(vnd, 0).
currency_decimals(btc, 8).
currency_decimals(eth, 8).
currency_decimals(Code, 2) :-
    currency_code(Code, _),
    \+ member(Code, [jpy, krw, vnd, btc, eth]).

%! currency_symbol(?Code, ?Symbol) is nondet.
%
%  Currency symbol for display.
%
%  @arg Code Currency code atom
%  @arg Symbol Unicode symbol string
currency_symbol(usd, "$").
currency_symbol(eur, "\u20AC").   % Euro sign
currency_symbol(gbp, "\u00A3").   % Pound sign
currency_symbol(jpy, "\u00A5").   % Yen sign
currency_symbol(cny, "\u00A5").   % Yuan (same as yen)
currency_symbol(chf, "Fr").
currency_symbol(inr, "\u20B9").   % Indian Rupee
currency_symbol(krw, "\u20A9").   % Won sign
currency_symbol(rub, "\u20BD").   % Ruble sign
currency_symbol(btc, "\u20BF").   % Bitcoin sign
currency_symbol(eth, "\u039E").   % Xi (Ethereum)
currency_symbol(Code, "") :-
    currency_code(Code, _),
    \+ currency_symbol(Code, _).

%! currency_name(?Code, ?Name) is nondet.
%
%  Full name of the currency.
%
%  @arg Code Currency code atom
%  @arg Name Human-readable name
currency_name(usd, "US Dollar").
currency_name(eur, "Euro").
currency_name(gbp, "British Pound").
currency_name(jpy, "Japanese Yen").
currency_name(chf, "Swiss Franc").
currency_name(cad, "Canadian Dollar").
currency_name(aud, "Australian Dollar").
currency_name(nzd, "New Zealand Dollar").
currency_name(cny, "Chinese Yuan").
currency_name(inr, "Indian Rupee").
currency_name(brl, "Brazilian Real").
currency_name(mxn, "Mexican Peso").
currency_name(krw, "South Korean Won").
currency_name(btc, "Bitcoin").
currency_name(eth, "Ethereum").

%! money(?MinorUnits, ?CurrencyCode, ?MoneyTerm) is det.
%
%  Construct or deconstruct a money term.
%  Money is stored in minor units (cents, satoshis, etc.).
%
%  @arg MinorUnits Integer amount in smallest currency unit
%  @arg CurrencyCode Currency code atom
%  @arg MoneyTerm money(MinorUnits, CurrencyCode) structure
money(MinorUnits, CurrencyCode, money(MinorUnits, CurrencyCode)) :-
    integer(MinorUnits),
    currency_code(CurrencyCode, _).

%! money_zero(+CurrencyCode, -Money) is det.
%
%  Create zero money in given currency.
%
%  @arg CurrencyCode Currency code atom
%  @arg Money Zero money term
money_zero(CurrencyCode, money(0, CurrencyCode)) :-
    currency_code(CurrencyCode, _).

%! money_from_major(+MajorUnits, +CurrencyCode, -Money) is det.
%
%  Create money from major units (dollars, euros, etc.).
%
%  @arg MajorUnits Amount in major units
%  @arg CurrencyCode Currency code atom
%  @arg Money Money term
money_from_major(MajorUnits, CurrencyCode, money(MinorUnits, CurrencyCode)) :-
    currency_code(CurrencyCode, _),
    currency_decimals(CurrencyCode, Decimals),
    Multiplier is 10 ^ Decimals,
    MinorUnits is round(MajorUnits * Multiplier).

%! money_from_minor(+MinorUnits, +CurrencyCode, -Money) is det.
%
%  Create money from minor units (cents, satoshis, etc.).
%
%  @arg MinorUnits Amount in minor units
%  @arg CurrencyCode Currency code atom
%  @arg Money Money term
money_from_minor(MinorUnits, CurrencyCode, money(MinorUnits, CurrencyCode)) :-
    integer(MinorUnits),
    currency_code(CurrencyCode, _).

%! money_add(+MoneyA, +MoneyB, -Result) is semidet.
%
%  Add two money values. Currencies must match.
%
%  @arg MoneyA First money term
%  @arg MoneyB Second money term
%  @arg Result Sum as money term
money_add(money(A, Currency), money(B, Currency), money(Sum, Currency)) :-
    Sum is A + B.

%! money_sub(+MoneyA, +MoneyB, -Result) is semidet.
%
%  Subtract money values. Currencies must match.
%
%  @arg MoneyA First money term
%  @arg MoneyB Second money term
%  @arg Result Difference as money term
money_sub(money(A, Currency), money(B, Currency), money(Diff, Currency)) :-
    Diff is A - B.

%! money_mul(+Money, +Scalar, -Result) is det.
%
%  Multiply money by a scalar.
%
%  @arg Money Money term
%  @arg Scalar Numeric multiplier
%  @arg Result Product as money term
money_mul(money(Amount, Currency), Scalar, money(Product, Currency)) :-
    Product is round(Amount * Scalar).

%! money_div(+Money, +Scalar, -Result) is semidet.
%
%  Divide money by a scalar. Fails if scalar is zero.
%
%  @arg Money Money term
%  @arg Scalar Numeric divisor (non-zero)
%  @arg Result Quotient as money term
money_div(money(Amount, Currency), Scalar, money(Quotient, Currency)) :-
    Scalar =\= 0,
    Quotient is Amount // Scalar.

%! money_abs(+Money, -AbsMoney) is det.
%
%  Absolute value of money.
%
%  @arg Money Money term
%  @arg AbsMoney Absolute value as money term
money_abs(money(Amount, Currency), money(AbsAmount, Currency)) :-
    AbsAmount is abs(Amount).

%! money_negate(+Money, -NegMoney) is det.
%
%  Negate money value.
%
%  @arg Money Money term
%  @arg NegMoney Negated money term
money_negate(money(Amount, Currency), money(NegAmount, Currency)) :-
    NegAmount is -Amount.

%! money_is_zero(+Money) is semidet.
%
%  Check if money amount is zero.
%
%  @arg Money Money term
money_is_zero(money(0, _)).

%! money_is_positive(+Money) is semidet.
%
%  Check if money amount is positive.
%
%  @arg Money Money term
money_is_positive(money(Amount, _)) :-
    Amount > 0.

%! money_is_negative(+Money) is semidet.
%
%  Check if money amount is negative.
%
%  @arg Money Money term
money_is_negative(money(Amount, _)) :-
    Amount < 0.

%! money_compare(-Order, +MoneyA, +MoneyB) is semidet.
%
%  Compare two money values. Currencies must match.
%  Order is one of: <, =, >.
%
%  @arg Order Comparison result atom
%  @arg MoneyA First money term
%  @arg MoneyB Second money term
money_compare(Order, money(A, Currency), money(B, Currency)) :-
    compare(Order, A, B).

%! format_money(+Money, -Formatted) is det.
%
%  Format money for display with symbol.
%
%  @arg Money Money term
%  @arg Formatted Formatted string with symbol
format_money(money(Amount, Currency), Formatted) :-
    currency_decimals(Currency, Decimals),
    currency_symbol(Currency, Symbol),
    AbsAmount is abs(Amount),
    Divisor is 10 ^ Decimals,
    Major is AbsAmount // Divisor,
    Minor is AbsAmount mod Divisor,
    ( Amount < 0 -> Sign = "-" ; Sign = "" ),
    ( Decimals =:= 0 ->
        format(atom(Formatted), "~w~w~d", [Sign, Symbol, Major])
    ;
        format(atom(Formatted), "~w~w~d.~|~`0t~d~*+",
               [Sign, Symbol, Major, Minor, Decimals])
    ).

%! parse_currency_code(+String, -Code) is semidet.
%
%  Parse a currency code string to atom.
%
%  @arg String Currency code string (case-insensitive)
%  @arg Code Currency code atom
parse_currency_code(String, Code) :-
    atom_string(StringAtom, String),
    downcase_atom(StringAtom, Code),
    currency_code(Code, _).

%! is_valid_currency_code(+String) is semidet.
%
%  Check if string is a valid currency code.
%
%  @arg String Currency code string
is_valid_currency_code(String) :-
    parse_currency_code(String, _).
