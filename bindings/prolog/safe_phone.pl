% SPDX-License-Identifier: PMPL-1.0
% SPDX-FileCopyrightText: 2025 Hyperpolymath
%
% Proven SafePhone - Phone number validation for Prolog
% Following E.164 specification.

:- module(safe_phone, [
    country_code/2,
    country_name/2,
    phone_number/3,
    parse_phone/2,
    format_phone/2,
    format_phone_e164/2,
    format_phone_international/2,
    is_valid_phone/1,
    phone_country/2,
    phone_national/2,
    phone_digit_count/2
]).

:- use_module(library(lists)).

%! country_code(?CountryAtom, ?DialingCode) is nondet.
%
%  Country calling codes (ITU-T E.164).
%  Can be used to enumerate or look up codes.
%
%  @arg CountryAtom Two-letter country code atom
%  @arg DialingCode Numeric dialing code
country_code(us, 1).    % USA, Canada, Caribbean
country_code(ca, 1).    % Canada (shares with US)
country_code(ru, 7).    % Russia
country_code(kz, 7).    % Kazakhstan (shares with Russia)
country_code(eg, 20).   % Egypt
country_code(za, 27).   % South Africa
country_code(gr, 30).   % Greece
country_code(nl, 31).   % Netherlands
country_code(be, 32).   % Belgium
country_code(fr, 33).   % France
country_code(es, 34).   % Spain
country_code(hu, 36).   % Hungary
country_code(it, 39).   % Italy
country_code(ro, 40).   % Romania
country_code(ch, 41).   % Switzerland
country_code(at, 43).   % Austria
country_code(uk, 44).   % United Kingdom
country_code(gb, 44).   % Great Britain (alias for UK)
country_code(dk, 45).   % Denmark
country_code(se, 46).   % Sweden
country_code(no, 47).   % Norway
country_code(pl, 48).   % Poland
country_code(de, 49).   % Germany
country_code(mx, 52).   % Mexico
country_code(br, 55).   % Brazil
country_code(ar, 54).   % Argentina
country_code(cl, 56).   % Chile
country_code(co, 57).   % Colombia
country_code(pe, 51).   % Peru
country_code(ve, 58).   % Venezuela
country_code(my, 60).   % Malaysia
country_code(au, 61).   % Australia
country_code(id, 62).   % Indonesia
country_code(ph, 63).   % Philippines
country_code(nz, 64).   % New Zealand
country_code(sg, 65).   % Singapore
country_code(th, 66).   % Thailand
country_code(jp, 81).   % Japan
country_code(kr, 82).   % South Korea
country_code(vn, 84).   % Vietnam
country_code(cn, 86).   % China
country_code(tw, 886).  % Taiwan
country_code(hk, 852).  % Hong Kong
country_code(in, 91).   % India
country_code(pk, 92).   % Pakistan
country_code(ir, 98).   % Iran
country_code(tr, 90).   % Turkey
country_code(ae, 971).  % UAE
country_code(sa, 966).  % Saudi Arabia
country_code(il, 972).  % Israel

%! country_name(?CountryAtom, ?Name) is nondet.
%
%  Full country name for country code.
%
%  @arg CountryAtom Two-letter country code atom
%  @arg Name Full country name string
country_name(us, "United States").
country_name(ca, "Canada").
country_name(uk, "United Kingdom").
country_name(gb, "Great Britain").
country_name(de, "Germany").
country_name(fr, "France").
country_name(it, "Italy").
country_name(es, "Spain").
country_name(jp, "Japan").
country_name(cn, "China").
country_name(kr, "South Korea").
country_name(au, "Australia").
country_name(in, "India").
country_name(br, "Brazil").
country_name(mx, "Mexico").
country_name(ru, "Russia").

%! phone_number(?CountryCode, ?NationalNumber, ?PhoneTerm) is det.
%
%  Construct or deconstruct a phone number term.
%
%  @arg CountryCode Numeric country calling code
%  @arg NationalNumber National number as atom/string
%  @arg PhoneTerm phone(CountryCode, NationalNumber) structure
phone_number(CountryCode, NationalNumber, phone(CountryCode, NationalNumber)) :-
    integer(CountryCode),
    CountryCode > 0,
    atom_string(NationalNumber, NatStr),
    string_length(NatStr, Len),
    Len >= 4,
    Len =< 14.

%! parse_phone(+Input, -Phone) is semidet.
%
%  Parse a phone number string into phone/2 structure.
%  Accepts various formats: +1 555 123 4567, 15551234567, etc.
%
%  @arg Input Phone number string (various formats)
%  @arg Phone phone(CountryCode, NationalNumber) structure
parse_phone(Input, phone(CountryCode, NationalNumber)) :-
    atom_string(Input, InputStr),
    string_chars(InputStr, Chars),
    include(char_type_(digit), Chars, DigitChars),
    atom_chars(DigitsAtom, DigitChars),
    atom_string(DigitsAtom, Digits),
    string_length(Digits, Len),
    Len >= 7,
    Len =< 15,
    parse_country_code(Digits, CountryCode, NationalStart),
    sub_string(Digits, NationalStart, _, 0, NationalStr),
    string_length(NationalStr, NatLen),
    NatLen >= 4,
    atom_string(NationalNumber, NationalStr).

%% Helper for char_type with single argument
char_type_(Type, Char) :-
    char_type(Char, Type).

%! format_phone(+Phone, -Formatted) is det.
%
%  Format phone number in E.164 format (alias for format_phone_e164).
%
%  @arg Phone phone/2 structure
%  @arg Formatted E.164 formatted string
format_phone(Phone, Formatted) :-
    format_phone_e164(Phone, Formatted).

%! format_phone_e164(+Phone, -Formatted) is det.
%
%  Format phone number in E.164 format (+XNNNNNNNNNN).
%
%  @arg Phone phone/2 structure
%  @arg Formatted E.164 formatted string
format_phone_e164(phone(CountryCode, NationalNumber), Formatted) :-
    format(atom(Formatted), "+~d~w", [CountryCode, NationalNumber]).

%! format_phone_international(+Phone, -Formatted) is det.
%
%  Format phone number with spaces for readability.
%
%  @arg Phone phone/2 structure
%  @arg Formatted Spaced international format
format_phone_international(phone(CountryCode, NationalNumber), Formatted) :-
    atom_string(NationalNumber, NatStr),
    string_length(NatStr, Len),
    format_national_spaced(NatStr, Len, SpacedNat),
    format(atom(Formatted), "+~d ~s", [CountryCode, SpacedNat]).

%% Format national number with spaces based on length
format_national_spaced(Nat, Len, Spaced) :-
    ( Len =< 4 ->
        Spaced = Nat
    ; Len =< 7 ->
        sub_string(Nat, 0, 3, _, Part1),
        sub_string(Nat, 3, _, 0, Part2),
        format(string(Spaced), "~s ~s", [Part1, Part2])
    ; Len =< 10 ->
        sub_string(Nat, 0, 3, _, Part1),
        sub_string(Nat, 3, 3, _, Part2),
        sub_string(Nat, 6, _, 0, Part3),
        format(string(Spaced), "~s ~s ~s", [Part1, Part2, Part3])
    ;
        Spaced = Nat
    ).

%! is_valid_phone(+Input) is semidet.
%
%  Check if input is a valid phone number.
%
%  @arg Input Phone number string
is_valid_phone(Input) :-
    catch(parse_phone(Input, _), _, fail).

%! phone_country(+Phone, -CountryAtom) is semidet.
%
%  Extract country atom from phone number.
%
%  @arg Phone phone/2 structure
%  @arg CountryAtom Two-letter country code atom
phone_country(phone(CountryCode, _), CountryAtom) :-
    country_code(CountryAtom, CountryCode),
    !.
phone_country(phone(_, _), unknown).

%! phone_national(+Phone, -NationalNumber) is det.
%
%  Extract national number from phone structure.
%
%  @arg Phone phone/2 structure
%  @arg NationalNumber National number atom
phone_national(phone(_, NationalNumber), NationalNumber).

%! phone_digit_count(+Phone, -Count) is det.
%
%  Total number of digits in the phone number (country + national).
%
%  @arg Phone phone/2 structure
%  @arg Count Total digit count
phone_digit_count(phone(CountryCode, NationalNumber), Count) :-
    atom_string(NationalNumber, NatStr),
    string_length(NatStr, NatLen),
    ( CountryCode >= 100 -> CCLen = 3
    ; CountryCode >= 10 -> CCLen = 2
    ; CCLen = 1
    ),
    Count is CCLen + NatLen.

%% Parse country code from digit string
%% Try 3-digit, 2-digit, then 1-digit codes
parse_country_code(Digits, CountryCode, NationalStart) :-
    string_length(Digits, Len),
    Len >= 4,
    (   % Try 3-digit country code
        sub_string(Digits, 0, 3, _, CC3Str),
        number_string(CC3, CC3Str),
        country_code(_, CC3)
    ->  CountryCode = CC3, NationalStart = 3
    ;   % Try 2-digit country code
        sub_string(Digits, 0, 2, _, CC2Str),
        number_string(CC2, CC2Str),
        country_code(_, CC2)
    ->  CountryCode = CC2, NationalStart = 2
    ;   % Try 1-digit country code
        sub_string(Digits, 0, 1, _, CC1Str),
        number_string(CC1, CC1Str),
        country_code(_, CC1)
    ->  CountryCode = CC1, NationalStart = 1
    ;   % No recognized country code
        fail
    ).
