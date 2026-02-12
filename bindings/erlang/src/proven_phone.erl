%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2025 Hyperpolymath
%%
%% Proven SafePhone - Phone number validation and formatting for Erlang

-module(proven_phone).

-export([
    parse/1,
    parse/2,
    format/1,
    format/2,
    is_valid/1,
    is_valid/2,
    normalize/1,
    get_country_code/1,
    get_national_number/1,
    get_region/1,
    country_info/1,
    compare/2
]).

-export_type([phone_number/0, country_code/0, phone_format/0]).

%% Phone number record
-record(phone_number, {
    country_code :: pos_integer(),
    national_number :: string(),
    extension :: string() | undefined,
    region :: country_code()
}).

-type phone_number() :: #phone_number{}.

%% ISO 3166-1 alpha-2 country codes
-type country_code() :: us | ca | gb | de | fr | it | es | nl | be | at |
                        ch | au | nz | jp | cn | kr | in | br | mx | ar |
                        ru | pl | se | no | dk | fi | ie | pt | gr | cz |
                        hu | ro | bg | hr | sk | si | ee | lv | lt | ua |
                        il | ae | sa | za | eg | ng | ke | ma | sg | hk |
                        tw | th | my | ph | id | vn | pk | bd | atom().

-type phone_format() :: e164 | international | national | rfc3966.

%% Country code to dial code mapping
dial_code(us) -> 1;
dial_code(ca) -> 1;
dial_code(gb) -> 44;
dial_code(de) -> 49;
dial_code(fr) -> 33;
dial_code(it) -> 39;
dial_code(es) -> 34;
dial_code(nl) -> 31;
dial_code(be) -> 32;
dial_code(at) -> 43;
dial_code(ch) -> 41;
dial_code(au) -> 61;
dial_code(nz) -> 64;
dial_code(jp) -> 81;
dial_code(cn) -> 86;
dial_code(kr) -> 82;
dial_code(in) -> 91;
dial_code(br) -> 55;
dial_code(mx) -> 52;
dial_code(ar) -> 54;
dial_code(ru) -> 7;
dial_code(pl) -> 48;
dial_code(se) -> 46;
dial_code(no) -> 47;
dial_code(dk) -> 45;
dial_code(fi) -> 358;
dial_code(ie) -> 353;
dial_code(pt) -> 351;
dial_code(gr) -> 30;
dial_code(il) -> 972;
dial_code(ae) -> 971;
dial_code(sa) -> 966;
dial_code(za) -> 27;
dial_code(sg) -> 65;
dial_code(hk) -> 852;
dial_code(tw) -> 886;
dial_code(th) -> 66;
dial_code(my) -> 60;
dial_code(ph) -> 63;
dial_code(id) -> 62;
dial_code(vn) -> 84;
dial_code(pk) -> 92;
dial_code(bd) -> 880;
dial_code(_) -> undefined.

%% Dial code to country mapping (for parsing)
country_from_dial_code(1) -> us;  %% Default to US for NANP
country_from_dial_code(44) -> gb;
country_from_dial_code(49) -> de;
country_from_dial_code(33) -> fr;
country_from_dial_code(39) -> it;
country_from_dial_code(34) -> es;
country_from_dial_code(31) -> nl;
country_from_dial_code(32) -> be;
country_from_dial_code(43) -> at;
country_from_dial_code(41) -> ch;
country_from_dial_code(61) -> au;
country_from_dial_code(64) -> nz;
country_from_dial_code(81) -> jp;
country_from_dial_code(86) -> cn;
country_from_dial_code(82) -> kr;
country_from_dial_code(91) -> in;
country_from_dial_code(55) -> br;
country_from_dial_code(52) -> mx;
country_from_dial_code(54) -> ar;
country_from_dial_code(7) -> ru;
country_from_dial_code(48) -> pl;
country_from_dial_code(46) -> se;
country_from_dial_code(47) -> no;
country_from_dial_code(45) -> dk;
country_from_dial_code(358) -> fi;
country_from_dial_code(353) -> ie;
country_from_dial_code(351) -> pt;
country_from_dial_code(30) -> gr;
country_from_dial_code(972) -> il;
country_from_dial_code(971) -> ae;
country_from_dial_code(966) -> sa;
country_from_dial_code(27) -> za;
country_from_dial_code(65) -> sg;
country_from_dial_code(852) -> hk;
country_from_dial_code(886) -> tw;
country_from_dial_code(66) -> th;
country_from_dial_code(60) -> my;
country_from_dial_code(63) -> ph;
country_from_dial_code(62) -> id;
country_from_dial_code(84) -> vn;
country_from_dial_code(92) -> pk;
country_from_dial_code(880) -> bd;
country_from_dial_code(_) -> unknown.

%% Expected national number length ranges by country
national_length_range(us) -> {10, 10};
national_length_range(ca) -> {10, 10};
national_length_range(gb) -> {10, 11};
national_length_range(de) -> {7, 12};
national_length_range(fr) -> {9, 9};
national_length_range(jp) -> {9, 11};
national_length_range(cn) -> {11, 11};
national_length_range(in) -> {10, 10};
national_length_range(au) -> {9, 9};
national_length_range(_) -> {6, 15}.

%% @doc Parse a phone number string with automatic country detection.
-spec parse(string() | binary()) -> {ok, phone_number()} | {error, term()}.
parse(Input) ->
    parse(Input, undefined).

%% @doc Parse a phone number string with a default country.
-spec parse(string() | binary(), country_code() | undefined) -> {ok, phone_number()} | {error, term()}.
parse(Input, DefaultCountry) when is_binary(Input) ->
    parse(binary_to_list(Input), DefaultCountry);
parse(Input, DefaultCountry) when is_list(Input) ->
    %% Extract extension if present
    {MainNumber, Extension} = extract_extension(Input),

    %% Extract only digits and + sign
    Cleaned = clean_phone_string(MainNumber),

    case Cleaned of
        [$+ | DigitsAfterPlus] ->
            parse_international(DigitsAfterPlus, Extension);
        Digits when length(Digits) > 0 ->
            parse_national(Digits, DefaultCountry, Extension);
        _ ->
            {error, invalid_format}
    end.

extract_extension(Input) ->
    %% Look for extension markers: x, ext, ext., #
    LowerInput = string:lowercase(Input),
    ExtPatterns = ["ext.", "ext ", " x ", " x", "#"],
    extract_extension_with_patterns(Input, LowerInput, ExtPatterns).

extract_extension_with_patterns(Input, _LowerInput, []) ->
    {Input, undefined};
extract_extension_with_patterns(Input, LowerInput, [Pattern | Rest]) ->
    case string:find(LowerInput, Pattern) of
        nomatch ->
            extract_extension_with_patterns(Input, LowerInput, Rest);
        _ ->
            %% Find position in original string
            Pos = string:str(LowerInput, Pattern),
            MainPart = string:substr(Input, 1, Pos - 1),
            ExtPart = string:substr(Input, Pos + length(Pattern)),
            ExtDigits = [C || C <- ExtPart, C >= $0, C =< $9],
            case ExtDigits of
                [] -> {Input, undefined};
                _ -> {MainPart, ExtDigits}
            end
    end.

clean_phone_string(Input) ->
    [C || C <- Input, (C >= $0 andalso C =< $9) orelse C == $+].

parse_international(Digits, Extension) ->
    %% Try to match country codes (1-3 digits)
    case try_country_codes(Digits) of
        {ok, CountryCode, NationalDigits, Region} ->
            {ok, #phone_number{
                country_code = CountryCode,
                national_number = NationalDigits,
                extension = Extension,
                region = Region
            }};
        error ->
            {error, unknown_country_code}
    end.

try_country_codes(Digits) ->
    %% Try 1, 2, and 3 digit country codes
    try_country_code(Digits, 1).

try_country_code(Digits, N) when N =< 3, length(Digits) > N ->
    {CodePart, NationalPart} = lists:split(N, Digits),
    CountryCode = list_to_integer(CodePart),
    case country_from_dial_code(CountryCode) of
        unknown ->
            try_country_code(Digits, N + 1);
        Region ->
            {ok, CountryCode, NationalPart, Region}
    end;
try_country_code(_, _) ->
    error.

parse_national(Digits, undefined, Extension) ->
    %% Without a default country, assume US/NANP for 10-digit numbers
    case length(Digits) of
        10 ->
            {ok, #phone_number{
                country_code = 1,
                national_number = Digits,
                extension = Extension,
                region = us
            }};
        11 when hd(Digits) == $1 ->
            {ok, #phone_number{
                country_code = 1,
                national_number = tl(Digits),
                extension = Extension,
                region = us
            }};
        _ ->
            {error, cannot_determine_country}
    end;
parse_national(Digits, Country, Extension) ->
    CountryCode = dial_code(Country),
    case CountryCode of
        undefined ->
            {error, unknown_country};
        _ ->
            %% Strip leading country code if present
            CodeStr = integer_to_list(CountryCode),
            NationalDigits = case lists:prefix(CodeStr, Digits) of
                true -> lists:nthtail(length(CodeStr), Digits);
                false -> Digits
            end,
            {ok, #phone_number{
                country_code = CountryCode,
                national_number = NationalDigits,
                extension = Extension,
                region = Country
            }}
    end.

%% @doc Format a phone number in E.164 format.
-spec format(phone_number()) -> string().
format(Phone) ->
    format(Phone, e164).

%% @doc Format a phone number in the specified format.
-spec format(phone_number(), phone_format()) -> string().
format(#phone_number{country_code = CC, national_number = NN, extension = Ext}, e164) ->
    Base = "+" ++ integer_to_list(CC) ++ NN,
    add_extension(Base, Ext, ";ext=");
format(#phone_number{country_code = CC, national_number = NN, extension = Ext, region = Region}, international) ->
    Formatted = format_national_number(NN, Region),
    Base = "+" ++ integer_to_list(CC) ++ " " ++ Formatted,
    add_extension(Base, Ext, " ext. ");
format(#phone_number{national_number = NN, extension = Ext, region = Region}, national) ->
    Formatted = format_national_number(NN, Region),
    add_extension(Formatted, Ext, " ext. ");
format(#phone_number{country_code = CC, national_number = NN, extension = Ext}, rfc3966) ->
    Base = "tel:+" ++ integer_to_list(CC) ++ NN,
    add_extension(Base, Ext, ";ext=").

add_extension(Base, undefined, _) -> Base;
add_extension(Base, Ext, Prefix) -> Base ++ Prefix ++ Ext.

format_national_number(Digits, us) when length(Digits) == 10 ->
    [A, B, C, D, E, F, G, H, I, J] = Digits,
    [$( , A, B, C, $), $\s, D, E, F, $-, G, H, I, J];
format_national_number(Digits, ca) when length(Digits) == 10 ->
    format_national_number(Digits, us);
format_national_number(Digits, gb) ->
    %% UK format: XXXX XXX XXXX or similar
    case Digits of
        [A, B, C, D, E | Rest] when length(Rest) >= 5 ->
            [A, B, C, D, E, $\s] ++ Rest;
        _ ->
            Digits
    end;
format_national_number(Digits, _) ->
    %% Default: just space every 3-4 digits
    group_digits_phone(Digits).

group_digits_phone([]) -> [];
group_digits_phone(Digits) when length(Digits) =< 4 -> Digits;
group_digits_phone(Digits) ->
    {First, Rest} = lists:split(min(4, length(Digits)), Digits),
    First ++ [$\s | group_digits_phone(Rest)].

%% @doc Check if a phone number string is valid.
-spec is_valid(string() | binary()) -> boolean().
is_valid(Input) ->
    case parse(Input) of
        {ok, Phone} -> validate_phone(Phone);
        {error, _} -> false
    end.

%% @doc Check if a phone number string is valid for a specific country.
-spec is_valid(string() | binary(), country_code()) -> boolean().
is_valid(Input, Country) ->
    case parse(Input, Country) of
        {ok, Phone} -> validate_phone(Phone);
        {error, _} -> false
    end.

validate_phone(#phone_number{national_number = NN, region = Region}) ->
    {MinLen, MaxLen} = national_length_range(Region),
    Len = length(NN),
    Len >= MinLen andalso Len =< MaxLen.

%% @doc Normalize a phone number to E.164 format.
-spec normalize(string() | binary()) -> {ok, string()} | {error, term()}.
normalize(Input) ->
    case parse(Input) of
        {ok, Phone} -> {ok, format(Phone, e164)};
        Error -> Error
    end.

%% @doc Get the country code from a phone number.
-spec get_country_code(phone_number()) -> pos_integer().
get_country_code(#phone_number{country_code = CC}) -> CC.

%% @doc Get the national number from a phone number.
-spec get_national_number(phone_number()) -> string().
get_national_number(#phone_number{national_number = NN}) -> NN.

%% @doc Get the region (country) from a phone number.
-spec get_region(phone_number()) -> country_code().
get_region(#phone_number{region = Region}) -> Region.

%% @doc Get information about a country's phone system.
-spec country_info(country_code()) -> map().
country_info(Country) ->
    {MinLen, MaxLen} = national_length_range(Country),
    #{
        country => Country,
        dial_code => dial_code(Country),
        min_length => MinLen,
        max_length => MaxLen,
        trunk_prefix => trunk_prefix(Country)
    }.

trunk_prefix(us) -> "1";
trunk_prefix(gb) -> "0";
trunk_prefix(de) -> "0";
trunk_prefix(fr) -> "0";
trunk_prefix(au) -> "0";
trunk_prefix(jp) -> "0";
trunk_prefix(_) -> undefined.

%% @doc Compare two phone numbers for equality.
-spec compare(phone_number(), phone_number()) -> boolean().
compare(#phone_number{country_code = CC, national_number = NN1},
        #phone_number{country_code = CC, national_number = NN2}) ->
    NN1 == NN2;
compare(_, _) ->
    false.
