%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Erlang NIF bridge to libproven.
%% All computation happens in the Idris2-verified C library via Zig FFI.
%% This module loads the NIF shared library and exposes functions for
%% Gleam @external declarations.

-module(proven_nif).

-export([
    %% Runtime
    init/0,
    deinit/0,
    is_initialized/0,
    ffi_abi_version/0,

    %% SafeMath
    math_div/2,
    math_mod/2,
    math_add_checked/2,
    math_sub_checked/2,
    math_mul_checked/2,
    math_abs_safe/1,
    math_clamp/3,
    math_pow_checked/2,

    %% SafeString
    string_is_valid_utf8/1,
    string_escape_sql/1,
    string_escape_html/1,
    string_escape_js/1,

    %% SafePath
    path_has_traversal/1,
    path_sanitize_filename/1,

    %% SafeEmail
    email_is_valid/1,

    %% SafeNetwork
    network_parse_ipv4/1,
    network_ipv4_is_private/4,
    network_ipv4_is_loopback/4,

    %% SafeUrl
    url_parse/1,

    %% SafeCrypto
    crypto_constant_time_eq/2,
    crypto_random_bytes/1,

    %% SafeJson
    json_is_valid/1,
    json_get_type/1,

    %% SafeDateTime
    datetime_parse/1,
    datetime_format_iso8601/7,
    datetime_is_leap_year/1,
    datetime_days_in_month/2,

    %% SafeFloat
    float_div/2,
    float_sqrt/1,
    float_ln/1,
    float_is_finite/1,
    float_is_nan/1,

    %% SafeVersion
    version_parse/1,
    version_compare/6,

    %% SafeColor
    color_parse_hex/1,
    color_rgb_to_hsl/3,
    color_to_hex/3,

    %% SafeAngle
    angle_deg_to_rad/1,
    angle_rad_to_deg/1,
    angle_normalize_degrees/1,
    angle_normalize_radians/1,

    %% SafeUnit
    unit_convert_length/3,
    unit_convert_temp/3,

    %% SafeUuid
    uuid_v4_generate/0,
    uuid_parse/1,
    uuid_to_string/1,
    uuid_to_urn/1,
    uuid_is_nil/1,
    uuid_is_valid/1,
    uuid_get_version/1,
    uuid_get_variant/1,
    uuid_equals/2,

    %% SafeHex
    hex_encode/1,
    hex_encode_upper/1,
    hex_decode/1,
    hex_is_valid/1,
    hex_is_valid_bytes/1,
    hex_constant_time_eq/2,
    hex_to_int/1,
    hex_from_int/2,

    %% SafeCurrency
    currency_parse_code/1,
    currency_is_valid_code/1,
    currency_get_decimals/1,
    currency_get_symbol/1,
    currency_get_name/1,
    money_from_major/2,
    money_from_minor/2,
    money_add/4,
    money_sub/4,
    money_mul/3,
    money_div/3,
    money_format/2,

    %% SafePhone
    phone_parse/1,
    phone_is_valid/1,
    phone_format_e164/2,
    phone_format_international/2
]).

-on_load(load_nif/0).

load_nif() ->
    PrivDir = code:priv_dir(proven),
    ok = erlang:load_nif(filename:join(PrivDir, "libproven_nif"), 0).

%% ============================================================================
%% Runtime Management
%% ============================================================================

init() -> erlang:nif_error(not_loaded).
deinit() -> erlang:nif_error(not_loaded).
is_initialized() -> erlang:nif_error(not_loaded).
ffi_abi_version() -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeMath
%% ============================================================================

math_div(_Numerator, _Denominator) -> erlang:nif_error(not_loaded).
math_mod(_Numerator, _Denominator) -> erlang:nif_error(not_loaded).
math_add_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_sub_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_mul_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_abs_safe(_N) -> erlang:nif_error(not_loaded).
math_clamp(_Lo, _Hi, _Value) -> erlang:nif_error(not_loaded).
math_pow_checked(_Base, _Exp) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeString
%% ============================================================================

string_is_valid_utf8(_Bin) -> erlang:nif_error(not_loaded).
string_escape_sql(_Bin) -> erlang:nif_error(not_loaded).
string_escape_html(_Bin) -> erlang:nif_error(not_loaded).
string_escape_js(_Bin) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafePath
%% ============================================================================

path_has_traversal(_PathBin) -> erlang:nif_error(not_loaded).
path_sanitize_filename(_FilenameBin) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeEmail
%% ============================================================================

email_is_valid(_EmailBin) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeNetwork
%% ============================================================================

network_parse_ipv4(_AddrBin) -> erlang:nif_error(not_loaded).
network_ipv4_is_private(_A, _B, _C, _D) -> erlang:nif_error(not_loaded).
network_ipv4_is_loopback(_A, _B, _C, _D) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeUrl
%% ============================================================================

url_parse(_UrlBin) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeCrypto
%% ============================================================================

crypto_constant_time_eq(_Bin1, _Bin2) -> erlang:nif_error(not_loaded).
crypto_random_bytes(_Size) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeJson
%% ============================================================================

json_is_valid(_JsonBin) -> erlang:nif_error(not_loaded).
json_get_type(_JsonBin) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeDateTime
%% ============================================================================

datetime_parse(_DatetimeBin) -> erlang:nif_error(not_loaded).
datetime_format_iso8601(_Year, _Month, _Day, _Hour, _Minute, _Second, _TzOffset) ->
    erlang:nif_error(not_loaded).
datetime_is_leap_year(_Year) -> erlang:nif_error(not_loaded).
datetime_days_in_month(_Year, _Month) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeFloat
%% ============================================================================

float_div(_A, _B) -> erlang:nif_error(not_loaded).
float_sqrt(_X) -> erlang:nif_error(not_loaded).
float_ln(_X) -> erlang:nif_error(not_loaded).
float_is_finite(_X) -> erlang:nif_error(not_loaded).
float_is_nan(_X) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeVersion
%% ============================================================================

version_parse(_VersionBin) -> erlang:nif_error(not_loaded).
version_compare(_MajorA, _MinorA, _PatchA, _MajorB, _MinorB, _PatchB) ->
    erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeColor
%% ============================================================================

color_parse_hex(_HexBin) -> erlang:nif_error(not_loaded).
color_rgb_to_hsl(_R, _G, _B) -> erlang:nif_error(not_loaded).
color_to_hex(_R, _G, _B) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeAngle
%% ============================================================================

angle_deg_to_rad(_Degrees) -> erlang:nif_error(not_loaded).
angle_rad_to_deg(_Radians) -> erlang:nif_error(not_loaded).
angle_normalize_degrees(_Degrees) -> erlang:nif_error(not_loaded).
angle_normalize_radians(_Radians) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeUnit
%% ============================================================================

unit_convert_length(_Value, _From, _To) -> erlang:nif_error(not_loaded).
unit_convert_temp(_Value, _From, _To) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeUuid
%% ============================================================================

uuid_v4_generate() -> erlang:nif_error(not_loaded).
uuid_parse(_UuidBin) -> erlang:nif_error(not_loaded).
uuid_to_string(_Bytes) -> erlang:nif_error(not_loaded).
uuid_to_urn(_Bytes) -> erlang:nif_error(not_loaded).
uuid_is_nil(_Bytes) -> erlang:nif_error(not_loaded).
uuid_is_valid(_UuidBin) -> erlang:nif_error(not_loaded).
uuid_get_version(_Bytes) -> erlang:nif_error(not_loaded).
uuid_get_variant(_Bytes) -> erlang:nif_error(not_loaded).
uuid_equals(_BytesA, _BytesB) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeHex
%% ============================================================================

hex_encode(_Bytes) -> erlang:nif_error(not_loaded).
hex_encode_upper(_Bytes) -> erlang:nif_error(not_loaded).
hex_decode(_HexBin) -> erlang:nif_error(not_loaded).
hex_is_valid(_HexBin) -> erlang:nif_error(not_loaded).
hex_is_valid_bytes(_HexBin) -> erlang:nif_error(not_loaded).
hex_constant_time_eq(_HexA, _HexB) -> erlang:nif_error(not_loaded).
hex_to_int(_HexBin) -> erlang:nif_error(not_loaded).
hex_from_int(_Value, _MinWidth) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafeCurrency
%% ============================================================================

currency_parse_code(_CodeBin) -> erlang:nif_error(not_loaded).
currency_is_valid_code(_CodeBin) -> erlang:nif_error(not_loaded).
currency_get_decimals(_Code) -> erlang:nif_error(not_loaded).
currency_get_symbol(_Code) -> erlang:nif_error(not_loaded).
currency_get_name(_Code) -> erlang:nif_error(not_loaded).
money_from_major(_Amount, _Currency) -> erlang:nif_error(not_loaded).
money_from_minor(_Amount, _Currency) -> erlang:nif_error(not_loaded).
money_add(_MinorA, _CurrencyA, _MinorB, _CurrencyB) -> erlang:nif_error(not_loaded).
money_sub(_MinorA, _CurrencyA, _MinorB, _CurrencyB) -> erlang:nif_error(not_loaded).
money_mul(_Minor, _Currency, _Scalar) -> erlang:nif_error(not_loaded).
money_div(_Minor, _Currency, _Divisor) -> erlang:nif_error(not_loaded).
money_format(_Minor, _Currency) -> erlang:nif_error(not_loaded).

%% ============================================================================
%% SafePhone
%% ============================================================================

phone_parse(_PhoneBin) -> erlang:nif_error(not_loaded).
phone_is_valid(_PhoneBin) -> erlang:nif_error(not_loaded).
phone_format_e164(_CountryCode, _NationalNumber) -> erlang:nif_error(not_loaded).
phone_format_international(_CountryCode, _NationalNumber) -> erlang:nif_error(not_loaded).
