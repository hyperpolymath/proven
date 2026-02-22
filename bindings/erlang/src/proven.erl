%% SPDX-License-Identifier: PMPL-1.0-or-later
%% Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
%%
%% Proven - Erlang NIF bindings to libproven.
%%
%% This module is a THIN FFI WRAPPER. All computation is performed in
%% formally verified Idris 2 code via the Zig FFI bridge (libproven).
%% No logic is reimplemented in Erlang.
%%
%% Each exported function is a NIF stub that gets replaced at load time
%% by the C NIF implementation in c_src/proven_nif.c.

-module(proven).
-on_load(load_nif/0).

%% ============================================================================
%% Lifecycle & Version (8 functions)
%% ============================================================================
-export([
    init/0,
    deinit/0,
    is_initialized/0,
    ffi_abi_version/0,
    version_major/0,
    version_minor/0,
    version_patch/0,
    module_count/0
]).

%% ============================================================================
%% Memory management (1 function - internal, called by NIF C code)
%% ============================================================================
%% proven_free_string is handled internally by the NIF layer;
%% Erlang callers never need to call it directly.

%% ============================================================================
%% SafeMath (8 functions)
%% ============================================================================
-export([
    math_div/2,
    math_mod/2,
    math_add_checked/2,
    math_sub_checked/2,
    math_mul_checked/2,
    math_abs_safe/1,
    math_clamp/3,
    math_pow_checked/2
]).

%% ============================================================================
%% SafeString (4 functions)
%% ============================================================================
-export([
    string_is_valid_utf8/1,
    string_escape_sql/1,
    string_escape_html/1,
    string_escape_js/1
]).

%% ============================================================================
%% SafePath (2 functions)
%% ============================================================================
-export([
    path_has_traversal/1,
    path_sanitize_filename/1
]).

%% ============================================================================
%% SafeCrypto (2 functions)
%% ============================================================================
-export([
    crypto_constant_time_eq/2,
    crypto_random_bytes/1
]).

%% ============================================================================
%% SafeUrl (2 functions)
%% ============================================================================
-export([
    url_parse/1,
    url_free/1
]).

%% ============================================================================
%% SafeEmail (1 function)
%% ============================================================================
-export([
    email_is_valid/1
]).

%% ============================================================================
%% SafeNetwork (3 functions)
%% ============================================================================
-export([
    network_parse_ipv4/1,
    network_ipv4_is_private/1,
    network_ipv4_is_loopback/1
]).

%% ============================================================================
%% SafeHeader (6 functions)
%% ============================================================================
-export([
    header_has_crlf/1,
    header_is_valid_name/1,
    header_is_dangerous/1,
    header_render/2,
    header_build_csp/1,
    header_build_hsts/3
]).

%% ============================================================================
%% SafeCookie (6 functions)
%% ============================================================================
-export([
    cookie_has_injection/1,
    cookie_validate_name/1,
    cookie_validate_value/1,
    cookie_get_prefix/1,
    cookie_build_set_cookie/3,
    cookie_build_delete/1
]).

%% ============================================================================
%% SafeContentType (6 functions)
%% ============================================================================
-export([
    content_type_parse/1,
    content_type_free/1,
    content_type_can_sniff_dangerous/1,
    content_type_render/4,
    content_type_is_json/2,
    content_type_is_xml/2
]).

%% ============================================================================
%% SafeUUID (5 functions)
%% ============================================================================
-export([
    uuid_v4/0,
    uuid_to_string/1,
    uuid_parse/1,
    uuid_is_nil/1,
    uuid_version/1
]).

%% ============================================================================
%% SafeJson (2 functions)
%% ============================================================================
-export([
    json_is_valid/1,
    json_get_type/1
]).

%% ============================================================================
%% SafeDateTime (4 functions)
%% ============================================================================
-export([
    datetime_parse/1,
    datetime_format_iso8601/1,
    datetime_is_leap_year/1,
    datetime_days_in_month/2
]).

%% ============================================================================
%% SafeFloat (5 functions)
%% ============================================================================
-export([
    float_div/2,
    float_is_finite/1,
    float_is_nan/1,
    float_sqrt/1,
    float_ln/1
]).

%% ============================================================================
%% SafePassword (2 functions)
%% ============================================================================
-export([
    password_validate/1,
    password_is_common/1
]).

%% ============================================================================
%% SafeHex (3 functions)
%% ============================================================================
-export([
    hex_encode/2,
    hex_decode/1,
    hex_free/1
]).

%% ============================================================================
%% SafeCurrency (2 functions)
%% ============================================================================
-export([
    currency_parse/1,
    currency_format/3
]).

%% ============================================================================
%% SafePhone (2 functions)
%% ============================================================================
-export([
    phone_parse/1,
    phone_format_e164/2
]).

%% ============================================================================
%% SafeVersion (3 functions)
%% ============================================================================
-export([
    version_parse/1,
    version_compare/2,
    version_free/1
]).

%% ============================================================================
%% SafeGeo (3 functions)
%% ============================================================================
-export([
    geo_validate/2,
    geo_distance/2,
    geo_in_bounds/5
]).

%% ============================================================================
%% SafeChecksum (2 functions)
%% ============================================================================
-export([
    checksum_crc32/1,
    checksum_verify_crc32/2
]).

%% ============================================================================
%% SafeProbability (4 functions)
%% ============================================================================
-export([
    probability_create/1,
    probability_and/2,
    probability_or_exclusive/2,
    probability_not/1
]).

%% ============================================================================
%% SafeCalculator (1 function)
%% ============================================================================
-export([
    calculator_eval/1
]).

%% ============================================================================
%% SafeBuffer (4 functions)
%% ============================================================================
-export([
    buffer_create/1,
    buffer_append/2,
    buffer_get/1,
    buffer_free/1
]).

%% ============================================================================
%% SafeRateLimiter (3 functions)
%% ============================================================================
-export([
    rate_limiter_create/2,
    rate_limiter_try_acquire/2,
    rate_limiter_free/1
]).

%% ============================================================================
%% SafeCircuitBreaker (6 functions)
%% ============================================================================
-export([
    circuit_breaker_create/3,
    circuit_breaker_allow/1,
    circuit_breaker_success/1,
    circuit_breaker_failure/1,
    circuit_breaker_state/1,
    circuit_breaker_free/1
]).

%% ============================================================================
%% SafeRetry (2 functions)
%% ============================================================================
-export([
    retry_delay/2,
    retry_should_retry/2
]).

%% ============================================================================
%% SafeMonotonic (3 functions)
%% ============================================================================
-export([
    monotonic_create/2,
    monotonic_next/1,
    monotonic_free/1
]).

%% ============================================================================
%% SafeStateMachine (5 functions)
%% ============================================================================
-export([
    state_machine_create/2,
    state_machine_allow/3,
    state_machine_transition/2,
    state_machine_state/1,
    state_machine_free/1
]).

%% ============================================================================
%% SafeColor (3 functions)
%% ============================================================================
-export([
    color_parse_hex/1,
    color_rgb_to_hsl/1,
    color_to_hex/1
]).

%% ============================================================================
%% SafeAngle (4 functions)
%% ============================================================================
-export([
    angle_deg_to_rad/1,
    angle_rad_to_deg/1,
    angle_normalize_degrees/1,
    angle_normalize_radians/1
]).

%% ============================================================================
%% SafeUnit (2 functions)
%% ============================================================================
-export([
    unit_convert_length/3,
    unit_convert_temp/3
]).

%% ============================================================================
%% SafeQueue (5 functions)
%% ============================================================================
-export([
    queue_create/1,
    queue_push/2,
    queue_pop/1,
    queue_size/1,
    queue_free/1
]).

%% ============================================================================
%% SafeBloom (4 functions)
%% ============================================================================
-export([
    bloom_create/2,
    bloom_add/2,
    bloom_contains/2,
    bloom_free/1
]).

%% ============================================================================
%% SafeTensor (5 functions)
%% ============================================================================
-export([
    tensor_create/2,
    tensor_set/4,
    tensor_get/3,
    tensor_matmul/2,
    tensor_free/1
]).

%% ============================================================================
%% SafeMl (5 functions)
%% ============================================================================
-export([
    ml_softmax/1,
    ml_sigmoid/1,
    ml_relu/1,
    ml_leaky_relu/2,
    ml_clamp/3
]).

%% ============================================================================
%% SafeLru (4 functions)
%% ============================================================================
-export([
    lru_create/1,
    lru_get/2,
    lru_put/3,
    lru_free/1
]).

%% ============================================================================
%% SafeGraph (4 functions)
%% ============================================================================
-export([
    graph_create/1,
    graph_add_edge/3,
    graph_has_edge/3,
    graph_free/1
]).

%% ============================================================================
%% SafeHttp (3 functions)
%% ============================================================================
-export([
    http_url_encode/1,
    http_url_decode/1,
    http_parse_www_authenticate/1
]).

%% ============================================================================
%% NIF loading
%% ============================================================================

load_nif() ->
    PrivDir = case code:priv_dir(proven) of
        {error, _} ->
            %% Fallback: look relative to beam dir
            EbinDir = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(EbinDir), "priv");
        Dir -> Dir
    end,
    erlang:load_nif(filename:join(PrivDir, "proven_nif"), 0).

%% ============================================================================
%% NIF stubs - replaced at load time by C implementations
%%
%% Each stub returns erlang:nif_error(not_loaded). If you see this error at
%% runtime, it means the NIF shared library failed to load.
%% ============================================================================

%% Lifecycle & Version
init() -> erlang:nif_error(not_loaded).
deinit() -> erlang:nif_error(not_loaded).
is_initialized() -> erlang:nif_error(not_loaded).
ffi_abi_version() -> erlang:nif_error(not_loaded).
version_major() -> erlang:nif_error(not_loaded).
version_minor() -> erlang:nif_error(not_loaded).
version_patch() -> erlang:nif_error(not_loaded).
module_count() -> erlang:nif_error(not_loaded).

%% SafeMath
math_div(_Numerator, _Denominator) -> erlang:nif_error(not_loaded).
math_mod(_Numerator, _Denominator) -> erlang:nif_error(not_loaded).
math_add_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_sub_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_mul_checked(_A, _B) -> erlang:nif_error(not_loaded).
math_abs_safe(_N) -> erlang:nif_error(not_loaded).
math_clamp(_Lo, _Hi, _Value) -> erlang:nif_error(not_loaded).
math_pow_checked(_Base, _Exp) -> erlang:nif_error(not_loaded).

%% SafeString
string_is_valid_utf8(_Bin) -> erlang:nif_error(not_loaded).
string_escape_sql(_Bin) -> erlang:nif_error(not_loaded).
string_escape_html(_Bin) -> erlang:nif_error(not_loaded).
string_escape_js(_Bin) -> erlang:nif_error(not_loaded).

%% SafePath
path_has_traversal(_Bin) -> erlang:nif_error(not_loaded).
path_sanitize_filename(_Bin) -> erlang:nif_error(not_loaded).

%% SafeCrypto
crypto_constant_time_eq(_Bin1, _Bin2) -> erlang:nif_error(not_loaded).
crypto_random_bytes(_Len) -> erlang:nif_error(not_loaded).

%% SafeUrl
url_parse(_Bin) -> erlang:nif_error(not_loaded).
url_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeEmail
email_is_valid(_Bin) -> erlang:nif_error(not_loaded).

%% SafeNetwork
network_parse_ipv4(_Bin) -> erlang:nif_error(not_loaded).
network_ipv4_is_private(_Octets) -> erlang:nif_error(not_loaded).
network_ipv4_is_loopback(_Octets) -> erlang:nif_error(not_loaded).

%% SafeHeader
header_has_crlf(_Bin) -> erlang:nif_error(not_loaded).
header_is_valid_name(_Bin) -> erlang:nif_error(not_loaded).
header_is_dangerous(_Bin) -> erlang:nif_error(not_loaded).
header_render(_NameBin, _ValueBin) -> erlang:nif_error(not_loaded).
header_build_csp(_JsonBin) -> erlang:nif_error(not_loaded).
header_build_hsts(_MaxAge, _IncludeSubdomains, _Preload) -> erlang:nif_error(not_loaded).

%% SafeCookie
cookie_has_injection(_Bin) -> erlang:nif_error(not_loaded).
cookie_validate_name(_Bin) -> erlang:nif_error(not_loaded).
cookie_validate_value(_Bin) -> erlang:nif_error(not_loaded).
cookie_get_prefix(_Bin) -> erlang:nif_error(not_loaded).
cookie_build_set_cookie(_NameBin, _ValueBin, _Attrs) -> erlang:nif_error(not_loaded).
cookie_build_delete(_NameBin) -> erlang:nif_error(not_loaded).

%% SafeContentType
content_type_parse(_Bin) -> erlang:nif_error(not_loaded).
content_type_free(_Ref) -> erlang:nif_error(not_loaded).
content_type_can_sniff_dangerous(_Bin) -> erlang:nif_error(not_loaded).
content_type_render(_TypeBin, _SubtypeBin, _SuffixBin, _Charset) -> erlang:nif_error(not_loaded).
content_type_is_json(_SubtypeBin, _SuffixBin) -> erlang:nif_error(not_loaded).
content_type_is_xml(_SubtypeBin, _SuffixBin) -> erlang:nif_error(not_loaded).

%% SafeUUID
uuid_v4() -> erlang:nif_error(not_loaded).
uuid_to_string(_UuidBin) -> erlang:nif_error(not_loaded).
uuid_parse(_Bin) -> erlang:nif_error(not_loaded).
uuid_is_nil(_UuidBin) -> erlang:nif_error(not_loaded).
uuid_version(_UuidBin) -> erlang:nif_error(not_loaded).

%% SafeJson
json_is_valid(_Bin) -> erlang:nif_error(not_loaded).
json_get_type(_Bin) -> erlang:nif_error(not_loaded).

%% SafeDateTime
datetime_parse(_Bin) -> erlang:nif_error(not_loaded).
datetime_format_iso8601(_DatetimeMap) -> erlang:nif_error(not_loaded).
datetime_is_leap_year(_Year) -> erlang:nif_error(not_loaded).
datetime_days_in_month(_Year, _Month) -> erlang:nif_error(not_loaded).

%% SafeFloat
float_div(_A, _B) -> erlang:nif_error(not_loaded).
float_is_finite(_X) -> erlang:nif_error(not_loaded).
float_is_nan(_X) -> erlang:nif_error(not_loaded).
float_sqrt(_X) -> erlang:nif_error(not_loaded).
float_ln(_X) -> erlang:nif_error(not_loaded).

%% SafePassword
password_validate(_Bin) -> erlang:nif_error(not_loaded).
password_is_common(_Bin) -> erlang:nif_error(not_loaded).

%% SafeHex
hex_encode(_Bin, _Uppercase) -> erlang:nif_error(not_loaded).
hex_decode(_Bin) -> erlang:nif_error(not_loaded).
hex_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeCurrency
currency_parse(_Bin) -> erlang:nif_error(not_loaded).
currency_format(_AmountMinor, _Code, _DecimalPlaces) -> erlang:nif_error(not_loaded).

%% SafePhone
phone_parse(_Bin) -> erlang:nif_error(not_loaded).
phone_format_e164(_CountryCode, _NationalNumber) -> erlang:nif_error(not_loaded).

%% SafeVersion
version_parse(_Bin) -> erlang:nif_error(not_loaded).
version_compare(_VersionA, _VersionB) -> erlang:nif_error(not_loaded).
version_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeGeo
geo_validate(_Lat, _Lon) -> erlang:nif_error(not_loaded).
geo_distance(_CoordA, _CoordB) -> erlang:nif_error(not_loaded).
geo_in_bounds(_Coord, _MinLat, _MaxLat, _MinLon, _MaxLon) -> erlang:nif_error(not_loaded).

%% SafeChecksum
checksum_crc32(_Bin) -> erlang:nif_error(not_loaded).
checksum_verify_crc32(_Bin, _Expected) -> erlang:nif_error(not_loaded).

%% SafeProbability
probability_create(_Value) -> erlang:nif_error(not_loaded).
probability_and(_A, _B) -> erlang:nif_error(not_loaded).
probability_or_exclusive(_A, _B) -> erlang:nif_error(not_loaded).
probability_not(_P) -> erlang:nif_error(not_loaded).

%% SafeCalculator
calculator_eval(_Bin) -> erlang:nif_error(not_loaded).

%% SafeBuffer
buffer_create(_Capacity) -> erlang:nif_error(not_loaded).
buffer_append(_Ref, _Bin) -> erlang:nif_error(not_loaded).
buffer_get(_Ref) -> erlang:nif_error(not_loaded).
buffer_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeRateLimiter
rate_limiter_create(_Capacity, _RefillRate) -> erlang:nif_error(not_loaded).
rate_limiter_try_acquire(_Ref, _Tokens) -> erlang:nif_error(not_loaded).
rate_limiter_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeCircuitBreaker
circuit_breaker_create(_FailThresh, _SuccThresh, _TimeoutMs) -> erlang:nif_error(not_loaded).
circuit_breaker_allow(_Ref) -> erlang:nif_error(not_loaded).
circuit_breaker_success(_Ref) -> erlang:nif_error(not_loaded).
circuit_breaker_failure(_Ref) -> erlang:nif_error(not_loaded).
circuit_breaker_state(_Ref) -> erlang:nif_error(not_loaded).
circuit_breaker_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeRetry
retry_delay(_Config, _Attempt) -> erlang:nif_error(not_loaded).
retry_should_retry(_Config, _Attempt) -> erlang:nif_error(not_loaded).

%% SafeMonotonic
monotonic_create(_Initial, _MaxValue) -> erlang:nif_error(not_loaded).
monotonic_next(_Ref) -> erlang:nif_error(not_loaded).
monotonic_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeStateMachine
state_machine_create(_StateCount, _InitialState) -> erlang:nif_error(not_loaded).
state_machine_allow(_Ref, _From, _To) -> erlang:nif_error(not_loaded).
state_machine_transition(_Ref, _To) -> erlang:nif_error(not_loaded).
state_machine_state(_Ref) -> erlang:nif_error(not_loaded).
state_machine_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeColor
color_parse_hex(_Bin) -> erlang:nif_error(not_loaded).
color_rgb_to_hsl(_Rgb) -> erlang:nif_error(not_loaded).
color_to_hex(_Rgb) -> erlang:nif_error(not_loaded).

%% SafeAngle
angle_deg_to_rad(_Degrees) -> erlang:nif_error(not_loaded).
angle_rad_to_deg(_Radians) -> erlang:nif_error(not_loaded).
angle_normalize_degrees(_Degrees) -> erlang:nif_error(not_loaded).
angle_normalize_radians(_Radians) -> erlang:nif_error(not_loaded).

%% SafeUnit
unit_convert_length(_Value, _From, _To) -> erlang:nif_error(not_loaded).
unit_convert_temp(_Value, _From, _To) -> erlang:nif_error(not_loaded).

%% SafeQueue
queue_create(_Capacity) -> erlang:nif_error(not_loaded).
queue_push(_Ref, _Value) -> erlang:nif_error(not_loaded).
queue_pop(_Ref) -> erlang:nif_error(not_loaded).
queue_size(_Ref) -> erlang:nif_error(not_loaded).
queue_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeBloom
bloom_create(_ExpectedElements, _FalsePositiveRate) -> erlang:nif_error(not_loaded).
bloom_add(_Ref, _Bin) -> erlang:nif_error(not_loaded).
bloom_contains(_Ref, _Bin) -> erlang:nif_error(not_loaded).
bloom_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeTensor
tensor_create(_Rows, _Cols) -> erlang:nif_error(not_loaded).
tensor_set(_Ref, _Row, _Col, _Value) -> erlang:nif_error(not_loaded).
tensor_get(_Ref, _Row, _Col) -> erlang:nif_error(not_loaded).
tensor_matmul(_RefA, _RefB) -> erlang:nif_error(not_loaded).
tensor_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeMl
ml_softmax(_InputList) -> erlang:nif_error(not_loaded).
ml_sigmoid(_X) -> erlang:nif_error(not_loaded).
ml_relu(_X) -> erlang:nif_error(not_loaded).
ml_leaky_relu(_X, _Alpha) -> erlang:nif_error(not_loaded).
ml_clamp(_X, _Min, _Max) -> erlang:nif_error(not_loaded).

%% SafeLru
lru_create(_Capacity) -> erlang:nif_error(not_loaded).
lru_get(_Ref, _Key) -> erlang:nif_error(not_loaded).
lru_put(_Ref, _Key, _Value) -> erlang:nif_error(not_loaded).
lru_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeGraph
graph_create(_NodeCount) -> erlang:nif_error(not_loaded).
graph_add_edge(_Ref, _From, _To) -> erlang:nif_error(not_loaded).
graph_has_edge(_Ref, _From, _To) -> erlang:nif_error(not_loaded).
graph_free(_Ref) -> erlang:nif_error(not_loaded).

%% SafeHttp
http_url_encode(_Bin) -> erlang:nif_error(not_loaded).
http_url_decode(_Bin) -> erlang:nif_error(not_loaded).
http_parse_www_authenticate(_Bin) -> erlang:nif_error(not_loaded).
