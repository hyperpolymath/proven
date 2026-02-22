(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> *)

(** Low-level ctypes FFI bindings to libproven.

    This module declares all C-ABI types and function bindings using
    [Ctypes] and [Ctypes.Foreign].  Every function here is a direct
    1-to-1 mapping of a [proven_*] export from the shared library.

    Users should prefer the safe wrappers in {!Proven} instead. *)

open Ctypes
open Foreign

(* -------------------------------------------------------------------------- *)
(* Library handle                                                             *)
(* -------------------------------------------------------------------------- *)

let lib = Dl.dlopen ~filename:"libproven.so" ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

(* -------------------------------------------------------------------------- *)
(* Enum: ProvenStatus                                                         *)
(* -------------------------------------------------------------------------- *)

(** Status codes returned by most libproven functions. *)
let proven_status_ok                = 0l
let proven_status_err_null_pointer  = -1l
let proven_status_err_invalid_arg   = -2l
let proven_status_err_overflow      = -3l
let proven_status_err_underflow     = -4l
let proven_status_err_div_by_zero   = -5l
let proven_status_err_parse_failure = -6l
let proven_status_err_validation    = -7l
let proven_status_err_out_of_bounds = -8l
let proven_status_err_encoding      = -9l
let proven_status_err_alloc_failed  = -10l

(* -------------------------------------------------------------------------- *)
(* Struct: IntResult  { status: i32, value: i64 }                             *)
(* -------------------------------------------------------------------------- *)

type int_result
let int_result : int_result structure typ = structure "IntResult"
let ir_status = field int_result "status" int32_t
let ir_value  = field int_result "value"  int64_t
let () = seal int_result

(* -------------------------------------------------------------------------- *)
(* Struct: BoolResult { status: i32, value: bool }                            *)
(* -------------------------------------------------------------------------- *)

type bool_result
let bool_result : bool_result structure typ = structure "BoolResult"
let br_status = field bool_result "status" int32_t
let br_value  = field bool_result "value"  bool
let () = seal bool_result

(* -------------------------------------------------------------------------- *)
(* Struct: StringResult { status: i32, value: *char, length: size_t }         *)
(* -------------------------------------------------------------------------- *)

type string_result
let string_result : string_result structure typ = structure "StringResult"
let sr_status = field string_result "status" int32_t
let sr_value  = field string_result "value"  (ptr char)
let sr_length = field string_result "length" size_t
let () = seal string_result

(* -------------------------------------------------------------------------- *)
(* Struct: FloatResult { status: i32, value: f64 }                            *)
(* -------------------------------------------------------------------------- *)

type float_result
let float_result : float_result structure typ = structure "FloatResult"
let fr_status = field float_result "status" int32_t
let fr_value  = field float_result "value"  double
let () = seal float_result

(* -------------------------------------------------------------------------- *)
(* Struct: UUID { bytes: u8[16] }                                             *)
(* -------------------------------------------------------------------------- *)

type uuid
let uuid : uuid structure typ = structure "UUID"
let uuid_bytes = field uuid "bytes" (array 16 uint8_t)
let () = seal uuid

(* -------------------------------------------------------------------------- *)
(* Struct: UUIDResult { status: i32, uuid: UUID }                             *)
(* -------------------------------------------------------------------------- *)

type uuid_result
let uuid_result : uuid_result structure typ = structure "UUIDResult"
let ur_status = field uuid_result "status" int32_t
let ur_uuid   = field uuid_result "uuid"   uuid
let () = seal uuid_result

(* -------------------------------------------------------------------------- *)
(* Struct: IPv4Address { octets: u8[4] }                                      *)
(* -------------------------------------------------------------------------- *)

type ipv4_address
let ipv4_address : ipv4_address structure typ = structure "IPv4Address"
let ipv4_octets = field ipv4_address "octets" (array 4 uint8_t)
let () = seal ipv4_address

(* -------------------------------------------------------------------------- *)
(* Struct: IPv4Result { status: i32, address: IPv4Address }                   *)
(* -------------------------------------------------------------------------- *)

type ipv4_result
let ipv4_result : ipv4_result structure typ = structure "IPv4Result"
let ipv4r_status  = field ipv4_result "status"  int32_t
let ipv4r_address = field ipv4_result "address" ipv4_address
let () = seal ipv4_result

(* -------------------------------------------------------------------------- *)
(* Struct: CurrencyResult                                                     *)
(* -------------------------------------------------------------------------- *)

type currency_result
let currency_result : currency_result structure typ = structure "CurrencyResult"
let cur_status         = field currency_result "status"         int32_t
let cur_amount_minor   = field currency_result "amount_minor"   int64_t
let cur_currency_code  = field currency_result "currency_code"  (array 3 uint8_t)
let cur_decimal_places = field currency_result "decimal_places" uint8_t
let () = seal currency_result

(* -------------------------------------------------------------------------- *)
(* Struct: PhoneResult                                                        *)
(* -------------------------------------------------------------------------- *)

type phone_result
let phone_result : phone_result structure typ = structure "PhoneResult"
let phr_status          = field phone_result "status"          int32_t
let phr_country_code    = field phone_result "country_code"    uint16_t
let phr_national_number = field phone_result "national_number" uint64_t
let phr_is_valid        = field phone_result "is_valid"        bool
let () = seal phone_result

(* -------------------------------------------------------------------------- *)
(* Struct: DateTime                                                           *)
(* -------------------------------------------------------------------------- *)

type datetime
let datetime : datetime structure typ = structure "DateTime"
let dt_year              = field datetime "year"              int32_t
let dt_month             = field datetime "month"             uint8_t
let dt_day               = field datetime "day"               uint8_t
let dt_hour              = field datetime "hour"              uint8_t
let dt_minute            = field datetime "minute"            uint8_t
let dt_second            = field datetime "second"            uint8_t
let dt_nanosecond        = field datetime "nanosecond"        uint32_t
let dt_tz_offset_minutes = field datetime "tz_offset_minutes" int16_t
let () = seal datetime

(* -------------------------------------------------------------------------- *)
(* Struct: DateTimeResult                                                     *)
(* -------------------------------------------------------------------------- *)

type datetime_result
let datetime_result : datetime_result structure typ = structure "DateTimeResult"
let dtr_status   = field datetime_result "status"   int32_t
let dtr_datetime = field datetime_result "datetime" datetime
let () = seal datetime_result

(* -------------------------------------------------------------------------- *)
(* Struct: SemanticVersion                                                    *)
(* -------------------------------------------------------------------------- *)

type semantic_version
let semantic_version : semantic_version structure typ = structure "SemanticVersion"
let sv_major          = field semantic_version "major"          uint32_t
let sv_minor          = field semantic_version "minor"          uint32_t
let sv_patch          = field semantic_version "patch"          uint32_t
let sv_prerelease_len = field semantic_version "prerelease_len" size_t
let sv_prerelease     = field semantic_version "prerelease"     (ptr char)
let () = seal semantic_version

(* -------------------------------------------------------------------------- *)
(* Struct: VersionResult                                                      *)
(* -------------------------------------------------------------------------- *)

type version_result
let version_result : version_result structure typ = structure "VersionResult"
let vr_status  = field version_result "status"  int32_t
let vr_version = field version_result "version" semantic_version
let () = seal version_result

(* -------------------------------------------------------------------------- *)
(* Struct: GeoCoordinate                                                      *)
(* -------------------------------------------------------------------------- *)

type geo_coordinate
let geo_coordinate : geo_coordinate structure typ = structure "GeoCoordinate"
let geo_latitude  = field geo_coordinate "latitude"  double
let geo_longitude = field geo_coordinate "longitude" double
let () = seal geo_coordinate

(* -------------------------------------------------------------------------- *)
(* Struct: GeoResult                                                          *)
(* -------------------------------------------------------------------------- *)

type geo_result
let geo_result : geo_result structure typ = structure "GeoResult"
let geor_status     = field geo_result "status"     int32_t
let geor_coordinate = field geo_result "coordinate" geo_coordinate
let () = seal geo_result

(* -------------------------------------------------------------------------- *)
(* Struct: PasswordResult                                                     *)
(* -------------------------------------------------------------------------- *)

type password_result
let password_result : password_result structure typ = structure "PasswordResult"
let pw_strength      = field password_result "strength"      int32_t
let pw_has_lowercase = field password_result "has_lowercase" bool
let pw_has_uppercase = field password_result "has_uppercase" bool
let pw_has_digit     = field password_result "has_digit"     bool
let pw_has_special   = field password_result "has_special"   bool
let pw_length        = field password_result "length"        size_t
let () = seal password_result

(* -------------------------------------------------------------------------- *)
(* Struct: RGBColor                                                           *)
(* -------------------------------------------------------------------------- *)

type rgb_color
let rgb_color : rgb_color structure typ = structure "RGBColor"
let rgb_r = field rgb_color "r" uint8_t
let rgb_g = field rgb_color "g" uint8_t
let rgb_b = field rgb_color "b" uint8_t
let () = seal rgb_color

(* -------------------------------------------------------------------------- *)
(* Struct: HSLColor                                                           *)
(* -------------------------------------------------------------------------- *)

type hsl_color
let hsl_color : hsl_color structure typ = structure "HSLColor"
let hsl_h = field hsl_color "h" double
let hsl_s = field hsl_color "s" double
let hsl_l = field hsl_color "l" double
let () = seal hsl_color

(* -------------------------------------------------------------------------- *)
(* Struct: ColorParseResult                                                   *)
(* -------------------------------------------------------------------------- *)

type color_parse_result
let color_parse_result : color_parse_result structure typ = structure "ColorParseResult"
let cpr_status = field color_parse_result "status" int32_t
let cpr_color  = field color_parse_result "color"  rgb_color
let () = seal color_parse_result

(* -------------------------------------------------------------------------- *)
(* Struct: HexDecodeResult                                                    *)
(* -------------------------------------------------------------------------- *)

type hex_decode_result
let hex_decode_result : hex_decode_result structure typ = structure "HexDecodeResult"
let hdr_status = field hex_decode_result "status" int32_t
let hdr_data   = field hex_decode_result "data"   (ptr uint8_t)
let hdr_length = field hex_decode_result "length" size_t
let () = seal hex_decode_result

(* -------------------------------------------------------------------------- *)
(* Struct: RetryConfig                                                        *)
(* -------------------------------------------------------------------------- *)

type retry_config
let retry_config : retry_config structure typ = structure "RetryConfig"
let rc_max_attempts  = field retry_config "max_attempts"  uint32_t
let rc_base_delay_ms = field retry_config "base_delay_ms" uint64_t
let rc_max_delay_ms  = field retry_config "max_delay_ms"  uint64_t
let rc_multiplier    = field retry_config "multiplier"    double
let () = seal retry_config

(* -------------------------------------------------------------------------- *)
(* Opaque pointer types for stateful objects                                  *)
(* -------------------------------------------------------------------------- *)

(** Opaque handle for BoundedBuffer. *)
type bounded_buffer
let bounded_buffer : bounded_buffer structure typ = structure "BoundedBuffer"
let () = seal bounded_buffer

(** Opaque handle for RateLimiter. *)
type rate_limiter
let rate_limiter : rate_limiter structure typ = structure "RateLimiter"
let () = seal rate_limiter

(** Opaque handle for CircuitBreaker. *)
type circuit_breaker
let circuit_breaker : circuit_breaker structure typ = structure "CircuitBreaker"
let () = seal circuit_breaker

(** Opaque handle for MonotonicCounter. *)
type monotonic_counter
let monotonic_counter : monotonic_counter structure typ = structure "MonotonicCounter"
let () = seal monotonic_counter

(** Opaque handle for StateMachine. *)
type state_machine
let state_machine : state_machine structure typ = structure "StateMachine"
let () = seal state_machine

(** Opaque handle for Tensor2D. *)
type tensor_2d
let tensor_2d : tensor_2d structure typ = structure "Tensor2D"
let () = seal tensor_2d

(** Opaque handle for LRUCache. *)
type lru_cache
let lru_cache : lru_cache structure typ = structure "LRUCache"
let () = seal lru_cache

(** Opaque handle for Graph. *)
type graph
let graph : graph structure typ = structure "Graph"
let () = seal graph

(** Opaque handle for BoundedQueue. *)
type bounded_queue
let bounded_queue : bounded_queue structure typ = structure "BoundedQueue"
let () = seal bounded_queue

(** Opaque handle for BloomFilter. *)
type bloom_filter
let bloom_filter : bloom_filter structure typ = structure "BloomFilter"
let () = seal bloom_filter

(* -------------------------------------------------------------------------- *)
(* Struct: BufferResult { status: i32, buffer: *BoundedBuffer }               *)
(* -------------------------------------------------------------------------- *)

type buffer_result
let buffer_result : buffer_result structure typ = structure "BufferResult"
let bufr_status = field buffer_result "status" int32_t
let bufr_buffer = field buffer_result "buffer" (ptr bounded_buffer)
let () = seal buffer_result

(* ========================================================================== *)
(* Function bindings                                                          *)
(* ========================================================================== *)

(* -- Lifecycle ------------------------------------------------------------- *)

let proven_init =
  foreign ~from:lib "proven_init"
    (void @-> returning int32_t)

let proven_deinit =
  foreign ~from:lib "proven_deinit"
    (void @-> returning void)

let proven_is_initialized =
  foreign ~from:lib "proven_is_initialized"
    (void @-> returning bool)

(* -- Memory management ----------------------------------------------------- *)

let proven_free_string =
  foreign ~from:lib "proven_free_string"
    (ptr char @-> returning void)

(* -- Version info ---------------------------------------------------------- *)

let proven_version_major =
  foreign ~from:lib "proven_version_major"
    (void @-> returning uint32_t)

let proven_version_minor =
  foreign ~from:lib "proven_version_minor"
    (void @-> returning uint32_t)

let proven_version_patch =
  foreign ~from:lib "proven_version_patch"
    (void @-> returning uint32_t)

let proven_ffi_abi_version =
  foreign ~from:lib "proven_ffi_abi_version"
    (void @-> returning uint32_t)

let proven_module_count =
  foreign ~from:lib "proven_module_count"
    (void @-> returning uint32_t)

(* -- SafeMath -------------------------------------------------------------- *)

let proven_math_div =
  foreign ~from:lib "proven_math_div"
    (int64_t @-> int64_t @-> returning int_result)

let proven_math_mod =
  foreign ~from:lib "proven_math_mod"
    (int64_t @-> int64_t @-> returning int_result)

let proven_math_add_checked =
  foreign ~from:lib "proven_math_add_checked"
    (int64_t @-> int64_t @-> returning int_result)

let proven_math_sub_checked =
  foreign ~from:lib "proven_math_sub_checked"
    (int64_t @-> int64_t @-> returning int_result)

let proven_math_mul_checked =
  foreign ~from:lib "proven_math_mul_checked"
    (int64_t @-> int64_t @-> returning int_result)

let proven_math_abs_safe =
  foreign ~from:lib "proven_math_abs_safe"
    (int64_t @-> returning int_result)

let proven_math_clamp =
  foreign ~from:lib "proven_math_clamp"
    (int64_t @-> int64_t @-> int64_t @-> returning int64_t)

let proven_math_pow_checked =
  foreign ~from:lib "proven_math_pow_checked"
    (int64_t @-> uint32_t @-> returning int_result)

(* -- SafeString ------------------------------------------------------------ *)

let proven_string_is_valid_utf8 =
  foreign ~from:lib "proven_string_is_valid_utf8"
    (ptr char @-> size_t @-> returning bool_result)

let proven_string_escape_sql =
  foreign ~from:lib "proven_string_escape_sql"
    (ptr char @-> size_t @-> returning string_result)

let proven_string_escape_html =
  foreign ~from:lib "proven_string_escape_html"
    (ptr char @-> size_t @-> returning string_result)

let proven_string_escape_js =
  foreign ~from:lib "proven_string_escape_js"
    (ptr char @-> size_t @-> returning string_result)

(* -- SafePath -------------------------------------------------------------- *)

let proven_path_has_traversal =
  foreign ~from:lib "proven_path_has_traversal"
    (ptr char @-> size_t @-> returning bool_result)

let proven_path_sanitize_filename =
  foreign ~from:lib "proven_path_sanitize_filename"
    (ptr char @-> size_t @-> returning string_result)

(* -- SafeCrypto ------------------------------------------------------------ *)

let proven_crypto_constant_time_eq =
  foreign ~from:lib "proven_crypto_constant_time_eq"
    (ptr char @-> size_t @-> ptr char @-> size_t @-> returning bool_result)

let proven_crypto_random_bytes =
  foreign ~from:lib "proven_crypto_random_bytes"
    (ptr char @-> size_t @-> returning int32_t)

(* -- SafeUrl --------------------------------------------------------------- *)

(* Note: URL parsing uses a complex struct; we wrap only the simpler
   helpers at this level.  Full URL parsing is available through the
   proven_url_parse / proven_url_free pair if needed. *)

(* -- SafeEmail ------------------------------------------------------------- *)

let proven_email_is_valid =
  foreign ~from:lib "proven_email_is_valid"
    (ptr char @-> size_t @-> returning bool_result)

(* -- SafeNetwork ----------------------------------------------------------- *)

let proven_network_parse_ipv4 =
  foreign ~from:lib "proven_network_parse_ipv4"
    (ptr char @-> size_t @-> returning ipv4_result)

let proven_network_ipv4_is_private =
  foreign ~from:lib "proven_network_ipv4_is_private"
    (ipv4_address @-> returning bool)

let proven_network_ipv4_is_loopback =
  foreign ~from:lib "proven_network_ipv4_is_loopback"
    (ipv4_address @-> returning bool)

(* -- SafeHeader ------------------------------------------------------------ *)

let proven_header_has_crlf =
  foreign ~from:lib "proven_header_has_crlf"
    (ptr char @-> size_t @-> returning bool_result)

let proven_header_is_valid_name =
  foreign ~from:lib "proven_header_is_valid_name"
    (ptr char @-> size_t @-> returning bool_result)

let proven_header_is_dangerous =
  foreign ~from:lib "proven_header_is_dangerous"
    (ptr char @-> size_t @-> returning bool_result)

let proven_header_render =
  foreign ~from:lib "proven_header_render"
    (ptr char @-> size_t @-> ptr char @-> size_t @-> returning string_result)

let proven_header_build_csp =
  foreign ~from:lib "proven_header_build_csp"
    (ptr char @-> size_t @-> returning string_result)

let proven_header_build_hsts =
  foreign ~from:lib "proven_header_build_hsts"
    (int64_t @-> bool @-> bool @-> returning string_result)

(* -- SafeCookie ------------------------------------------------------------ *)

let proven_cookie_has_injection =
  foreign ~from:lib "proven_cookie_has_injection"
    (ptr char @-> size_t @-> returning bool_result)

let proven_cookie_validate_name =
  foreign ~from:lib "proven_cookie_validate_name"
    (ptr char @-> size_t @-> returning bool_result)

let proven_cookie_validate_value =
  foreign ~from:lib "proven_cookie_validate_value"
    (ptr char @-> size_t @-> returning bool_result)

let proven_cookie_get_prefix =
  foreign ~from:lib "proven_cookie_get_prefix"
    (ptr char @-> size_t @-> returning int_result)

let proven_cookie_build_delete =
  foreign ~from:lib "proven_cookie_build_delete"
    (ptr char @-> size_t @-> returning string_result)

(* -- SafeContentType ------------------------------------------------------- *)

let proven_content_type_can_sniff_dangerous =
  foreign ~from:lib "proven_content_type_can_sniff_dangerous"
    (ptr char @-> size_t @-> returning bool_result)

let proven_content_type_is_json =
  foreign ~from:lib "proven_content_type_is_json"
    (ptr char @-> size_t @-> ptr char @-> size_t @-> returning bool_result)

let proven_content_type_is_xml =
  foreign ~from:lib "proven_content_type_is_xml"
    (ptr char @-> size_t @-> ptr char @-> size_t @-> returning bool_result)

(* -- SafeUUID -------------------------------------------------------------- *)

let proven_uuid_v4 =
  foreign ~from:lib "proven_uuid_v4"
    (void @-> returning uuid_result)

let proven_uuid_to_string =
  foreign ~from:lib "proven_uuid_to_string"
    (uuid @-> returning string_result)

let proven_uuid_parse =
  foreign ~from:lib "proven_uuid_parse"
    (ptr char @-> size_t @-> returning uuid_result)

let proven_uuid_is_nil =
  foreign ~from:lib "proven_uuid_is_nil"
    (uuid @-> returning bool)

let proven_uuid_version =
  foreign ~from:lib "proven_uuid_version"
    (uuid @-> returning uint8_t)

(* -- SafeJson -------------------------------------------------------------- *)

let proven_json_is_valid =
  foreign ~from:lib "proven_json_is_valid"
    (ptr char @-> size_t @-> returning bool_result)

let proven_json_get_type =
  foreign ~from:lib "proven_json_get_type"
    (ptr char @-> size_t @-> returning int32_t)

(* -- SafeDateTime ---------------------------------------------------------- *)

let proven_datetime_parse =
  foreign ~from:lib "proven_datetime_parse"
    (ptr char @-> size_t @-> returning datetime_result)

let proven_datetime_format_iso8601 =
  foreign ~from:lib "proven_datetime_format_iso8601"
    (datetime @-> returning string_result)

let proven_datetime_is_leap_year =
  foreign ~from:lib "proven_datetime_is_leap_year"
    (int32_t @-> returning bool)

let proven_datetime_days_in_month =
  foreign ~from:lib "proven_datetime_days_in_month"
    (int32_t @-> uint8_t @-> returning uint8_t)

(* -- SafeFloat ------------------------------------------------------------- *)

let proven_float_div =
  foreign ~from:lib "proven_float_div"
    (double @-> double @-> returning float_result)

let proven_float_is_finite =
  foreign ~from:lib "proven_float_is_finite"
    (double @-> returning bool)

let proven_float_is_nan =
  foreign ~from:lib "proven_float_is_nan"
    (double @-> returning bool)

let proven_float_sqrt =
  foreign ~from:lib "proven_float_sqrt"
    (double @-> returning float_result)

let proven_float_ln =
  foreign ~from:lib "proven_float_ln"
    (double @-> returning float_result)

(* -- SafePassword ---------------------------------------------------------- *)

let proven_password_validate =
  foreign ~from:lib "proven_password_validate"
    (ptr char @-> size_t @-> returning password_result)

let proven_password_is_common =
  foreign ~from:lib "proven_password_is_common"
    (ptr char @-> size_t @-> returning bool)

(* -- SafeVersion ----------------------------------------------------------- *)

let proven_version_parse_semver =
  foreign ~from:lib "proven_version_parse"
    (ptr char @-> size_t @-> returning version_result)

let proven_version_compare =
  foreign ~from:lib "proven_version_compare"
    (semantic_version @-> semantic_version @-> returning int32_t)

let proven_version_free =
  foreign ~from:lib "proven_version_free"
    (ptr semantic_version @-> returning void)

(* -- SafeGeo --------------------------------------------------------------- *)

let proven_geo_validate =
  foreign ~from:lib "proven_geo_validate"
    (double @-> double @-> returning geo_result)

let proven_geo_distance =
  foreign ~from:lib "proven_geo_distance"
    (geo_coordinate @-> geo_coordinate @-> returning float_result)

let proven_geo_in_bounds =
  foreign ~from:lib "proven_geo_in_bounds"
    (geo_coordinate @-> double @-> double @-> double @-> double @-> returning bool)

(* -- SafeChecksum ---------------------------------------------------------- *)

let proven_checksum_crc32 =
  foreign ~from:lib "proven_checksum_crc32"
    (ptr char @-> size_t @-> returning int_result)

let proven_checksum_verify_crc32 =
  foreign ~from:lib "proven_checksum_verify_crc32"
    (ptr char @-> size_t @-> uint32_t @-> returning bool_result)

(* -- SafeProbability ------------------------------------------------------- *)

let proven_probability_create =
  foreign ~from:lib "proven_probability_create"
    (double @-> returning double)

let proven_probability_and =
  foreign ~from:lib "proven_probability_and"
    (double @-> double @-> returning double)

let proven_probability_or_exclusive =
  foreign ~from:lib "proven_probability_or_exclusive"
    (double @-> double @-> returning double)

let proven_probability_not =
  foreign ~from:lib "proven_probability_not"
    (double @-> returning double)

(* -- SafeCalculator -------------------------------------------------------- *)

let proven_calculator_eval =
  foreign ~from:lib "proven_calculator_eval"
    (ptr char @-> size_t @-> returning float_result)

(* -- SafeHex --------------------------------------------------------------- *)

let proven_hex_encode =
  foreign ~from:lib "proven_hex_encode"
    (ptr char @-> size_t @-> bool @-> returning string_result)

let proven_hex_decode =
  foreign ~from:lib "proven_hex_decode"
    (ptr char @-> size_t @-> returning hex_decode_result)

let proven_hex_free =
  foreign ~from:lib "proven_hex_free"
    (ptr hex_decode_result @-> returning void)

(* -- SafeCurrency ---------------------------------------------------------- *)

let proven_currency_parse =
  foreign ~from:lib "proven_currency_parse"
    (ptr char @-> size_t @-> returning currency_result)

let proven_currency_format =
  foreign ~from:lib "proven_currency_format"
    (int64_t @-> array 3 uint8_t @-> uint8_t @-> returning string_result)

(* -- SafePhone ------------------------------------------------------------- *)

let proven_phone_parse =
  foreign ~from:lib "proven_phone_parse"
    (ptr char @-> size_t @-> returning phone_result)

let proven_phone_format_e164 =
  foreign ~from:lib "proven_phone_format_e164"
    (uint16_t @-> uint64_t @-> returning string_result)

(* -- SafeColor ------------------------------------------------------------- *)

let proven_color_parse_hex =
  foreign ~from:lib "proven_color_parse_hex"
    (ptr char @-> size_t @-> returning color_parse_result)

let proven_color_rgb_to_hsl =
  foreign ~from:lib "proven_color_rgb_to_hsl"
    (rgb_color @-> returning hsl_color)

let proven_color_to_hex =
  foreign ~from:lib "proven_color_to_hex"
    (rgb_color @-> returning string_result)

(* -- SafeAngle ------------------------------------------------------------- *)

let proven_angle_deg_to_rad =
  foreign ~from:lib "proven_angle_deg_to_rad"
    (double @-> returning double)

let proven_angle_rad_to_deg =
  foreign ~from:lib "proven_angle_rad_to_deg"
    (double @-> returning double)

let proven_angle_normalize_degrees =
  foreign ~from:lib "proven_angle_normalize_degrees"
    (double @-> returning double)

let proven_angle_normalize_radians =
  foreign ~from:lib "proven_angle_normalize_radians"
    (double @-> returning double)

(* -- SafeUnit -------------------------------------------------------------- *)

let proven_unit_convert_length =
  foreign ~from:lib "proven_unit_convert_length"
    (double @-> int32_t @-> int32_t @-> returning float_result)

let proven_unit_convert_temp =
  foreign ~from:lib "proven_unit_convert_temp"
    (double @-> int32_t @-> int32_t @-> returning float_result)

(* -- SafeHttp -------------------------------------------------------------- *)

let proven_http_url_encode =
  foreign ~from:lib "proven_http_url_encode"
    (ptr char @-> size_t @-> returning string_result)

let proven_http_url_decode =
  foreign ~from:lib "proven_http_url_decode"
    (ptr char @-> size_t @-> returning string_result)

(* -- SafeBuffer ------------------------------------------------------------ *)

let proven_buffer_create =
  foreign ~from:lib "proven_buffer_create"
    (size_t @-> returning buffer_result)

let proven_buffer_append =
  foreign ~from:lib "proven_buffer_append"
    (ptr bounded_buffer @-> ptr char @-> size_t @-> returning int32_t)

let proven_buffer_get =
  foreign ~from:lib "proven_buffer_get"
    (ptr bounded_buffer @-> ptr (ptr char) @-> ptr size_t @-> returning int32_t)

let proven_buffer_free =
  foreign ~from:lib "proven_buffer_free"
    (ptr bounded_buffer @-> returning void)

(* -- SafeRateLimiter ------------------------------------------------------- *)

let proven_rate_limiter_create =
  foreign ~from:lib "proven_rate_limiter_create"
    (double @-> double @-> returning (ptr_opt rate_limiter))

let proven_rate_limiter_try_acquire =
  foreign ~from:lib "proven_rate_limiter_try_acquire"
    (ptr rate_limiter @-> double @-> returning bool)

let proven_rate_limiter_free =
  foreign ~from:lib "proven_rate_limiter_free"
    (ptr rate_limiter @-> returning void)

(* -- SafeCircuitBreaker ---------------------------------------------------- *)

let proven_circuit_breaker_create =
  foreign ~from:lib "proven_circuit_breaker_create"
    (uint32_t @-> uint32_t @-> int64_t @-> returning (ptr_opt circuit_breaker))

let proven_circuit_breaker_allow =
  foreign ~from:lib "proven_circuit_breaker_allow"
    (ptr circuit_breaker @-> returning bool)

let proven_circuit_breaker_success =
  foreign ~from:lib "proven_circuit_breaker_success"
    (ptr circuit_breaker @-> returning void)

let proven_circuit_breaker_failure =
  foreign ~from:lib "proven_circuit_breaker_failure"
    (ptr circuit_breaker @-> returning void)

let proven_circuit_breaker_state =
  foreign ~from:lib "proven_circuit_breaker_state"
    (ptr circuit_breaker @-> returning int32_t)

let proven_circuit_breaker_free =
  foreign ~from:lib "proven_circuit_breaker_free"
    (ptr circuit_breaker @-> returning void)

(* -- SafeRetry ------------------------------------------------------------- *)

let proven_retry_delay =
  foreign ~from:lib "proven_retry_delay"
    (retry_config @-> uint32_t @-> returning uint64_t)

let proven_retry_should_retry =
  foreign ~from:lib "proven_retry_should_retry"
    (retry_config @-> uint32_t @-> returning bool)

(* -- SafeMonotonic --------------------------------------------------------- *)

let proven_monotonic_create =
  foreign ~from:lib "proven_monotonic_create"
    (uint64_t @-> uint64_t @-> returning (ptr_opt monotonic_counter))

let proven_monotonic_next =
  foreign ~from:lib "proven_monotonic_next"
    (ptr monotonic_counter @-> returning int_result)

let proven_monotonic_free =
  foreign ~from:lib "proven_monotonic_free"
    (ptr monotonic_counter @-> returning void)

(* -- SafeStateMachine ------------------------------------------------------ *)

let proven_state_machine_create =
  foreign ~from:lib "proven_state_machine_create"
    (uint32_t @-> uint32_t @-> returning (ptr_opt state_machine))

let proven_state_machine_allow =
  foreign ~from:lib "proven_state_machine_allow"
    (ptr state_machine @-> uint32_t @-> uint32_t @-> returning bool)

let proven_state_machine_transition =
  foreign ~from:lib "proven_state_machine_transition"
    (ptr state_machine @-> uint32_t @-> returning bool)

let proven_state_machine_state =
  foreign ~from:lib "proven_state_machine_state"
    (ptr state_machine @-> returning uint32_t)

let proven_state_machine_free =
  foreign ~from:lib "proven_state_machine_free"
    (ptr state_machine @-> returning void)

(* -- SafeTensor ------------------------------------------------------------ *)

let proven_tensor_create =
  foreign ~from:lib "proven_tensor_create"
    (size_t @-> size_t @-> returning (ptr_opt tensor_2d))

let proven_tensor_set =
  foreign ~from:lib "proven_tensor_set"
    (ptr tensor_2d @-> size_t @-> size_t @-> double @-> returning int32_t)

let proven_tensor_get =
  foreign ~from:lib "proven_tensor_get"
    (ptr tensor_2d @-> size_t @-> size_t @-> returning float_result)

let proven_tensor_matmul =
  foreign ~from:lib "proven_tensor_matmul"
    (ptr tensor_2d @-> ptr tensor_2d @-> returning (ptr_opt tensor_2d))

let proven_tensor_free =
  foreign ~from:lib "proven_tensor_free"
    (ptr tensor_2d @-> returning void)

(* -- SafeML ---------------------------------------------------------------- *)

let proven_ml_sigmoid =
  foreign ~from:lib "proven_ml_sigmoid"
    (double @-> returning double)

let proven_ml_relu =
  foreign ~from:lib "proven_ml_relu"
    (double @-> returning double)

let proven_ml_leaky_relu =
  foreign ~from:lib "proven_ml_leaky_relu"
    (double @-> double @-> returning double)

let proven_ml_clamp =
  foreign ~from:lib "proven_ml_clamp"
    (double @-> double @-> double @-> returning double)

(* -- SafeLRU --------------------------------------------------------------- *)

let proven_lru_create =
  foreign ~from:lib "proven_lru_create"
    (size_t @-> returning (ptr_opt lru_cache))

let proven_lru_get =
  foreign ~from:lib "proven_lru_get"
    (ptr lru_cache @-> uint64_t @-> returning int_result)

let proven_lru_put =
  foreign ~from:lib "proven_lru_put"
    (ptr lru_cache @-> uint64_t @-> int64_t @-> returning int32_t)

let proven_lru_free =
  foreign ~from:lib "proven_lru_free"
    (ptr lru_cache @-> returning void)

(* -- SafeGraph ------------------------------------------------------------- *)

let proven_graph_create =
  foreign ~from:lib "proven_graph_create"
    (size_t @-> returning (ptr_opt graph))

let proven_graph_add_edge =
  foreign ~from:lib "proven_graph_add_edge"
    (ptr graph @-> size_t @-> size_t @-> returning int32_t)

let proven_graph_has_edge =
  foreign ~from:lib "proven_graph_has_edge"
    (ptr graph @-> size_t @-> size_t @-> returning bool)

let proven_graph_free =
  foreign ~from:lib "proven_graph_free"
    (ptr graph @-> returning void)

(* -- SafeQueue ------------------------------------------------------------- *)

let proven_queue_create =
  foreign ~from:lib "proven_queue_create"
    (size_t @-> returning (ptr_opt bounded_queue))

let proven_queue_push =
  foreign ~from:lib "proven_queue_push"
    (ptr bounded_queue @-> int64_t @-> returning bool)

let proven_queue_pop =
  foreign ~from:lib "proven_queue_pop"
    (ptr bounded_queue @-> returning int_result)

let proven_queue_size =
  foreign ~from:lib "proven_queue_size"
    (ptr bounded_queue @-> returning size_t)

let proven_queue_free =
  foreign ~from:lib "proven_queue_free"
    (ptr bounded_queue @-> returning void)

(* -- SafeBloom ------------------------------------------------------------- *)

let proven_bloom_create =
  foreign ~from:lib "proven_bloom_create"
    (size_t @-> double @-> returning (ptr_opt bloom_filter))

let proven_bloom_add =
  foreign ~from:lib "proven_bloom_add"
    (ptr bloom_filter @-> ptr char @-> size_t @-> returning void)

let proven_bloom_contains =
  foreign ~from:lib "proven_bloom_contains"
    (ptr bloom_filter @-> ptr char @-> size_t @-> returning bool)

let proven_bloom_free =
  foreign ~from:lib "proven_bloom_free"
    (ptr bloom_filter @-> returning void)

(* -- Callbacks ------------------------------------------------------------- *)

let proven_callback_register =
  foreign ~from:lib "proven_callback_register"
    (int32_t @-> ptr void @-> ptr_opt void @-> returning uint32_t)

let proven_callback_unregister =
  foreign ~from:lib "proven_callback_unregister"
    (uint32_t @-> returning int32_t)

let proven_callback_fire =
  foreign ~from:lib "proven_callback_fire"
    (int32_t @-> ptr_opt char @-> size_t @-> int32_t @-> returning int32_t)

let proven_callback_count =
  foreign ~from:lib "proven_callback_count"
    (int32_t @-> returning uint32_t)

let proven_callback_clear_all =
  foreign ~from:lib "proven_callback_clear_all"
    (void @-> returning uint32_t)

(* -- SafeRegistry ---------------------------------------------------------- *)

(* Note: Registry and Digest use complex structs with internal pointers.
   We provide the simpler interfaces here; full parsing can be added
   by extending the struct definitions above. *)

(* -- SafeDigest ------------------------------------------------------------ *)

(* Digest structs with internal pointers require careful lifetime management.
   Basic operations are exposed via the string-based API through SafeHex. *)
