# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Raw C FFI declarations for libproven.
# These map 1:1 to the C header `proven.h` and per-module headers.
# Higher-level Crystal wrappers live in the sibling safe_*.cr files.

@[Link("proven")]
lib LibProven
  # ---------------------------------------------------------------------------
  # Status codes (ProvenStatus enum)
  # ---------------------------------------------------------------------------
  enum ProvenStatus : Int32
    Ok                = 0
    ErrNullPointer    = -1
    ErrInvalidArgument = -2
    ErrOverflow       = -3
    ErrUnderflow      = -4
    ErrDivisionByZero = -5
    ErrParseFailure   = -6
    ErrValidationFailed = -7
    ErrOutOfBounds    = -8
    ErrEncodingError  = -9
    ErrAllocationFailed = -10
    ErrNotImplemented = -99
  end

  # ---------------------------------------------------------------------------
  # Core result structs
  # ---------------------------------------------------------------------------
  struct IntResult
    status : ProvenStatus
    value : Int64
  end

  struct BoolResult
    status : ProvenStatus
    value : Bool
  end

  struct StringResult
    status : ProvenStatus
    value : Pointer(UInt8)
    length : LibC::SizeT
  end

  struct FloatResult
    status : ProvenStatus
    value : Float64
  end

  # ---------------------------------------------------------------------------
  # Network types
  # ---------------------------------------------------------------------------
  struct IPv4Address
    octets : StaticArray(UInt8, 4)
  end

  struct IPv4Result
    status : ProvenStatus
    address : IPv4Address
  end

  # ---------------------------------------------------------------------------
  # URL types
  # ---------------------------------------------------------------------------
  struct UrlComponents
    scheme : Pointer(UInt8)
    scheme_len : LibC::SizeT
    host : Pointer(UInt8)
    host_len : LibC::SizeT
    port : UInt16
    has_port : Bool
    path : Pointer(UInt8)
    path_len : LibC::SizeT
    query : Pointer(UInt8)
    query_len : LibC::SizeT
    fragment : Pointer(UInt8)
    fragment_len : LibC::SizeT
  end

  struct UrlResult
    status : ProvenStatus
    components : UrlComponents
  end

  # ---------------------------------------------------------------------------
  # DateTime types
  # ---------------------------------------------------------------------------
  struct DateTime
    year : Int32
    month : UInt8
    day : UInt8
    hour : UInt8
    minute : UInt8
    second : UInt8
    nanosecond : UInt32
    tz_offset_minutes : Int16
  end

  struct DateTimeResult
    status : ProvenStatus
    datetime : DateTime
  end

  # ---------------------------------------------------------------------------
  # Version types
  # ---------------------------------------------------------------------------
  struct SemanticVersion
    major : UInt32
    minor : UInt32
    patch : UInt32
    prerelease_len : LibC::SizeT
    prerelease : Pointer(UInt8)
  end

  struct VersionResult
    status : ProvenStatus
    version : SemanticVersion
  end

  # ---------------------------------------------------------------------------
  # Color types
  # ---------------------------------------------------------------------------
  struct RGBColor
    r : UInt8
    g : UInt8
    b : UInt8
  end

  struct HSLColor
    h : Float64
    s : Float64
    l : Float64
  end

  struct ColorResult
    status : ProvenStatus
    color : RGBColor
  end

  # ---------------------------------------------------------------------------
  # JSON type enum
  # ---------------------------------------------------------------------------
  enum JsonType : Int32
    Null    = 0
    Bool    = 1
    Number  = 2
    String  = 3
    Array   = 4
    Object  = 5
    Invalid = -1
  end

  # ---------------------------------------------------------------------------
  # Unit enums
  # ---------------------------------------------------------------------------
  enum LengthUnit : Int32
    Meters      = 0
    Kilometers  = 1
    Centimeters = 2
    Millimeters = 3
    Feet        = 4
    Inches      = 5
    Miles       = 6
    Yards       = 7
  end

  enum TempUnit : Int32
    Celsius    = 0
    Fahrenheit = 1
    Kelvin     = 2
  end

  # ---------------------------------------------------------------------------
  # Runtime management
  # ---------------------------------------------------------------------------
  fun proven_init : Int32
  fun proven_deinit : Void
  fun proven_is_initialized : Bool
  fun proven_ffi_abi_version : UInt32

  # ---------------------------------------------------------------------------
  # Memory management
  # ---------------------------------------------------------------------------
  fun proven_free_string(ptr : Pointer(UInt8)) : Void
  fun proven_url_free(components : Pointer(UrlComponents)) : Void
  fun proven_version_free(version : Pointer(SemanticVersion)) : Void

  # ---------------------------------------------------------------------------
  # Version info
  # ---------------------------------------------------------------------------
  fun proven_version_major : UInt32
  fun proven_version_minor : UInt32
  fun proven_version_patch : UInt32

  # ---------------------------------------------------------------------------
  # SafeMath (8 functions)
  # ---------------------------------------------------------------------------
  fun proven_math_div(numerator : Int64, denominator : Int64) : IntResult
  fun proven_math_mod(numerator : Int64, denominator : Int64) : IntResult
  fun proven_math_add_checked(a : Int64, b : Int64) : IntResult
  fun proven_math_sub_checked(a : Int64, b : Int64) : IntResult
  fun proven_math_mul_checked(a : Int64, b : Int64) : IntResult
  fun proven_math_abs_safe(n : Int64) : IntResult
  fun proven_math_clamp(lo : Int64, hi : Int64, value : Int64) : Int64
  fun proven_math_pow_checked(base : Int64, exp : UInt32) : IntResult

  # ---------------------------------------------------------------------------
  # SafeString (4 functions)
  # ---------------------------------------------------------------------------
  fun proven_string_is_valid_utf8(ptr : Pointer(UInt8), len : LibC::SizeT) : BoolResult
  fun proven_string_escape_sql(ptr : Pointer(UInt8), len : LibC::SizeT) : StringResult
  fun proven_string_escape_html(ptr : Pointer(UInt8), len : LibC::SizeT) : StringResult
  fun proven_string_escape_js(ptr : Pointer(UInt8), len : LibC::SizeT) : StringResult

  # ---------------------------------------------------------------------------
  # SafePath (2 functions)
  # ---------------------------------------------------------------------------
  fun proven_path_has_traversal(ptr : Pointer(UInt8), len : LibC::SizeT) : BoolResult
  fun proven_path_sanitize_filename(ptr : Pointer(UInt8), len : LibC::SizeT) : StringResult

  # ---------------------------------------------------------------------------
  # SafeEmail (1 function)
  # ---------------------------------------------------------------------------
  fun proven_email_is_valid(ptr : Pointer(UInt8), len : LibC::SizeT) : BoolResult

  # ---------------------------------------------------------------------------
  # SafeNetwork (3 functions)
  # ---------------------------------------------------------------------------
  fun proven_network_parse_ipv4(ptr : Pointer(UInt8), len : LibC::SizeT) : IPv4Result
  fun proven_network_ipv4_is_private(addr : IPv4Address) : Bool
  fun proven_network_ipv4_is_loopback(addr : IPv4Address) : Bool

  # ---------------------------------------------------------------------------
  # SafeCrypto (2 functions)
  # ---------------------------------------------------------------------------
  fun proven_crypto_constant_time_eq(
    ptr1 : Pointer(UInt8), len1 : LibC::SizeT,
    ptr2 : Pointer(UInt8), len2 : LibC::SizeT
  ) : BoolResult
  fun proven_crypto_random_bytes(ptr : Pointer(UInt8), len : LibC::SizeT) : ProvenStatus

  # ---------------------------------------------------------------------------
  # SafeUrl (2 functions)
  # ---------------------------------------------------------------------------
  fun proven_url_parse(ptr : Pointer(UInt8), len : LibC::SizeT) : UrlResult

  # ---------------------------------------------------------------------------
  # SafeJson (2 functions)
  # ---------------------------------------------------------------------------
  fun proven_json_is_valid(ptr : Pointer(UInt8), len : LibC::SizeT) : BoolResult
  fun proven_json_get_type(ptr : Pointer(UInt8), len : LibC::SizeT) : JsonType

  # ---------------------------------------------------------------------------
  # SafeDateTime (4 functions)
  # ---------------------------------------------------------------------------
  fun proven_datetime_parse(ptr : Pointer(UInt8), len : LibC::SizeT) : DateTimeResult
  fun proven_datetime_format_iso8601(dt : DateTime) : StringResult
  fun proven_datetime_is_leap_year(year : Int32) : Bool
  fun proven_datetime_days_in_month(year : Int32, month : UInt8) : UInt8

  # ---------------------------------------------------------------------------
  # SafeFloat (5 functions)
  # ---------------------------------------------------------------------------
  fun proven_float_div(a : Float64, b : Float64) : FloatResult
  fun proven_float_sqrt(x : Float64) : FloatResult
  fun proven_float_ln(x : Float64) : FloatResult
  fun proven_float_is_finite(x : Float64) : Bool
  fun proven_float_is_nan(x : Float64) : Bool

  # ---------------------------------------------------------------------------
  # SafeVersion (3 functions)
  # ---------------------------------------------------------------------------
  fun proven_version_parse(ptr : Pointer(UInt8), len : LibC::SizeT) : VersionResult
  fun proven_version_compare(a : SemanticVersion, b : SemanticVersion) : Int32

  # ---------------------------------------------------------------------------
  # SafeColor (3 functions)
  # ---------------------------------------------------------------------------
  fun proven_color_parse_hex(ptr : Pointer(UInt8), len : LibC::SizeT) : ColorResult
  fun proven_color_rgb_to_hsl(rgb : RGBColor) : HSLColor
  fun proven_color_to_hex(rgb : RGBColor) : StringResult

  # ---------------------------------------------------------------------------
  # SafeAngle (4 functions)
  # ---------------------------------------------------------------------------
  fun proven_angle_deg_to_rad(degrees : Float64) : Float64
  fun proven_angle_rad_to_deg(radians : Float64) : Float64
  fun proven_angle_normalize_degrees(degrees : Float64) : Float64
  fun proven_angle_normalize_radians(radians : Float64) : Float64

  # ---------------------------------------------------------------------------
  # SafeUnit (2 functions)
  # ---------------------------------------------------------------------------
  fun proven_unit_convert_length(value : Float64, from : LengthUnit, to : LengthUnit) : FloatResult
  fun proven_unit_convert_temp(value : Float64, from : TempUnit, to : TempUnit) : FloatResult

  # ---------------------------------------------------------------------------
  # SafeUuid (UUID has its own status/type namespace)
  # ---------------------------------------------------------------------------
  struct Uuid
    bytes : StaticArray(UInt8, 16)
  end

  fun uuid_v4_generate(uuid : Pointer(Uuid)) : Int32
  fun uuid_parse(str : Pointer(UInt8), len : LibC::SizeT, uuid : Pointer(Uuid)) : Int32
  fun uuid_parse_cstr(str : Pointer(UInt8), uuid : Pointer(Uuid)) : Int32
  fun uuid_to_string(uuid : Pointer(Uuid), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun uuid_to_urn(uuid : Pointer(Uuid), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun uuid_is_nil(uuid : Pointer(Uuid)) : Bool
  fun uuid_compare(a : Pointer(Uuid), b : Pointer(Uuid)) : Int32
  fun uuid_equals(a : Pointer(Uuid), b : Pointer(Uuid)) : Bool
  fun uuid_is_valid(str : Pointer(UInt8), len : LibC::SizeT) : Bool

  # ---------------------------------------------------------------------------
  # SafeCurrency (selected functions)
  # ---------------------------------------------------------------------------
  struct Money
    minor_units : Int64
    currency : Int32
  end

  struct MoneyResult
    status : Int32
    value : Money
  end

  struct CurrencyCodeResult
    status : Int32
    code : Int32
  end

  fun currency_parse_code(str : Pointer(UInt8), len : LibC::SizeT) : CurrencyCodeResult
  fun currency_is_valid_code(str : Pointer(UInt8), len : LibC::SizeT) : Bool
  fun currency_get_decimals(code : Int32) : UInt8
  fun currency_get_symbol(code : Int32) : Pointer(UInt8)
  fun currency_get_name(code : Int32) : Pointer(UInt8)
  fun currency_get_iso_code(code : Int32) : Pointer(UInt8)
  fun money_from_major(amount : Int64, currency : Int32) : Money
  fun money_from_minor(amount : Int64, currency : Int32) : Money
  fun money_zero(currency : Int32) : Money
  fun money_add(a : Money, b : Money) : MoneyResult
  fun money_sub(a : Money, b : Money) : MoneyResult
  fun money_mul(m : Money, scalar : Int64) : MoneyResult
  fun money_div(m : Money, divisor : Int64) : MoneyResult
  fun money_abs(m : Money) : MoneyResult
  fun money_negate(m : Money) : MoneyResult
  fun money_get_major(m : Money) : Int64
  fun money_get_minor(m : Money) : Int64
  fun money_get_fraction(m : Money) : Int64
  fun money_is_zero(m : Money) : Bool
  fun money_is_positive(m : Money) : Bool
  fun money_is_negative(m : Money) : Bool
  fun money_compare(a : Money, b : Money) : Int32
  fun money_same_currency(a : Money, b : Money) : Bool
  fun money_format(m : Money, buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun money_format_plain(m : Money, buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun money_format_iso(m : Money, buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32

  # ---------------------------------------------------------------------------
  # SafePhone (selected functions)
  # ---------------------------------------------------------------------------
  struct PhoneNumber
    country_code : Int32
    national_number : StaticArray(UInt8, 16)
    national_number_len : UInt8
  end

  struct PhoneResult
    status : Int32
    number : PhoneNumber
  end

  fun phone_parse(str : Pointer(UInt8), len : LibC::SizeT) : PhoneResult
  fun phone_parse_cstr(str : Pointer(UInt8)) : PhoneResult
  fun phone_is_valid(str : Pointer(UInt8), len : LibC::SizeT) : Bool
  fun phone_format_e164(number : Pointer(PhoneNumber), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun phone_format_international(number : Pointer(PhoneNumber), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun phone_format_national(number : Pointer(PhoneNumber), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun phone_format_rfc3966(number : Pointer(PhoneNumber), buf : Pointer(UInt8), buf_size : LibC::SizeT) : Int32
  fun phone_get_calling_code(code : Int32) : UInt16
  fun phone_get_country_name(code : Int32) : Pointer(UInt8)
  fun phone_get_iso_alpha2(code : Int32) : Pointer(UInt8)
  fun phone_digit_count(number : Pointer(PhoneNumber)) : LibC::SizeT
  fun phone_equals(a : Pointer(PhoneNumber), b : Pointer(PhoneNumber)) : Bool

  # ---------------------------------------------------------------------------
  # SafeHex (selected functions)
  # ---------------------------------------------------------------------------
  struct HexDecodeResult
    status : Int32
    bytes_written : LibC::SizeT
  end

  struct HexEncodeResult
    status : Int32
    chars_written : LibC::SizeT
  end

  struct HexIntResult
    status : Int32
    value : UInt64
  end

  fun hex_encode(bytes : Pointer(UInt8), bytes_len : LibC::SizeT, hex : Pointer(UInt8), hex_size : LibC::SizeT) : HexEncodeResult
  fun hex_encode_upper(bytes : Pointer(UInt8), bytes_len : LibC::SizeT, hex : Pointer(UInt8), hex_size : LibC::SizeT) : HexEncodeResult
  fun hex_decode(hex : Pointer(UInt8), hex_len : LibC::SizeT, bytes : Pointer(UInt8), bytes_size : LibC::SizeT) : HexDecodeResult
  fun hex_is_valid(str : Pointer(UInt8), len : LibC::SizeT) : Bool
  fun hex_is_valid_bytes(str : Pointer(UInt8), len : LibC::SizeT) : Bool
  fun hex_constant_time_eq(a : Pointer(UInt8), a_len : LibC::SizeT, b : Pointer(UInt8), b_len : LibC::SizeT) : Bool
  fun hex_to_int(hex : Pointer(UInt8), hex_len : LibC::SizeT) : HexIntResult
  fun hex_from_int(value : UInt64, min_width : LibC::SizeT, uppercase : Bool, hex : Pointer(UInt8), hex_size : LibC::SizeT) : HexEncodeResult
  fun hex_format_spaced(hex : Pointer(UInt8), hex_len : LibC::SizeT, out_buf : Pointer(UInt8), out_size : LibC::SizeT) : HexEncodeResult
  fun hex_format_colons(hex : Pointer(UInt8), hex_len : LibC::SizeT, out_buf : Pointer(UInt8), out_size : LibC::SizeT) : HexEncodeResult
  fun hex_encode_size(bytes_len : LibC::SizeT) : LibC::SizeT
  fun hex_decode_size(hex_len : LibC::SizeT) : LibC::SizeT

  # ---------------------------------------------------------------------------
  # SafeCalculator (1 function)
  # ---------------------------------------------------------------------------
  fun proven_calculator_eval(ptr : Pointer(UInt8), len : LibC::SizeT) : FloatResult
end
